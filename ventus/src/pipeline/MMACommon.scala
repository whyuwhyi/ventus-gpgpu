package pipeline

import chisel3._
import chisel3.util._

object MMAConst {
  val ShapeM8N8K16   = 0
  val ShapeM16N8K16  = 1
  val ShapeM8N16K16  = 2
  val ShapeM16N16K16 = 3
  val ShapeM8N8K8    = 4
  val ShapeM16N8K8   = 5
  val ShapeM8N16K8   = 6
  val ShapeM16N16K8  = 7

  val ABTypeTF32 = 0
  val ABTypeFP16 = 1
  val ABTypeBF16 = 2

  val CDTypeFP16 = 0
  val CDTypeFP32 = 1

  val MaxARegs  = 4
  val MaxBRegs  = 4
  val MaxCDRegs = 8
}

object MMAWindowInfo {
  import MMAConst._

  def isK8Shape(shape: UInt): Bool = shape(2)
  def isK8Shape(shape: Int): Boolean = (shape & 0x4) != 0

  def mDim(shape: UInt): UInt = Mux(shape(0), 16.U(5.W), 8.U(5.W))
  def nDim(shape: UInt): UInt = Mux(shape(1), 16.U(5.W), 8.U(5.W))
  def kDim(shape: UInt): UInt = Mux(isK8Shape(shape), 8.U(5.W), 16.U(5.W))

  def mDim(shape: Int): Int = if ((shape & 0x1) != 0) 16 else 8
  def nDim(shape: Int): Int = if ((shape & 0x2) != 0) 16 else 8
  def kDim(shape: Int): Int = if (isK8Shape(shape)) 8 else 16

  def isLegal(shape: UInt, abtype: UInt, cdtype: UInt): Bool = {
    val isTF32 = abtype === ABTypeTF32.U
    val isFP16 = abtype === ABTypeFP16.U
    val isBF16 = abtype === ABTypeBF16.U
    Mux(isTF32, cdtype === CDTypeFP32.U && isK8Shape(shape),
      Mux(isFP16, true.B,
        Mux(isBF16, cdtype === CDTypeFP32.U, false.B)))
  }

  def isLegal(shape: Int, abtype: Int, cdtype: Int): Boolean = abtype match {
    case ABTypeTF32 => cdtype == CDTypeFP32 && isK8Shape(shape)
    case ABTypeFP16 => cdtype == CDTypeFP16 || cdtype == CDTypeFP32
    case ABTypeBF16 => cdtype == CDTypeFP32
    case _ => false
  }

  def srcARegs(shape: UInt): UInt = MuxLookup(shape, MaxARegs.U(3.W))(Seq(
    ShapeM8N8K16.U   -> 2.U(3.W),
    ShapeM16N8K16.U  -> 4.U(3.W),
    ShapeM8N16K16.U  -> 2.U(3.W),
    ShapeM16N16K16.U -> 4.U(3.W),
    ShapeM8N8K8.U    -> 2.U(3.W),
    ShapeM16N8K8.U   -> 4.U(3.W),
    ShapeM8N16K8.U   -> 2.U(3.W),
    ShapeM16N16K8.U  -> 4.U(3.W)
  ))

  def srcBRegs(shape: UInt): UInt = MuxLookup(shape, MaxBRegs.U(3.W))(Seq(
    ShapeM8N8K16.U   -> 2.U(3.W),
    ShapeM16N8K16.U  -> 2.U(3.W),
    ShapeM8N16K16.U  -> 4.U(3.W),
    ShapeM16N16K16.U -> 4.U(3.W),
    ShapeM8N8K8.U    -> 2.U(3.W),
    ShapeM16N8K8.U   -> 2.U(3.W),
    ShapeM8N16K8.U   -> 4.U(3.W),
    ShapeM16N16K8.U  -> 4.U(3.W)
  ))

  def srcCDRegs(shape: UInt, cdtype: UInt): UInt = {
    val fp16Regs = MuxLookup(shape, MaxCDRegs.U(4.W))(Seq(
      ShapeM8N8K16.U   -> 1.U(4.W),
      ShapeM16N8K16.U  -> 2.U(4.W),
      ShapeM8N16K16.U  -> 2.U(4.W),
      ShapeM16N16K16.U -> 4.U(4.W),
      ShapeM8N8K8.U    -> 1.U(4.W),
      ShapeM16N8K8.U   -> 2.U(4.W),
      ShapeM8N16K8.U   -> 2.U(4.W),
      ShapeM16N16K8.U  -> 4.U(4.W)
    ))
    val fp32Regs = MuxLookup(shape, MaxCDRegs.U(4.W))(Seq(
      ShapeM8N8K16.U   -> 2.U(4.W),
      ShapeM16N8K16.U  -> 4.U(4.W),
      ShapeM8N16K16.U  -> 4.U(4.W),
      ShapeM16N16K16.U -> 8.U(4.W),
      ShapeM8N8K8.U    -> 2.U(4.W),
      ShapeM16N8K8.U   -> 4.U(4.W),
      ShapeM8N16K8.U   -> 4.U(4.W),
      ShapeM16N16K8.U  -> 8.U(4.W)
    ))
    Mux(cdtype === CDTypeFP32.U, fp32Regs, fp16Regs)
  }

  def srcARegs(shape: Int): Int = shape match {
    case ShapeM8N8K16 | ShapeM8N16K16 | ShapeM8N8K8 | ShapeM8N16K8   => 2
    case ShapeM16N8K16 | ShapeM16N16K16 | ShapeM16N8K8 | ShapeM16N16K8 => 4
    case _ => MaxARegs
  }

  def srcBRegs(shape: Int): Int = shape match {
    case ShapeM8N8K16 | ShapeM16N8K16 | ShapeM8N8K8 | ShapeM16N8K8   => 2
    case ShapeM8N16K16 | ShapeM16N16K16 | ShapeM8N16K8 | ShapeM16N16K8 => 4
    case _ => MaxBRegs
  }

  def srcCDRegs(shape: Int, cdtype: Int): Int = cdtype match {
    case CDTypeFP16 => shape match {
      case ShapeM8N8K16 | ShapeM8N8K8     => 1
      case ShapeM16N8K16 | ShapeM16N8K8   => 2
      case ShapeM8N16K16 | ShapeM8N16K8   => 2
      case ShapeM16N16K16 | ShapeM16N16K8 => 4
      case _ => MaxCDRegs
    }
    case CDTypeFP32 => shape match {
      case ShapeM8N8K16 | ShapeM8N8K8     => 2
      case ShapeM16N8K16 | ShapeM16N8K8   => 4
      case ShapeM8N16K16 | ShapeM8N16K8   => 4
      case ShapeM16N16K16 | ShapeM16N16K8 => 8
      case _ => MaxCDRegs
    }
    case _ => MaxCDRegs
  }
}
