package pipeline

import chisel3._
import chisel3.util._

object MMAConst {
  val ShapeM8N8K16   = 0
  val ShapeM16N8K16  = 1
  val ShapeM8N16K16  = 2
  val ShapeM16N16K16 = 3

  val ABTypeFP16 = 0
  val ABTypeBF16 = 1

  val CDTypeFP16 = 0
  val CDTypeFP32 = 1

  val MaxARegs  = 4
  val MaxBRegs  = 4
  val MaxCDRegs = 8
}

object MMAWindowInfo {
  import MMAConst._

  def srcARegs(shape: UInt): UInt = MuxLookup(shape, MaxARegs.U(3.W))(Seq(
    ShapeM8N8K16.U   -> 2.U(3.W),
    ShapeM16N8K16.U  -> 4.U(3.W),
    ShapeM8N16K16.U  -> 2.U(3.W),
    ShapeM16N16K16.U -> 4.U(3.W)
  ))

  def srcBRegs(shape: UInt): UInt = MuxLookup(shape, MaxBRegs.U(3.W))(Seq(
    ShapeM8N8K16.U   -> 2.U(3.W),
    ShapeM16N8K16.U  -> 2.U(3.W),
    ShapeM8N16K16.U  -> 4.U(3.W),
    ShapeM16N16K16.U -> 4.U(3.W)
  ))

  def srcCDRegs(shape: UInt, cdtype: UInt): UInt = {
    val fp16Regs = MuxLookup(shape, MaxCDRegs.U(4.W))(Seq(
      ShapeM8N8K16.U   -> 1.U(4.W),
      ShapeM16N8K16.U  -> 2.U(4.W),
      ShapeM8N16K16.U  -> 2.U(4.W),
      ShapeM16N16K16.U -> 4.U(4.W)
    ))
    val fp32Regs = MuxLookup(shape, MaxCDRegs.U(4.W))(Seq(
      ShapeM8N8K16.U   -> 2.U(4.W),
      ShapeM16N8K16.U  -> 4.U(4.W),
      ShapeM8N16K16.U  -> 4.U(4.W),
      ShapeM16N16K16.U -> 8.U(4.W)
    ))
    Mux(cdtype === CDTypeFP32.U, fp32Regs, fp16Regs)
  }

  def srcARegs(shape: Int): Int = shape match {
    case ShapeM8N8K16 | ShapeM8N16K16   => 2
    case ShapeM16N8K16 | ShapeM16N16K16 => 4
    case _ => MaxARegs
  }

  def srcBRegs(shape: Int): Int = shape match {
    case ShapeM8N8K16 | ShapeM16N8K16   => 2
    case ShapeM8N16K16 | ShapeM16N16K16 => 4
    case _ => MaxBRegs
  }

  def srcCDRegs(shape: Int, cdtype: Int): Int = cdtype match {
    case CDTypeFP16 => shape match {
      case ShapeM8N8K16   => 1
      case ShapeM16N8K16  => 2
      case ShapeM8N16K16  => 2
      case ShapeM16N16K16 => 4
      case _ => MaxCDRegs
    }
    case CDTypeFP32 => shape match {
      case ShapeM8N8K16   => 2
      case ShapeM16N8K16  => 4
      case ShapeM8N16K16  => 4
      case ShapeM16N16K16 => 8
      case _ => MaxCDRegs
    }
    case _ => MaxCDRegs
  }
}
