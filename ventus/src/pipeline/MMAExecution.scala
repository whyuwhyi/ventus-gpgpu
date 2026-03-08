package pipeline

import chisel3._
import chisel3.util._
import dotproduct.{FDA_HP, FDA_HPInput}

class MMAFragmentInput extends Bundle {
  val shape = UInt(3.W)
  val abtype = UInt(4.W)
  val cdtype = UInt(1.W)
  val alayout = Bool()
  val blayout = Bool()
  val aWindow = Vec(MMAConst.MaxARegs, Vec(32, UInt(32.W)))
  val bWindow = Vec(MMAConst.MaxBRegs, Vec(32, UInt(32.W)))
  val cWindow = Vec(MMAConst.MaxCDRegs, Vec(32, UInt(32.W)))
}

class MMAFragmentOutput extends Bundle {
  val mDim = UInt(5.W)
  val nDim = UInt(5.W)
  val a = Vec(16, Vec(16, UInt(16.W)))
  val b = Vec(16, Vec(16, UInt(16.W)))
  val c = Vec(16, Vec(16, UInt(32.W)))
}

class MMAFragmentCanonicalizer extends Module {
  val io = IO(new Bundle {
    val in = Input(new MMAFragmentInput)
    val out = Output(new MMAFragmentOutput)
  })

  def packed16(window: Vec[Vec[UInt]], idx: Int): UInt = {
    val reg = idx / 64
    val off = idx % 64
    val lane = off / 2
    val half = off % 2
    if (half == 0) window(reg)(lane)(15, 0) else window(reg)(lane)(31, 16)
  }

  def packed32(window: Vec[Vec[UInt]], idx: Int): UInt = {
    val reg = idx / 32
    val lane = idx % 32
    window(reg)(lane)
  }

  io.out.mDim := MuxLookup(io.in.shape, 16.U(5.W))(Seq(
    MMAConst.ShapeM8N8K16.U -> 8.U(5.W),
    MMAConst.ShapeM16N8K16.U -> 16.U(5.W),
    MMAConst.ShapeM8N16K16.U -> 8.U(5.W),
    MMAConst.ShapeM16N16K16.U -> 16.U(5.W)
  ))
  io.out.nDim := MuxLookup(io.in.shape, 16.U(5.W))(Seq(
    MMAConst.ShapeM8N8K16.U -> 8.U(5.W),
    MMAConst.ShapeM16N8K16.U -> 8.U(5.W),
    MMAConst.ShapeM8N16K16.U -> 16.U(5.W),
    MMAConst.ShapeM16N16K16.U -> 16.U(5.W)
  ))

  val mIs16 = io.out.mDim === 16.U
  val nIs16 = io.out.nDim === 16.U

  for (m <- 0 until 16) {
    for (k <- 0 until 16) {
      val rowMajorA = packed16(io.in.aWindow, m * 16 + k)
      val colMajorA = Mux(mIs16, packed16(io.in.aWindow, k * 16 + m), packed16(io.in.aWindow, k * 8 + m))
      io.out.a(m)(k) := Mux(io.in.alayout, colMajorA, rowMajorA)
    }
  }

  for (n <- 0 until 16) {
    for (k <- 0 until 16) {
      val rowMajorB = packed16(io.in.bWindow, n * 16 + k)
      val colMajorB = Mux(nIs16, packed16(io.in.bWindow, k * 16 + n), packed16(io.in.bWindow, k * 8 + n))
      io.out.b(n)(k) := Mux(io.in.blayout, rowMajorB, colMajorB)
    }
  }

  for (m <- 0 until 16) {
    for (n <- 0 until 16) {
      val flatIdx8 = m * 8 + n
      val flatIdx16 = m * 16 + n
      val fp16Val = Cat(0.U(16.W), Mux(nIs16, packed16(io.in.cWindow, flatIdx16), packed16(io.in.cWindow, flatIdx8)))
      val fp32Val = Mux(nIs16, packed32(io.in.cWindow, flatIdx16), packed32(io.in.cWindow, flatIdx8))
      io.out.c(m)(n) := Mux(io.in.cdtype === MMAConst.CDTypeFP32.U, fp32Val, fp16Val)
    }
  }
}

class MMADotArrayInput(arrayM: Int, arrayN: Int) extends Bundle {
  val vecA = Vec(arrayM, Vec(16, UInt(16.W)))
  val vecB = Vec(arrayN, Vec(16, UInt(16.W)))
  val c = Vec(arrayM * arrayN, UInt(32.W))
  val abtype = UInt(4.W)
  val cdtype = UInt(1.W)
}

class MMADotArrayOutput(arrayM: Int, arrayN: Int) extends Bundle {
  val result = Vec(arrayM * arrayN, UInt(32.W))
}

class MMADotArray(arrayM: Int, arrayN: Int) extends Module {
  require(arrayM > 0 && arrayN > 0)
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new MMADotArrayInput(arrayM, arrayN)))
    val out = Decoupled(new MMADotArrayOutput(arrayM, arrayN))
  })

  val lanes = Seq.fill(arrayM * arrayN)(Module(new FDA_HP))
  val isBF16 = io.in.bits.abtype === MMAConst.ABTypeBF16.U

  lanes.zipWithIndex.foreach { case (lane, idx) =>
    val row = idx / arrayN
    val col = idx % arrayN
    lane.io.in.valid := io.in.valid
    lane.io.in.bits := 0.U.asTypeOf(new FDA_HPInput(lane.config))
    lane.io.in.bits.vecA := io.in.bits.vecA(row)
    lane.io.in.bits.vecB := io.in.bits.vecB(col)
    lane.io.in.bits.dataType := isBF16
    lane.io.in.bits.c := io.in.bits.c(idx)
    lane.io.in.bits.accType := io.in.bits.cdtype
    io.out.bits.result(idx) := lane.io.out.bits.result
    lane.io.out.ready := io.out.ready
  }

  io.in.ready := lanes.map(_.io.in.ready).reduce(_ && _)
  io.out.valid := lanes.map(_.io.out.valid).reduce(_ && _)
}
