package pipeline

import chisel3._
import chisel3.util._
import dotproduct.{FDA_HP, FDA_HPInput}
import top.parameters._

class MMAIssueData extends Bundle {
  val aWindow = Vec(MMAConst.MaxARegs, Vec(num_thread, UInt(xLen.W)))
  val bWindow = Vec(MMAConst.MaxBRegs, Vec(num_thread, UInt(xLen.W)))
  val cWindow = Vec(MMAConst.MaxCDRegs, Vec(num_thread, UInt(xLen.W)))
  val ctrl = new CtrlSigs()
}

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

class vMMAexe(arrayM: Int = 2, arrayN: Int = 1) extends Module {
  require(num_thread == 32, "warp-level MMA currently requires 32 threads")
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new MMAIssueData))
    val out_v = Decoupled(new WriteVecCtrl)
  })

  val dataBuffer = Queue(io.in, 1)
  val canonicalizer = Module(new MMAFragmentCanonicalizer)
  canonicalizer.io.in.shape := dataBuffer.bits.ctrl.mma_shape
  canonicalizer.io.in.abtype := dataBuffer.bits.ctrl.mma_abtype
  canonicalizer.io.in.cdtype := dataBuffer.bits.ctrl.mma_cdtype
  canonicalizer.io.in.alayout := dataBuffer.bits.ctrl.mma_alayout
  canonicalizer.io.in.blayout := dataBuffer.bits.ctrl.mma_blayout
  canonicalizer.io.in.aWindow := dataBuffer.bits.aWindow
  canonicalizer.io.in.bWindow := dataBuffer.bits.bWindow
  canonicalizer.io.in.cWindow := dataBuffer.bits.cWindow

  val dotArray = Module(new MMADotArray(arrayM, arrayN))

  val sIdle :: sIssue :: sWait :: sDrain :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val rowBase = RegInit(0.U(5.W))
  val colBase = RegInit(0.U(5.W))
  val drainIdx = RegInit(0.U(4.W))
  val resultTile = RegInit(VecInit(Seq.fill(16)(VecInit(Seq.fill(16)(0.U(xLen.W))))))

  val mDim = canonicalizer.io.out.mDim
  val nDim = canonicalizer.io.out.nDim
  val cdRegs = MMAWindowInfo.srcCDRegs(dataBuffer.bits.ctrl.mma_shape, dataBuffer.bits.ctrl.mma_cdtype)

  dotArray.io.in.valid := false.B
  dotArray.io.in.bits := 0.U.asTypeOf(new MMADotArrayInput(arrayM, arrayN))
  dotArray.io.out.ready := false.B
  for (r <- 0 until arrayM) {
    for (k <- 0 until 16) {
      dotArray.io.in.bits.vecA(r)(k) := canonicalizer.io.out.a(rowBase + r.U)(k)
    }
  }
  for (c <- 0 until arrayN) {
    for (k <- 0 until 16) {
      dotArray.io.in.bits.vecB(c)(k) := canonicalizer.io.out.b(colBase + c.U)(k)
    }
  }
  for (r <- 0 until arrayM) {
    for (c <- 0 until arrayN) {
      dotArray.io.in.bits.c(r * arrayN + c) := canonicalizer.io.out.c(rowBase + r.U)(colBase + c.U)
    }
  }
  dotArray.io.in.bits.abtype := dataBuffer.bits.ctrl.mma_abtype
  dotArray.io.in.bits.cdtype := dataBuffer.bits.ctrl.mma_cdtype

  dataBuffer.ready := false.B
  io.out_v.valid := false.B
  io.out_v.bits := 0.U.asTypeOf(new WriteVecCtrl)
  io.out_v.bits.warp_id := dataBuffer.bits.ctrl.wid
  io.out_v.bits.reg_idxw := dataBuffer.bits.ctrl.reg_idxw + drainIdx
  io.out_v.bits.wvd := true.B
  if (SPIKE_OUTPUT) {
    io.out_v.bits.spike_info.get := dataBuffer.bits.ctrl.spike_info.get
  }
  io.out_v.bits.wvd_mask.foreach(_ := true.B)

  val flatElemsPerReg = Mux(dataBuffer.bits.ctrl.mma_cdtype === MMAConst.CDTypeFP32.U, 32.U, 64.U)
  val totalElems = mDim * nDim
  def compactElem(idx: UInt): UInt = {
    val row = Mux(nDim === 16.U, idx >> 4, idx >> 3)
    val col = Mux(nDim === 16.U, idx(3, 0), idx(2, 0))
    resultTile(row)(col)
  }
  for (lane <- 0 until num_thread) {
    val baseIdx = drainIdx * flatElemsPerReg + Mux(dataBuffer.bits.ctrl.mma_cdtype === MMAConst.CDTypeFP32.U, lane.U, (lane * 2).U)
    val nextIdx = baseIdx + 1.U
    val baseValid = baseIdx < totalElems
    val nextValid = nextIdx < totalElems
    val fp32Val = Mux(baseValid, compactElem(baseIdx), 0.U)
    val fp16Lo = Mux(baseValid, compactElem(baseIdx)(15, 0), 0.U(16.W))
    val fp16Hi = Mux(nextValid, compactElem(nextIdx)(15, 0), 0.U(16.W))
    io.out_v.bits.wb_wvd_rd(lane) := Mux(dataBuffer.bits.ctrl.mma_cdtype === MMAConst.CDTypeFP32.U,
      fp32Val,
      Cat(fp16Hi, fp16Lo))
  }

  switch(state) {
    is(sIdle) {
      when(dataBuffer.valid) {
        rowBase := 0.U
        colBase := 0.U
        drainIdx := 0.U
        resultTile := 0.U.asTypeOf(resultTile)
        state := sIssue
      }
    }
    is(sIssue) {
      dotArray.io.in.valid := dataBuffer.valid
      when(dotArray.io.in.fire) {
        state := sWait
      }
    }
    is(sWait) {
      dotArray.io.out.ready := true.B
      when(dotArray.io.out.fire) {
        for (r <- 0 until arrayM) {
          for (c <- 0 until arrayN) {
            when((rowBase + r.U) < mDim && (colBase + c.U) < nDim) {
              resultTile(rowBase + r.U)(colBase + c.U) := dotArray.io.out.bits.result(r * arrayN + c)
            }
          }
        }
        val nextRow = rowBase + arrayM.U
        val nextCol = Mux(nextRow >= mDim, colBase + arrayN.U, colBase)
        rowBase := Mux(nextRow >= mDim, 0.U, nextRow)
        colBase := nextCol
        when(nextCol >= nDim) {
          state := sDrain
        }.otherwise {
          state := sIssue
        }
      }
    }
    is(sDrain) {
      io.out_v.valid := true.B
      when(io.out_v.fire) {
        when(drainIdx + 1.U >= cdRegs) {
          state := sIdle
          dataBuffer.ready := true.B
        }.otherwise {
          drainIdx := drainIdx + 1.U
        }
      }
    }
  }
}
