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

  io.out.mDim := MMAWindowInfo.mDim(io.in.shape)
  io.out.nDim := MMAWindowInfo.nDim(io.in.shape)

  val mIs16 = io.out.mDim === 16.U
  val nIs16 = io.out.nDim === 16.U
  val kIs8 = MMAWindowInfo.isK8Shape(io.in.shape)
  val kLimit = MMAWindowInfo.kDim(io.in.shape)
  val isTF32 = io.in.abtype === MMAConst.ABTypeTF32.U

  for (m <- 0 until 16) {
    for (k <- 0 until 16) {
      val fpRowMajorA = packed16(io.in.aWindow, m * 16 + k)
      val fpColMajorA = Mux(kIs8, packed16(io.in.aWindow, k * 16 + m),
        Mux(mIs16, packed16(io.in.aWindow, k * 16 + m), packed16(io.in.aWindow, k * 8 + m)))
      val tf32RowElemA = packed32(io.in.aWindow, m * 8 + (k / 2))
      val tf32ColElemA = Mux(mIs16, packed32(io.in.aWindow, (k / 2) * 16 + m), packed32(io.in.aWindow, (k / 2) * 8 + m))
      val tf32RowMajorA = if ((k & 1) == 0) tf32RowElemA(15, 0) else tf32RowElemA(31, 16)
      val tf32ColMajorA = if ((k & 1) == 0) tf32ColElemA(15, 0) else tf32ColElemA(31, 16)
      val fpVal = Mux(io.in.alayout, fpColMajorA, fpRowMajorA)
      val tf32Val = Mux(io.in.alayout, tf32ColMajorA, tf32RowMajorA)
      io.out.a(m)(k) := Mux(m.U < io.out.mDim,
        Mux(isTF32, tf32Val, Mux(k.U < kLimit, fpVal, 0.U(16.W))),
        0.U(16.W))
    }
  }

  for (n <- 0 until 16) {
    for (k <- 0 until 16) {
      val fpRowMajorB = packed16(io.in.bWindow, n * 16 + k)
      val fpColMajorB = Mux(kIs8, packed16(io.in.bWindow, k * 16 + n),
        Mux(nIs16, packed16(io.in.bWindow, k * 16 + n), packed16(io.in.bWindow, k * 8 + n)))
      val tf32RowElemB = packed32(io.in.bWindow, n * 8 + (k / 2))
      val tf32ColElemB = Mux(nIs16, packed32(io.in.bWindow, (k / 2) * 16 + n), packed32(io.in.bWindow, (k / 2) * 8 + n))
      val tf32RowMajorB = if ((k & 1) == 0) tf32RowElemB(15, 0) else tf32RowElemB(31, 16)
      val tf32ColMajorB = if ((k & 1) == 0) tf32ColElemB(15, 0) else tf32ColElemB(31, 16)
      val fpVal = Mux(io.in.blayout, fpRowMajorB, fpColMajorB)
      val tf32Val = Mux(io.in.blayout, tf32RowMajorB, tf32ColMajorB)
      io.out.b(n)(k) := Mux(n.U < io.out.nDim,
        Mux(isTF32, tf32Val, Mux(k.U < kLimit, fpVal, 0.U(16.W))),
        0.U(16.W))
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
  val shape = UInt(3.W)
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
  val dataType = io.in.bits.abtype(1, 0)
  when(io.in.valid) {
    assert(MMAWindowInfo.isLegal(io.in.bits.shape, io.in.bits.abtype, io.in.bits.cdtype),
      "illegal MMA input: TF32 requires k8 + FP32 accumulate; BF16 requires FP32 accumulate")
  }

  lanes.zipWithIndex.foreach { case (lane, idx) =>
    val row = idx / arrayN
    val col = idx % arrayN
    lane.io.in.valid := io.in.valid
    lane.io.in.bits := 0.U.asTypeOf(new FDA_HPInput(lane.config))
    lane.io.in.bits.vecA := io.in.bits.vecA(row)
    lane.io.in.bits.vecB := io.in.bits.vecB(col)
    lane.io.in.bits.dataType := dataType
    lane.io.in.bits.c := io.in.bits.c(idx)
    lane.io.in.bits.accType := io.in.bits.cdtype
    io.out.bits.result(idx) := lane.io.out.bits.result
    lane.io.out.ready := io.out.ready
  }

  io.in.ready := lanes.map(_.io.in.ready).reduce(_ && _)
  io.out.valid := lanes.map(_.io.out.valid).reduce(_ && _)
}

class MMASlotAlloc extends Bundle {
  val ctrl = new CtrlSigs()
  val a = Vec(16, Vec(16, UInt(16.W)))
  val b = Vec(16, Vec(16, UInt(16.W)))
  val c = Vec(16, Vec(16, UInt(32.W)))
}

class MMASlotIssue(arrayM: Int, arrayN: Int) extends Bundle {
  val rowBase = UInt(5.W)
  val colBase = UInt(5.W)
  val vecA = Vec(arrayM, Vec(16, UInt(16.W)))
  val vecB = Vec(arrayN, Vec(16, UInt(16.W)))
  val c = Vec(arrayM * arrayN, UInt(32.W))
  val shape = UInt(3.W)
  val abtype = UInt(4.W)
  val cdtype = UInt(1.W)
  val lastTile = Bool()
}

class MMASlotCollect(arrayM: Int, arrayN: Int) extends Bundle {
  val rowBase = UInt(5.W)
  val colBase = UInt(5.W)
  val result = Vec(arrayM * arrayN, UInt(32.W))
}

class MMASlot(arrayM: Int, arrayN: Int) extends Module {
  val io = IO(new Bundle {
    val alloc = Flipped(DecoupledIO(new MMASlotAlloc))
    val issue = DecoupledIO(new MMASlotIssue(arrayM, arrayN))
    val collect = Flipped(ValidIO(new MMASlotCollect(arrayM, arrayN)))
    val drain = DecoupledIO(new WriteVecCtrl)
    val active = Output(Bool())
  })

  def totalTiles(shape: UInt): UInt = {
    val mDim = MMAWindowInfo.mDim(shape)
    val nDim = MMAWindowInfo.nDim(shape)
    val rows = (mDim + (arrayM - 1).U) / arrayM.U
    val cols = (nDim + (arrayN - 1).U) / arrayN.U
    rows * cols
  }

  val validReg = RegInit(false.B)
  val ctrlReg = Reg(new CtrlSigs())
  val aReg = Reg(Vec(16, Vec(16, UInt(16.W))))
  val bReg = Reg(Vec(16, Vec(16, UInt(16.W))))
  val resultTileReg = Reg(Vec(16, Vec(16, UInt(xLen.W))))
  val issueRowBaseReg = RegInit(0.U(5.W))
  val issueColBaseReg = RegInit(0.U(5.W))
  val issueRemainingReg = RegInit(0.U(3.W))
  val collectPendingReg = RegInit(0.U(3.W))
  val drainIdxReg = RegInit(0.U(4.W))
  val doneReg = RegInit(false.B)

  val mDim = MMAWindowInfo.mDim(ctrlReg.mma_shape)
  val nDim = MMAWindowInfo.nDim(ctrlReg.mma_shape)
  val cdRegs = MMAWindowInfo.srcCDRegs(ctrlReg.mma_shape, ctrlReg.mma_cdtype)

  io.alloc.ready := !validReg
  io.issue.valid := validReg && issueRemainingReg.orR
  io.issue.bits.rowBase := issueRowBaseReg
  io.issue.bits.colBase := issueColBaseReg
  io.issue.bits.shape := ctrlReg.mma_shape
  io.issue.bits.abtype := ctrlReg.mma_abtype
  io.issue.bits.cdtype := ctrlReg.mma_cdtype
  io.issue.bits.lastTile := issueRemainingReg === 1.U
  for (r <- 0 until arrayM) {
    for (k <- 0 until 16) {
      io.issue.bits.vecA(r)(k) := aReg((issueRowBaseReg + r.U)(3, 0))(k)
    }
  }
  for (c <- 0 until arrayN) {
    for (k <- 0 until 16) {
      io.issue.bits.vecB(c)(k) := bReg((issueColBaseReg + c.U)(3, 0))(k)
    }
  }
  for (r <- 0 until arrayM) {
    for (c <- 0 until arrayN) {
      io.issue.bits.c(r * arrayN + c) := resultTileReg((issueRowBaseReg + r.U)(3, 0))((issueColBaseReg + c.U)(3, 0))
    }
  }

  io.drain.valid := validReg && doneReg
  io.drain.bits.warp_id := ctrlReg.wid
  io.drain.bits.reg_idxw := ctrlReg.reg_idxw + drainIdxReg
  io.drain.bits.wvd := ctrlReg.wvd
  io.drain.bits.wvd_mask.foreach(_ := true.B)
  if (SPIKE_OUTPUT) {
    io.drain.bits.spike_info.get := ctrlReg.spike_info.get
  }
  val flatElemsPerReg = Mux(ctrlReg.mma_cdtype === MMAConst.CDTypeFP32.U, 32.U, 64.U)
  val totalElems = mDim * nDim
  def compactElem(idx: UInt): UInt = {
    val row = Mux(nDim === 16.U, (idx >> 4)(3, 0), (idx >> 3)(3, 0))
    val col = Mux(nDim === 16.U, idx(3, 0), Cat(0.U(1.W), idx(2, 0)))
    resultTileReg(row)(col)
  }
  for (lane <- 0 until num_thread) {
    val baseIdx = drainIdxReg * flatElemsPerReg + Mux(ctrlReg.mma_cdtype === MMAConst.CDTypeFP32.U, lane.U, (lane * 2).U)
    val nextIdx = baseIdx + 1.U
    val baseValid = baseIdx < totalElems
    val nextValid = nextIdx < totalElems
    val fp32Val = Mux(baseValid, compactElem(baseIdx), 0.U)
    val fp16Lo = Mux(baseValid, compactElem(baseIdx)(15, 0), 0.U(16.W))
    val fp16Hi = Mux(nextValid, compactElem(nextIdx)(15, 0), 0.U(16.W))
    io.drain.bits.wb_wvd_rd(lane) := Mux(ctrlReg.mma_cdtype === MMAConst.CDTypeFP32.U, fp32Val, Cat(fp16Hi, fp16Lo))
  }

  when(io.alloc.fire) {
    validReg := true.B
    ctrlReg := io.alloc.bits.ctrl
    aReg := io.alloc.bits.a
    bReg := io.alloc.bits.b
    resultTileReg := io.alloc.bits.c
    issueRowBaseReg := 0.U
    issueColBaseReg := 0.U
    issueRemainingReg := totalTiles(io.alloc.bits.ctrl.mma_shape)
    collectPendingReg := totalTiles(io.alloc.bits.ctrl.mma_shape)
    drainIdxReg := 0.U
    doneReg := !totalTiles(io.alloc.bits.ctrl.mma_shape).orR
  }
  when(io.issue.fire) {
    val nextRow = issueRowBaseReg + arrayM.U
    val wrapRows = nextRow >= mDim
    issueRowBaseReg := Mux(wrapRows, 0.U, nextRow)
    issueColBaseReg := Mux(wrapRows, issueColBaseReg + arrayN.U, issueColBaseReg)
    issueRemainingReg := issueRemainingReg - 1.U
  }
  when(io.collect.valid) {
    for (r <- 0 until arrayM) {
      for (c <- 0 until arrayN) {
        when((io.collect.bits.rowBase + r.U) < mDim && (io.collect.bits.colBase + c.U) < nDim) {
          resultTileReg((io.collect.bits.rowBase + r.U)(3, 0))((io.collect.bits.colBase + c.U)(3, 0)) :=
            io.collect.bits.result(r * arrayN + c)
        }
      }
    }
    collectPendingReg := collectPendingReg - 1.U
    when(collectPendingReg === 1.U) {
      doneReg := true.B
    }
  }
  when(io.drain.fire) {
    when(drainIdxReg + 1.U >= cdRegs) {
      validReg := false.B
      issueRemainingReg := 0.U
      collectPendingReg := 0.U
      drainIdxReg := 0.U
      doneReg := false.B
    }.otherwise {
      drainIdxReg := drainIdxReg + 1.U
    }
  }

  io.active := validReg
}

class vMMAexe(arrayM: Int = 8, arrayN: Int = 8) extends Module {
  require(num_thread == 32, "warp-level MMA currently requires 32 threads")
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new MMAIssueData))
    val out_v = Decoupled(new WriteVecCtrl)
    val perf_mma_issue_count = Output(UInt(64.W))
    val perf_mma_busy_cycles = Output(UInt(64.W))
  })

  val maxInflight = 2
  val slotIdxWidth = scala.math.max(1, log2Ceil(maxInflight))
  class TileMeta extends Bundle {
    val slotIdx = UInt(slotIdxWidth.W)
    val rowBase = UInt(5.W)
    val colBase = UInt(5.W)
  }

  when(io.in.valid) {
    assert(MMAWindowInfo.isLegal(io.in.bits.ctrl.mma_shape, io.in.bits.ctrl.mma_abtype, io.in.bits.ctrl.mma_cdtype),
      "illegal MMA control: TF32 requires k8 + FP32 accumulate; BF16 requires FP32 accumulate")
  }

  val allocCanonicalizer = Module(new MMAFragmentCanonicalizer)
  allocCanonicalizer.io.in.shape := io.in.bits.ctrl.mma_shape
  allocCanonicalizer.io.in.abtype := io.in.bits.ctrl.mma_abtype
  allocCanonicalizer.io.in.cdtype := io.in.bits.ctrl.mma_cdtype
  allocCanonicalizer.io.in.alayout := io.in.bits.ctrl.mma_alayout
  allocCanonicalizer.io.in.blayout := io.in.bits.ctrl.mma_blayout
  allocCanonicalizer.io.in.aWindow := io.in.bits.aWindow
  allocCanonicalizer.io.in.bWindow := io.in.bits.bWindow
  allocCanonicalizer.io.in.cWindow := io.in.bits.cWindow

  val slots = Seq.fill(maxInflight)(Module(new MMASlot(arrayM, arrayN)))
  val dotArray = Module(new MMADotArray(arrayM, arrayN))
  val tileMetaQ = Module(new Queue(new TileMeta, maxInflight * 4, pipe = true))
  val drainArb = Module(new RRArbiter(new WriteVecCtrl, maxInflight))
  val perfMmaIssueCount = RegInit(0.U(64.W))
  val perfMmaBusyCycles = RegInit(0.U(64.W))
  val currentIssueSlot = RegInit(0.U(slotIdxWidth.W))

  val allocReadyVec = VecInit(slots.map(_.io.alloc.ready)).asUInt
  val allocIdx = PriorityEncoder(allocReadyVec)
  io.in.ready := allocReadyVec.orR
  val allocBits = Wire(new MMASlotAlloc)
  allocBits.ctrl := io.in.bits.ctrl
  allocBits.a := allocCanonicalizer.io.out.a
  allocBits.b := allocCanonicalizer.io.out.b
  allocBits.c := allocCanonicalizer.io.out.c
  slots.zipWithIndex.foreach { case (slot, idx) =>
    slot.io.alloc.valid := io.in.valid && allocReadyVec.orR && allocIdx === idx.U
    slot.io.alloc.bits := allocBits
  }

  val issueValidVec = VecInit(slots.map(_.io.issue.valid)).asUInt
  val currentIssueOH = UIntToOH(currentIssueSlot, maxInflight).asUInt
  val selectedIssueOH = Mux((issueValidVec & currentIssueOH).orR, currentIssueOH, PriorityEncoderOH(issueValidVec))
  val selectedIssueValid = selectedIssueOH.orR
  val selectedIssueIdx = OHToUInt(selectedIssueOH)
  val selectedIssue = Mux1H(selectedIssueOH, slots.map(_.io.issue.bits))

  dotArray.io.in.valid := selectedIssueValid && tileMetaQ.io.enq.ready
  dotArray.io.in.bits := 0.U.asTypeOf(new MMADotArrayInput(arrayM, arrayN))
  dotArray.io.in.bits.vecA := selectedIssue.vecA
  dotArray.io.in.bits.vecB := selectedIssue.vecB
  dotArray.io.in.bits.c := selectedIssue.c
  dotArray.io.in.bits.shape := selectedIssue.shape
  dotArray.io.in.bits.abtype := selectedIssue.abtype
  dotArray.io.in.bits.cdtype := selectedIssue.cdtype
  val issueFire = dotArray.io.in.fire
  slots.zipWithIndex.foreach { case (slot, idx) =>
    slot.io.issue.ready := issueFire && selectedIssueIdx === idx.U
  }
  tileMetaQ.io.enq.valid := issueFire
  tileMetaQ.io.enq.bits.slotIdx := selectedIssueIdx
  tileMetaQ.io.enq.bits.rowBase := selectedIssue.rowBase
  tileMetaQ.io.enq.bits.colBase := selectedIssue.colBase

  val collectPayload = Wire(new MMASlotCollect(arrayM, arrayN))
  collectPayload.rowBase := tileMetaQ.io.deq.bits.rowBase
  collectPayload.colBase := tileMetaQ.io.deq.bits.colBase
  collectPayload.result := dotArray.io.out.bits.result
  val collectFire = tileMetaQ.io.deq.valid && dotArray.io.out.valid
  dotArray.io.out.ready := tileMetaQ.io.deq.valid
  tileMetaQ.io.deq.ready := collectFire

  slots.zipWithIndex.foreach { case (slot, idx) =>
    slot.io.collect.valid := collectFire && tileMetaQ.io.deq.bits.slotIdx === idx.U
    slot.io.collect.bits := collectPayload
    drainArb.io.in(idx) <> slot.io.drain
  }

  when(io.in.fire) {
    perfMmaIssueCount := perfMmaIssueCount + 1.U
  }
  when(VecInit(slots.map(_.io.active)).asUInt.orR) {
    perfMmaBusyCycles := perfMmaBusyCycles + 1.U
  }
  when(issueFire && selectedIssue.lastTile) {
    currentIssueSlot := Mux(selectedIssueIdx === 0.U, 1.U, 0.U)
  }.elsewhen(!selectedIssueValid) {
    currentIssueSlot := Mux(currentIssueSlot === 0.U, 1.U, 0.U)
  }

  io.out_v <> drainArb.io.out
  io.perf_mma_issue_count := perfMmaIssueCount
  io.perf_mma_busy_cycles := perfMmaBusyCycles
}
