package play

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import pipeline.InstrDecodeV2

class MMADecodeTest extends AnyFreeSpec with ChiselScalatestTester {
  private def encodeMma32(abtype: Int, shape: Int, vrs2: Int, vrs1: Int,
                          alayout: Int, blayout: Int, cdtype: Int, vrd: Int): BigInt = {
    (BigInt(abtype) << 28) |
      (BigInt(shape) << 25) |
      (BigInt(vrs2) << 20) |
      (BigInt(vrs1) << 15) |
      (BigInt(alayout) << 14) |
      (BigInt(blayout) << 13) |
      (BigInt(cdtype) << 12) |
      (BigInt(vrd) << 7) |
      BigInt(0x0a)
  }

  "decode all MMA shapes through the tc route" in {
    test(new InstrDecodeV2) { dut =>
      val cases = Seq(
        encodeMma32(abtype = 0, shape = 0, vrs2 = 10, vrs1 = 8, alayout = 0, blayout = 1, cdtype = 1, vrd = 6) -> 0,
        encodeMma32(abtype = 0, shape = 1, vrs2 = 11, vrs1 = 9, alayout = 1, blayout = 0, cdtype = 0, vrd = 7) -> 1,
        encodeMma32(abtype = 1, shape = 2, vrs2 = 12, vrs1 = 4, alayout = 0, blayout = 1, cdtype = 1, vrd = 3) -> 2,
        encodeMma32(abtype = 1, shape = 3, vrs2 = 13, vrs1 = 5, alayout = 1, blayout = 0, cdtype = 1, vrd = 2) -> 3,
      )

      dut.io.inst(0).poke(cases.head._1.U)
      dut.io.inst(1).poke(0.U)
      dut.io.inst_mask(0).poke(true.B)
      dut.io.inst_mask(1).poke(false.B)
      dut.io.pc.poke(0x80000000L.U)
      dut.io.wid.poke(0.U)
      dut.io.sm_id.poke(0.U)
      dut.io.flush_wid.valid.poke(false.B)
      dut.io.flush_wid.bits.poke(0.U)
      dut.io.ibuffer_ready.foreach(_.poke(true.B))
      dut.clock.step()

      dut.io.control_mask(0).expect(true.B)
      dut.io.control(0).tc.expect(true.B)
      dut.io.control(0).mma.expect(true.B)
      dut.io.control(0).mma_shape.expect(cases.head._2.U)
      dut.io.control(0).mma_abtype.expect(0.U)
      dut.io.control(0).mma_cdtype.expect(1.U)
      dut.io.control(0).mma_alayout.expect(false.B)
      dut.io.control(0).mma_blayout.expect(true.B)
      dut.io.control(0).reg_idx1.expect(8.U)
      dut.io.control(0).reg_idx2.expect(10.U)
      dut.io.control(0).reg_idxw.expect(6.U)
    }
  }
}


class MMAWindowInfoTest extends AnyFreeSpec {
  "report exact source and destination window sizes for all shapes" in {
    assert(pipeline.MMAWindowInfo.srcARegs(0) == 2)
    assert(pipeline.MMAWindowInfo.srcARegs(1) == 4)
    assert(pipeline.MMAWindowInfo.srcARegs(2) == 2)
    assert(pipeline.MMAWindowInfo.srcARegs(3) == 4)

    assert(pipeline.MMAWindowInfo.srcBRegs(0) == 2)
    assert(pipeline.MMAWindowInfo.srcBRegs(1) == 2)
    assert(pipeline.MMAWindowInfo.srcBRegs(2) == 4)
    assert(pipeline.MMAWindowInfo.srcBRegs(3) == 4)

    assert(pipeline.MMAWindowInfo.srcCDRegs(0, 0) == 1)
    assert(pipeline.MMAWindowInfo.srcCDRegs(1, 0) == 2)
    assert(pipeline.MMAWindowInfo.srcCDRegs(2, 0) == 2)
    assert(pipeline.MMAWindowInfo.srcCDRegs(3, 0) == 4)

    assert(pipeline.MMAWindowInfo.srcCDRegs(0, 1) == 2)
    assert(pipeline.MMAWindowInfo.srcCDRegs(1, 1) == 4)
    assert(pipeline.MMAWindowInfo.srcCDRegs(2, 1) == 4)
    assert(pipeline.MMAWindowInfo.srcCDRegs(3, 1) == 8)
  }
}


class MMADotArrayTest extends AnyFreeSpec with ChiselScalatestTester {
  "compute a 2x1 sub-tile with two FDA_HP lanes in parallel" in {
    test(new pipeline.MMADotArray(2, 1)) { dut =>
      dut.io.in.valid.poke(true.B)
      for (row <- 0 until 2) {
        for (k <- 0 until 16) {
          dut.io.in.bits.vecA(row)(k).poke("h3c00".U)
        }
      }
      for (k <- 0 until 16) {
        dut.io.in.bits.vecB(0)(k).poke("h3c00".U)
      }
      dut.io.in.bits.c(0).poke(0.U)
      dut.io.in.bits.c(1).poke(0.U)
      dut.io.in.bits.abtype.poke(0.U)
      dut.io.in.bits.cdtype.poke(1.U)
      dut.io.out.ready.poke(true.B)
      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step()
      }
      dut.clock.step()
      dut.io.in.valid.poke(false.B)
      var cycles = 0
      while (!dut.io.out.valid.peek().litToBoolean && cycles < 64) {
        dut.clock.step()
        cycles += 1
      }
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.result(0).expect("h41800000".U)
      dut.io.out.bits.result(1).expect("h41800000".U)
    }
  }
}


class MMAFragmentCanonicalizerTest extends AnyFreeSpec with ChiselScalatestTester {
  "decode row/col layouts into canonical A/B/C tiles" in {
    test(new pipeline.MMAFragmentCanonicalizer) { dut =>
      dut.io.in.shape.poke(0.U)
      dut.io.in.abtype.poke(0.U)
      dut.io.in.cdtype.poke(1.U)
      dut.io.in.alayout.poke(false.B)
      dut.io.in.blayout.poke(true.B)
      for (i <- 0 until pipeline.MMAConst.MaxARegs) {
        for (lane <- 0 until 32) {
          dut.io.in.aWindow(i)(lane).poke((((i * 64 + lane * 2 + 1) << 16) | (i * 64 + lane * 2)).U)
        }
      }
      for (i <- 0 until pipeline.MMAConst.MaxBRegs) {
        for (lane <- 0 until 32) {
          dut.io.in.bWindow(i)(lane).poke((((i * 64 + lane * 2 + 1) << 16) | (i * 64 + lane * 2)).U)
        }
      }
      for (i <- 0 until pipeline.MMAConst.MaxCDRegs) {
        for (lane <- 0 until 32) {
          dut.io.in.cWindow(i)(lane).poke((i * 32 + lane).U)
        }
      }
      dut.clock.step()
      dut.io.out.a(0)(0).expect(0.U)
      dut.io.out.a(1)(0).expect(16.U)
      dut.io.out.b(0)(0).expect(0.U)
      dut.io.out.b(1)(0).expect(16.U)
      dut.io.out.c(1)(2).expect(10.U)

      dut.io.in.alayout.poke(true.B)
      dut.io.in.blayout.poke(false.B)
      dut.clock.step()
      dut.io.out.a(0)(0).expect(0.U)
      dut.io.out.a(1)(0).expect(1.U)
      dut.io.out.b(0)(0).expect(0.U)
      dut.io.out.b(1)(0).expect(1.U)
    }
  }
}
