package play

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import pipeline.{InstrDecodeV2, Instructions, Issue, UNFUexe, vExeData}
import top.parameters._

class UNFUDecodeTest extends AnyFreeSpec with ChiselScalatestTester {
  private def encodeCustomSfu(funct6: Int, dtype: Int, vs: Int, vd: Int): BigInt = {
    (BigInt(funct6) << 26) |
      (BigInt(1) << 25) |
      (BigInt(vs) << 20) |
      (BigInt(0) << 15) |
      (BigInt(dtype) << 12) |
      (BigInt(vd) << 7) |
      BigInt(0x2a)
  }

  "decode vex2.approx.f32 to unfu control" in {
    test(new InstrDecodeV2) { dut =>
      val inst = encodeCustomSfu(funct6 = 0x00, dtype = 0x0, vs = 4, vd = 8)
      dut.io.inst(0).poke(inst.U)
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
      dut.io.control(0).unfu.expect(true.B)
      dut.io.control(0).unfu_op.expect(0.U)
      dut.io.control(0).unfu_mode.expect(0.U)
      dut.io.control(0).isvec.expect(true.B)
      dut.io.control(0).wvd.expect(true.B)
      dut.io.control(0).sfu.expect(false.B)
      dut.io.control(0).fp.expect(false.B)
      dut.io.control(0).reg_idx2.expect(4.U)
      dut.io.control(0).reg_idxw.expect(8.U)
    }
  }
}

class UNFUIssueRoutingTest extends AnyFreeSpec with ChiselScalatestTester {
  "route unfu instructions to the dedicated issue port" in {
    test(new Issue) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.in1.foreach(_.poke(0.U))
      dut.io.in.bits.in2.foreach(_.poke(0.U))
      dut.io.in.bits.in3.foreach(_.poke(0.U))
      dut.io.in.bits.mask.foreach(_.poke(false.B))
      dut.io.in.bits.ctrl.unfu.poke(true.B)
      dut.io.in.bits.ctrl.isvec.poke(true.B)
      dut.io.in.bits.ctrl.wvd.poke(true.B)
      dut.io.in.bits.ctrl.reg_idxw.poke(3.U)
      dut.io.out_UNFU.ready.poke(true.B)
      dut.io.out_SFU.ready.poke(false.B)
      dut.io.out_vFPU.ready.poke(false.B)
      dut.io.out_vALU.ready.poke(false.B)
      dut.io.out_LSU.ready.poke(false.B)
      dut.io.out_SIMT.ready.poke(false.B)
      dut.io.out_warpscheduler.ready.poke(false.B)
      dut.io.out_CSR.ready.poke(false.B)
      dut.io.out_MUL.ready.poke(false.B)
      dut.io.out_TC.ready.poke(false.B)
      dut.io.out_sALU.ready.poke(false.B)
      dut.clock.step()

      dut.io.out_UNFU.valid.expect(true.B)
      dut.io.out_SFU.valid.expect(false.B)
      dut.io.in.ready.expect(true.B)
    }
  }
}

class UNFUExecutionTest extends AnyFreeSpec with ChiselScalatestTester {
  "execute fp32 exp2 on zero lanes and write back vector results" in {
    test(new UNFUexe) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.in1.foreach(_.poke(0.U))
      dut.io.in.bits.in2.foreach(_.poke(0.U))
      dut.io.in.bits.in3.foreach(_.poke(0.U))
      dut.io.in.bits.mask.foreach(_.poke(true.B))
      dut.io.in.bits.ctrl.unfu.poke(true.B)
      dut.io.in.bits.ctrl.unfu_op.poke(0.U)
      dut.io.in.bits.ctrl.unfu_mode.poke(0.U)
      dut.io.in.bits.ctrl.isvec.poke(true.B)
      dut.io.in.bits.ctrl.wvd.poke(true.B)
      dut.io.in.bits.ctrl.reg_idxw.poke(5.U)
      dut.io.in.bits.ctrl.wid.poke(0.U)
      dut.io.out_v.ready.poke(true.B)
      dut.io.out_x.ready.poke(false.B)
      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step()
      }
      dut.clock.step()
      dut.io.in.valid.poke(false.B)

      var cycles = 0
      while (!dut.io.out_v.valid.peek().litToBoolean && cycles < 128) {
        dut.clock.step()
        cycles += 1
      }

      dut.io.out_v.valid.expect(true.B)
      dut.io.out_v.bits.reg_idxw.expect(5.U)
      dut.io.out_v.bits.warp_id.expect(0.U)
      dut.io.out_v.bits.wvd.expect(true.B)
      dut.io.out_v.bits.wvd_mask.foreach(_.expect(true.B))
      dut.io.out_v.bits.wb_wvd_rd.foreach(_.expect("h3f800000".U))
    }
  }

  "only compute active lanes when unfu folds by num_sfu groups" in {
    test(new UNFUexe) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.in1.foreach(_.poke(0.U))
      dut.io.in.bits.in2.zipWithIndex.foreach { case (lane, idx) =>
        lane.poke("h3f800000".U)
      }
      dut.io.in.bits.in3.foreach(_.poke(0.U))
      dut.io.in.bits.mask.zipWithIndex.foreach { case (lane, idx) =>
        lane.poke((idx % 2 == 0).B)
      }
      dut.io.in.bits.ctrl.unfu.poke(true.B)
      dut.io.in.bits.ctrl.unfu_op.poke(0.U)
      dut.io.in.bits.ctrl.unfu_mode.poke(0.U)
      dut.io.in.bits.ctrl.isvec.poke(true.B)
      dut.io.in.bits.ctrl.wvd.poke(true.B)
      dut.io.in.bits.ctrl.reg_idxw.poke(7.U)
      dut.io.in.bits.ctrl.wid.poke(0.U)
      dut.io.out_v.ready.poke(true.B)
      dut.io.out_x.ready.poke(false.B)
      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step()
      }
      dut.clock.step()
      dut.io.in.valid.poke(false.B)

      var cycles = 0
      while (!dut.io.out_v.valid.peek().litToBoolean && cycles < 128) {
        dut.clock.step()
        cycles += 1
      }
      dut.io.out_v.valid.expect(true.B)
      dut.io.out_v.bits.wb_wvd_rd.zipWithIndex.foreach { case (lane, idx) =>
        if (idx % 2 == 0) {
          lane.expect("h40000000".U)
        } else {
          lane.expect(0.U)
        }
      }
    }
  }

  "execute back-to-back unfu ops without leaking stale results" in {
    test(new UNFUexe) { dut =>
      def driveInput(valid: Boolean, op: Int, mode: Int, value: BigInt, regIdx: Int): Unit = {
        dut.io.in.valid.poke(valid.B)
        dut.io.in.bits.in1.foreach(_.poke(0.U))
        dut.io.in.bits.in2.foreach(_.poke(value.U))
        dut.io.in.bits.in3.foreach(_.poke(0.U))
        dut.io.in.bits.mask.foreach(_.poke(true.B))
        dut.io.in.bits.ctrl.unfu.poke(true.B)
        dut.io.in.bits.ctrl.unfu_op.poke(op.U)
        dut.io.in.bits.ctrl.unfu_mode.poke(mode.U)
        dut.io.in.bits.ctrl.isvec.poke(true.B)
        dut.io.in.bits.ctrl.wvd.poke(true.B)
        dut.io.in.bits.ctrl.reg_idxw.poke(regIdx.U)
        dut.io.in.bits.ctrl.wid.poke(0.U)
      }

      dut.io.out_v.ready.poke(true.B)
      dut.io.out_x.ready.poke(false.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step()
      }
      driveInput(valid = true, op = 0, mode = 0, value = 0x00000000L, regIdx = 5)
      dut.clock.step()
      driveInput(valid = false, op = 0, mode = 0, value = 0, regIdx = 0)

      var cycles = 0
      while (!dut.io.out_v.valid.peek().litToBoolean && cycles < 128) {
        dut.clock.step()
        cycles += 1
      }
      dut.io.out_v.valid.expect(true.B)
      dut.io.out_v.bits.reg_idxw.expect(5.U)
      dut.io.out_v.bits.wb_wvd_rd.foreach(_.expect("h3f800000".U))
      dut.clock.step()

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step()
      }
      driveInput(valid = true, op = 2, mode = 0, value = 0x40000000L, regIdx = 6)
      dut.clock.step()
      driveInput(valid = false, op = 0, mode = 0, value = 0, regIdx = 0)

      cycles = 0
      while (!dut.io.out_v.valid.peek().litToBoolean && cycles < 128) {
        dut.clock.step()
        cycles += 1
      }
      dut.io.out_v.valid.expect(true.B)
      dut.io.out_v.bits.reg_idxw.expect(6.U)
      dut.io.out_v.bits.wb_wvd_rd.foreach { lane =>
        val bits = lane.peek().litValue
        assert(bits == 0x3f000000L || bits == 0x3effffffL, f"unexpected rcp approximation result: 0x$bits%08x")
      }
    }
  }

  "keep unfu group issue close to II=1 instead of waiting full pipeline latency between groups" in {
    test(new UNFUexe) { dut =>
      def measureLatency(maskFn: Int => Boolean, regIdx: Int): Int = {
        dut.io.out_v.ready.poke(true.B)
        dut.io.out_x.ready.poke(false.B)
        while (!dut.io.in.ready.peek().litToBoolean) {
          dut.clock.step()
        }
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.in1.foreach(_.poke(0.U))
        dut.io.in.bits.in2.foreach(_.poke(0.U))
        dut.io.in.bits.in3.foreach(_.poke(0.U))
        dut.io.in.bits.mask.zipWithIndex.foreach { case (lane, idx) =>
          lane.poke(maskFn(idx).B)
        }
        dut.io.in.bits.ctrl.unfu.poke(true.B)
        dut.io.in.bits.ctrl.unfu_op.poke(0.U)
        dut.io.in.bits.ctrl.unfu_mode.poke(0.U)
        dut.io.in.bits.ctrl.isvec.poke(true.B)
        dut.io.in.bits.ctrl.wvd.poke(true.B)
        dut.io.in.bits.ctrl.reg_idxw.poke(regIdx.U)
        dut.io.in.bits.ctrl.wid.poke(0.U)
        dut.clock.step()
        dut.io.in.valid.poke(false.B)

        var cycles = 0
        while (!dut.io.out_v.valid.peek().litToBoolean && cycles < 256) {
          dut.clock.step()
          cycles += 1
        }
        dut.io.out_v.valid.expect(true.B)
        dut.clock.step()
        cycles
      }

      val singleGroupLatency = measureLatency(idx => idx < num_sfu, regIdx = 10)
      val fullWarpLatency = measureLatency(_ => true, regIdx = 11)
      val extraCycles = fullWarpLatency - singleGroupLatency

      assert(
        extraCycles <= (num_thread / num_sfu) + 2,
        s"UNFU wrapper inserted too many bubbles between groups: single=$singleGroupLatency full=$fullWarpLatency extra=$extraCycles"
      )
    }
  }

}

class UNFUInstructionsTest extends AnyFreeSpec {
  "expose explicit UNFU instruction bitpats in Instructions.scala" in {
    assert(Instructions.VEX2_APPROX_F32.toString.contains("0101010"))
    assert(Instructions.VRCP_APPROX_F16X2.toString.contains("0101010"))
    assert(Instructions.VSILU_APPROX_BF16X2.toString.contains("0101010"))
  }
}
