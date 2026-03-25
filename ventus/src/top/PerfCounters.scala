package top

import chisel3._

class PerfCounters extends Bundle {
  val sm_active_cycles = UInt(64.W)
  val sm_eligible_cycles = UInt(64.W)
  val issue_scalar_inst = UInt(64.W)
  val issue_vector_inst = UInt(64.W)
  val issue_vector_lanes = UInt(64.W)
  val scoreboard_stall_cycles = UInt(64.W)
  val barrier_stall_cycles = UInt(64.W)
  val lsu_backpressure_cycles = UInt(64.W)
  val dcache_read_miss = UInt(64.W)
  val dcache_write_miss = UInt(64.W)
  val mshr_full_stall_cycles = UInt(64.W)
  val shared_bank_conflict_cycles = UInt(64.W)
  val mma_issue_count = UInt(64.W)
  val mma_busy_cycles = UInt(64.W)
  val unfu_issue_count = UInt(64.W)
  val unfu_busy_cycles = UInt(64.W)
}

object PerfCounters {
  def zero: PerfCounters = 0.U.asTypeOf(new PerfCounters)

  def add(a: PerfCounters, b: PerfCounters): PerfCounters = {
    val out = Wire(new PerfCounters)
    out.sm_active_cycles := a.sm_active_cycles + b.sm_active_cycles
    out.sm_eligible_cycles := a.sm_eligible_cycles + b.sm_eligible_cycles
    out.issue_scalar_inst := a.issue_scalar_inst + b.issue_scalar_inst
    out.issue_vector_inst := a.issue_vector_inst + b.issue_vector_inst
    out.issue_vector_lanes := a.issue_vector_lanes + b.issue_vector_lanes
    out.scoreboard_stall_cycles := a.scoreboard_stall_cycles + b.scoreboard_stall_cycles
    out.barrier_stall_cycles := a.barrier_stall_cycles + b.barrier_stall_cycles
    out.lsu_backpressure_cycles := a.lsu_backpressure_cycles + b.lsu_backpressure_cycles
    out.dcache_read_miss := a.dcache_read_miss + b.dcache_read_miss
    out.dcache_write_miss := a.dcache_write_miss + b.dcache_write_miss
    out.mshr_full_stall_cycles := a.mshr_full_stall_cycles + b.mshr_full_stall_cycles
    out.shared_bank_conflict_cycles :=
      a.shared_bank_conflict_cycles + b.shared_bank_conflict_cycles
    out.mma_issue_count := a.mma_issue_count + b.mma_issue_count
    out.mma_busy_cycles := a.mma_busy_cycles + b.mma_busy_cycles
    out.unfu_issue_count := a.unfu_issue_count + b.unfu_issue_count
    out.unfu_busy_cycles := a.unfu_busy_cycles + b.unfu_busy_cycles
    out
  }
}
