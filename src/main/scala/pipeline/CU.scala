package opengpgpu.pipeline

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._

class CU(implicit p: Parameters) extends LazyModule {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val dataWidth = p(XLen)

  val ifetch = LazyModule(new InstFetch())
  val lsu = LazyModule(new LSU())

  val xbar = AXI4Xbar()
  xbar := ifetch.axi_master
  xbar := lsu.axi_master

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val in = Flipped(Decoupled(new WarpCommandData))
      val out = Decoupled(new WarpEndData)
    })

    val warp_sched = Module(new WarpScheduler())
    val idecode = Module(new IDecodeUnit())
    val issue = Module(new Issue())
    val vector_alu = Module(new VectorALU())
    val branch = Module(new BranchUnit())
    val write_back = Module(new Writeback())

    io.in <> warp_sched.io.warp_cmd
    io.out <> warp_sched.io.warp_end

    // warp_sched
    warp_sched.io.inst_fetch <> ifetch.module.io.inst_fetch
    warp_sched.io.warp_ctl <> idecode.io.wcontrol
    warp_sched.io.branch_ctl <> branch.io.branch_ctl

    // ifetch -> idecode
    ifetch.module.io.inst_out <> idecode.io.inst

    // idecode -> issue
    idecode.io.decode <> issue.io.decode

    // issue -> lsu
    issue.io.lsu <> lsu.module.io.in
    issue.io.alu <> vector_alu.io.in

    // lsu alu -> writeback
    lsu.module.io.out_wb <> write_back.io.lsu_commit
    vector_alu.io.out <> write_back.io.alu_commit

    // write back -> issue
    write_back.io.writeback <> issue.io.writeback
    vector_alu.io.branch_data <> branch.io.branch_data
  }
}
