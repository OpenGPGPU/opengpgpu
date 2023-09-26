package opengpgpu.pipeline

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._

class WarpScheduler(implicit p: Parameters) extends Module {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val xLen = p(XLen)

  val io = IO(new Bundle {
    val warp_cmd = Flipped(DecoupledIO(new WarpCommandData()))
    val warp_ctl = Flipped(DecoupledIO(new WarpControlData()))
    val branch_ctl = Flipped(DecoupledIO(new BranchControlData()))
    val inst_fetch = DecoupledIO(new InstFetchData())
  })

  val warp_idle = RegInit(VecInit(Seq.fill(numWarps)(1.B)))
  val warp_active = RegInit(VecInit(Seq.fill(numWarps)(0.B)))
  val warp_pc = RegInit(VecInit(Seq.fill(numWarps)(0.U(addrWidth.W))))
  val warp_tmask = RegInit(VecInit(Seq.fill(numWarps)(0.U(numThreads.W))))
  val pop_valid = RegInit(0.B)
  val pop_wid = RegInit(0.U(log2Ceil(numWarps).W))
  io.warp_cmd.ready := warp_idle.asUInt.orR
  io.warp_ctl.ready := true.B
  io.branch_ctl.ready := true.B

  val has_idle = warp_idle.asUInt.orR
  val has_active = warp_active.asUInt.orR
  val idle_id = PriorityEncoder(warp_idle)
  val active_id = PriorityEncoder(warp_active)

  val simt_diverge = VecInit(Seq.fill(numWarps)(Wire(Bool())))
  val simt_data = VecInit(Seq.fill(numWarps)(Wire(new StackData())))
  val simt_stack = VecInit(Seq.fill(numWarps)(Module(new SIMTStack()).io))

  val pop_diverge = Wire(Bool())
  val pop_data = Wire(new StackData())

  // 使用for循环进行赋值
  for (i <- 0 until numWarps) {
    simt_stack(i).in_diverge := io.branch_ctl.bits.wid === i.U && io.branch_ctl.valid && io.branch_ctl.bits.diverge
    simt_stack(i).in_data := io.branch_ctl.bits.data
    simt_stack(i).push := io.branch_ctl.bits.wid === i.U && io.branch_ctl.valid
    simt_stack(i).pop := io.warp_ctl.bits.wid === i.U && io.warp_ctl.valid && io.warp_ctl.bits.join
    simt_diverge := simt_stack(i).out_diverge
    simt_data := simt_stack(i).out_data
  }

  pop_valid := io.warp_ctl.valid
  pop_wid := io.warp_ctl.bits.wid

  pop_diverge := simt_diverge(pop_wid)
  pop_data := simt_data(pop_wid)

  when(io.warp_cmd.fire) {
    warp_idle(idle_id) := 0.B
    warp_active(idle_id) := 1.B
    warp_pc(idle_id) := io.warp_cmd.bits.pc
    warp_tmask(idle_id) := io.warp_cmd.bits.mask
  }

  when(io.warp_ctl.valid) {
    warp_active(io.warp_ctl.bits.wid) := io.warp_ctl.bits.active
  }

  when(io.branch_ctl.valid) {
    warp_pc(io.branch_ctl.bits.wid) := io.branch_ctl.bits.pc
    warp_active(io.branch_ctl.bits.wid) := 1.B
  }

  when(pop_valid && pop_diverge) {
    warp_pc(pop_wid) := pop_data.pc
    warp_tmask(pop_wid) := pop_data.mask
  }

  when(io.inst_fetch.fire) {
    warp_active(io.inst_fetch.bits.wid) := 0.B
  }

}
