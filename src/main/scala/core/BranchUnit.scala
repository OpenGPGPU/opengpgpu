package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._

class BranchUnit(
  implicit p: Parameters)
    extends Module {
  val numThread = p(ThreadNum)
  val io = IO(new Bundle {
    val branch_data = Flipped(DecoupledIO(new BranchData()))
    val branch_ctl = DecoupledIO(new BranchControlData())
  })

  val branch_result = Module(new Queue(new BranchControlData(), 1, pipe = true))

  io.branch_data.ready := branch_result.io.enq.ready
  branch_result.io.enq.valid := io.branch_data.valid

  // default
  branch_result.io.enq.bits.pc := 0.U
  branch_result.io.enq.bits.mask := 0.U.asTypeOf(branch_result.io.enq.bits.mask)
  branch_result.io.enq.bits.wid := 0.U
  branch_result.io.enq.bits.diverge := 0.U

  branch_result.io.enq.bits.data.mask := 0.U.asTypeOf(branch_result.io.enq.bits.mask)
  branch_result.io.enq.bits.data.pc := 0.U
  branch_result.io.enq.bits.data.orig_mask := 0.U.asTypeOf(branch_result.io.enq.bits.mask)

  val pc_imm = io.branch_data.bits.pc + io.branch_data.bits.imm
  val pc_rs1 = io.branch_data.bits.rs1_data + io.branch_data.bits.imm
  val pc_next = io.branch_data.bits.pc + 4.U

  val taken_all = io.branch_data.bits.mask === io.branch_data.bits.orig_mask
  val taken_none = io.branch_data.bits.mask === 0.U.asTypeOf(io.branch_data.bits.orig_mask)
  val diverge = !(taken_all | taken_none)

  when(io.branch_data.bits.branch.jal) {
    branch_result.io.enq.bits.pc := pc_imm
    branch_result.io.enq.bits.mask := io.branch_data.bits.orig_mask
    branch_result.io.enq.bits.wid := io.branch_data.bits.wid
    branch_result.io.enq.bits.diverge := false.B

  }.elsewhen(io.branch_data.bits.branch.jalr) {
    branch_result.io.enq.bits.pc := pc_rs1
    branch_result.io.enq.bits.mask := io.branch_data.bits.orig_mask
    branch_result.io.enq.bits.wid := io.branch_data.bits.wid
    branch_result.io.enq.bits.diverge := false.B
  }.elsewhen(io.branch_data.bits.branch.branch) {
    branch_result.io.enq.bits.pc := Mux(taken_none, pc_next, pc_imm)
    branch_result.io.enq.bits.mask := Mux(diverge, io.branch_data.bits.mask, io.branch_data.bits.orig_mask)
    branch_result.io.enq.bits.wid := io.branch_data.bits.wid
    branch_result.io.enq.bits.diverge := diverge

    branch_result.io.enq.bits.data.mask := io.branch_data.bits.orig_mask.zip(io.branch_data.bits.mask).map {
      case (a, b) => a & !b
    }
    branch_result.io.enq.bits.data.pc := pc_next
    branch_result.io.enq.bits.data.orig_mask := io.branch_data.bits.orig_mask
  }

  io.branch_ctl <> branch_result.io.deq

}
