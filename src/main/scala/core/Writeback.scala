package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class Writeback(
  implicit p: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val alu_commit = Flipped(DecoupledIO(new CommitVData()))
    val lsu_commit = Flipped(DecoupledIO(new CommitVData()))
    val writeback = DecoupledIO(new CommitVData())
  })

  val rsp_data = VecInit(
    Seq(
      io.alu_commit,
      io.lsu_commit
    )
  )

  val rsp_arbiter = Module(new RRArbiter(new CommitVData(), 2))
  rsp_arbiter.io.in <> rsp_data

  val outQue = Module(new Queue(new CommitVData(), 1, pipe = true))
  outQue.io.enq <> rsp_arbiter.io.out

  io.writeback <> outQue.io.deq
}
