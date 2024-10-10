package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._

class Issue(
  implicit p: Parameters)
    extends Module {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val io = IO(new Bundle {
    val writeback = Flipped(DecoupledIO(new CommitVData()))
    val writeback_cmd = Flipped(DecoupledIO(new CommitVData()))
    val decode = Flipped(DecoupledIO(new DecodeData()))

    val alu = DecoupledIO(new VALUData())
    val lsu = DecoupledIO(new LSUData())

  })

  val decode_n = Reg(new DecodeData())
  val decode_valid_n = RegInit(0.B)
  when(io.decode.fire) {
    decode_n := io.decode.bits
    decode_valid_n := 1.B
  }.otherwise {
    decode_valid_n := 0.B
  }
  val vgpr = Module(new VGPR())
  val score_board = Module(new ScoreBoard())
  val dispatch = Module(new Dispatch())

  io.decode.ready := score_board.io.ibuffer.ready && dispatch.io.ibuffer.ready && !decode_valid_n

  vgpr.io.writeback.bits := io.writeback.bits
  vgpr.io.writeback.valid := io.writeback.valid
  vgpr.io.read_req.wid := io.decode.bits.wid
  vgpr.io.read_req.rs1 := io.decode.bits.rs1
  vgpr.io.read_req.rs2 := io.decode.bits.rs2
  vgpr.io.writeback_cmd <> io.writeback_cmd

  score_board.io.writeback <> io.writeback
  score_board.io.ibuffer.bits := io.decode.bits
  score_board.io.ibuffer.valid := io.decode.valid

  dispatch.io.ibuffer.valid := decode_valid_n
  dispatch.io.ibuffer.bits := decode_n
  dispatch.io.vgpr_rsp := vgpr.io.read_rsp

  io.alu <> dispatch.io.alu
  io.lsu <> dispatch.io.lsu
}
