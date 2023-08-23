/*
 * Copyright (c) 2023 OpenGPGPU
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package opengpgpu.pipeline

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._

class Issue(implicit p: Parameters) extends Module {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val io = IO(new Bundle {
    val writeback = Flipped(DecoupledIO(new CommitData()))
    val decode = Flipped(DecoupledIO(new DecodeData()))

    val alu = DecoupledIO(new ALUData())
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
  val gpr = Module(new GPR())
  val score_board = Module(new ScoreBoard())
  val dispatch = Module(new Dispatch())

  io.decode.ready := score_board.io.ibuffer.ready && dispatch.io.ibuffer.ready

  gpr.io.writeback.bits := io.writeback.bits
  gpr.io.writeback.valid := io.writeback.valid
  gpr.io.read_req.wid := io.decode.bits.wid
  gpr.io.read_req.rs1 := io.decode.bits.rs1
  gpr.io.read_req.rs2 := io.decode.bits.rs2

  score_board.io.writeback <> io.writeback
  score_board.io.ibuffer.bits := io.decode.bits
  score_board.io.ibuffer.valid := io.decode.valid

  dispatch.io.ibuffer.valid := decode_valid_n
  dispatch.io.ibuffer.bits := decode_n
  dispatch.io.gpr_rsp := gpr.io.read_rsp

  io.alu <> dispatch.io.alu
  io.lsu <> dispatch.io.lsu
}
