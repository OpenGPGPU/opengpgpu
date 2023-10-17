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
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

class Dispatch(implicit p: Parameters) extends Module {
  val numThread = p(ThreadNum)
  val xLen = p(XLen)
  val io = IO(new Bundle {
    val ibuffer = Flipped(DecoupledIO(new DecodeData()))
    val gpr_rsp = Flipped(new ReadGPRRsp())

    val alu = DecoupledIO(new ALUData())
    val lsu = DecoupledIO(new LSUData())
  })

  val buffer = Module(
    new Queue(
      new Bundle {
        val decode = new DecodeData
        val gpr_rsp = new ReadGPRRsp()
      },
      1,
      pipe = true
    )
  )

  val buffer_deq = buffer.io.deq.bits
  buffer.io.enq.valid := io.ibuffer.valid
  buffer.io.enq.bits.decode := io.ibuffer.bits
  buffer.io.enq.bits.gpr_rsp := io.gpr_rsp
  io.ibuffer.ready := buffer.io.enq.ready

  val ex_op1 = Wire(Vec(numThread, UInt(xLen.W)))
  val ex_op2 = Wire(Vec(numThread, UInt(xLen.W)))
  val pc_vec = Wire(Vec(numThread, UInt(xLen.W)))
  val imm_vec = Wire(Vec(numThread, UInt(xLen.W)))
  val const_vec = Wire(Vec(numThread, UInt(xLen.W)))

  pc_vec := VecInit.tabulate(numThread) { i => buffer_deq.decode.pc }
  imm_vec := VecInit.tabulate(numThread) { i => buffer_deq.decode.imm }
  const_vec := VecInit.tabulate(numThread) { i => 4.U }

  ex_op1 := MuxLookup(buffer_deq.decode.sel_alu1.asUInt, 0.U.asTypeOf(ex_op1))(
    Seq(A1_RS1.asUInt -> buffer_deq.gpr_rsp.rs1_data, A1_PC.asUInt -> pc_vec)
  )
  ex_op2 := MuxLookup(buffer_deq.decode.sel_alu2.asUInt, 0.U.asTypeOf(ex_op1))(
    Seq(A2_RS2.asUInt -> buffer_deq.gpr_rsp.rs2_data, A2_IMM.asUInt -> imm_vec, A2_SIZE.asUInt -> const_vec)
  )

  io.alu.valid := buffer.io.deq.valid && buffer_deq.decode.ex_type === ExType.ALU
  io.alu.bits.op1 := ex_op1
  io.alu.bits.op2 := ex_op2
  io.alu.bits.func := buffer_deq.decode.func
  io.alu.bits.mask := buffer_deq.decode.mask
  io.alu.bits.wid := buffer_deq.decode.wid
  io.alu.bits.pc := buffer_deq.decode.pc
  io.alu.bits.rd := buffer_deq.decode.rd
  io.alu.bits.imm := buffer_deq.decode.imm
  io.alu.bits.rs1_data := buffer_deq.gpr_rsp.rs1_data
  io.alu.bits.branch := buffer_deq.decode.branch

  io.lsu.valid := buffer.io.deq.valid && buffer_deq.decode.ex_type === ExType.LSU
  io.lsu.bits.func := buffer_deq.decode.mem_cmd
  io.lsu.bits.wid := buffer_deq.decode.wid
  io.lsu.bits.mask := buffer_deq.decode.mask
  io.lsu.bits.addr := buffer_deq.gpr_rsp.rs1_data
  io.lsu.bits.rd := buffer_deq.decode.rd
  io.lsu.bits.data := buffer_deq.gpr_rsp.rs2_data
  io.lsu.bits.offset := buffer_deq.decode.imm
  io.lsu.bits.pc := buffer_deq.decode.pc

  val mapping = Seq((0.U, io.alu.ready), (1.U, io.lsu.ready))

  buffer.io.deq.ready := MuxLookup(buffer_deq.decode.ex_type, 1.B)(mapping)
}
