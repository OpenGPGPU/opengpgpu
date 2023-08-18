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

class ScalarALU(aluFn: ALUFN)(implicit p: Parameters) extends Module {
  val xLen = p(XLen)
  val io = IO(new Bundle() {
    val func = Input(UInt(aluFn.SZ_ALU_FN.W))
    val op1 = Input(UInt(xLen.W))
    val op2 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
    val cmp_out = Output(Bool())
  })

  // ADD, SUB
  val op2_inv = Mux(aluFn.isSub(io.func), ~io.op2, io.op2)
  val adder_out = io.op1 + op2_inv + aluFn.isSub(io.func).asUInt

  // SLT, SLTU
  val op1_xor_op2 = io.op1 ^ op2_inv
  val slt = Mux(
    io.op1(xLen - 1) === io.op2(xLen - 1),
    adder_out(xLen - 1),
    Mux(aluFn.cmpUnsigned(io.func), io.op2(xLen - 1), io.op1(xLen - 1))
  )
  io.cmp_out := aluFn.cmpInverted(io.func) ^ Mux(aluFn.cmpEq(io.func), op1_xor_op2 === 0.U(xLen.W), slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) = (io.op2(4, 0), io.op1)
  // 64 bits
  // val (shamt, shin_r) =
  //   if (xLen == 32) (io.op2(4,0), io.op1)
  //   else {
  //     require(xLen == 64)
  //     val shin_hi_32 = Fill(32, aluFn.isSub(io.fn) && io.op1(31))
  //     val shin_hi = Mux(io.dw === DW_64, io.op1(63,32), shin_hi_32)
  //     val shamt = Cat(io.op2(5) & (io.dw === DW_64), io.op2(4,0))
  //     (shamt, Cat(shin_hi, io.op1(31,0)))
  //   }
  val shin = Mux(io.func === aluFn.FN_SR || io.func === aluFn.FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(aluFn.isSub(io.func) & shin(xLen - 1), shin).asSInt >> shamt)(xLen - 1, 0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.func === aluFn.FN_SR || io.func === aluFn.FN_SRA, shout_r, 0.U(xLen.W)) |
    Mux(io.func === aluFn.FN_SL, shout_l, 0.U(xLen.W))

  // AND, OR, XOR
  val logic = Mux(
    io.func === aluFn.FN_XOR,
    io.op1 ^ io.op2,
    Mux(io.func === aluFn.FN_OR, io.op1 | io.op2, Mux(io.func === aluFn.FN_AND, io.op1 & io.op2, 0.U(xLen.W)))
  )

  val shift_logic_cmp = (aluFn.isCmp(io.func) && slt) | logic | shout
  val out = Mux(io.func === aluFn.FN_ADD || io.func === aluFn.FN_SUB, adder_out, shift_logic_cmp)

  io.out := out
}

object ALURTL extends App {
  implicit val p = new CoreConfig
  emitVerilog(new ScalarALU(new ALUFN), Array("--target-dir", "generated"))
}
