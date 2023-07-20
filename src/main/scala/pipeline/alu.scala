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

/*
==== Supported Instructions ====
RV32I except:
ecall/ebreak
*/
import opengpgpu.config.parameters._
import opengpgpu.pipeline.ALUOps._
class ScalarALU() extends Module{
  val io = IO(new Bundle() {
    val func        = Input(UInt(5.W))
    val op1         = Input(UInt(xLen.W))
    val op2         = Input(UInt(xLen.W))
    val out         = Output(UInt(xLen.W))
    val cmp_out     = Output(Bool())
  })

  //ADD, SUB
  val op2_inv = Mux(isSub(io.func), ~io.op2, io.op2)
  val adder_out = io.op1 + op2_inv + isSub(io.func).asUInt

  //SLT, SLTU
  val op1_xor_op2 = io.op1 ^ op2_inv
  val slt = Mux(io.op1(xLen-1) === io.op2(xLen-1), adder_out(xLen-1),
                Mux(cmpUnsigned(io.func), io.op2(xLen-1), io.op1(xLen-1)))
  io.cmp_out := cmpInverted(io.func) ^ Mux(cmpEq(io.func), op1_xor_op2 === 0.U(xLen.W), slt)

  //SLL, SRL, SRA
  val (shamt, shin_r) = (io.op2(4,0), io.op1)
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
  val shin = Mux(io.func === FN_SR || io.func === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.func)&shin(xLen-1), shin).asSInt >> shamt)(xLen-1, 0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.func === FN_SR || io.func === FN_SRA, shout_r, 0.U(xLen.W)) |
    Mux(io.func === FN_SL,                       shout_l, 0.U(xLen.W))

  //AND, OR, XOR
  val logic = Mux(io.func === FN_XOR, io.op1 ^ io.op2,
    Mux(io.func === FN_OR, io.op1 | io.op2,
      Mux(io.func === FN_AND, io.op1 & io.op2, 0.U(xLen.W))))

  val shift_logic_cmp = (isCmp(io.func)&&slt) | logic | shout
  val out = Mux(io.func === FN_ADD || io.func === FN_SUB, adder_out, shift_logic_cmp)

  //MIN, MAX
  val minu=Mux(io.op1>io.op2,io.op2,io.op1)
  val maxu=Mux(io.op1>io.op2,io.op1,io.op2)
  val op1s=io.op1.asSInt
  val op2s=io.op2.asSInt
  val mins=Mux(op1s>op2s,op2s,op1s).asUInt
  val maxs=Mux(op1s>op2s,op1s,op2s).asUInt
  val minmaxout = Mux(io.func===FN_MIN,mins,
                  Mux(io.func===FN_MAX,maxs,
                  Mux(io.func===FN_MINU,minu,maxu) ) )

  io.out := Mux(io.func===FN_A1ZERO,io.op2,
            Mux(isMIN(io.func),minmaxout, out))
}


object ALURTL extends App {
  emitVerilog (new ScalarALU(), Array("--target-dir", "generated"))
}
