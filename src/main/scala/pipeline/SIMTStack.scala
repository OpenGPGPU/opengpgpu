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
import opengpgpu.libs._

class SIMTStack(implicit p: Parameters) extends Module {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val stackDepth = p(StackDepth)

  val io = IO(new Bundle {
    val in_diverge = Input(Bool())
    val in_data = Input(new StackData())
    val out_data = Output(new StackData())
    val push = Input(Bool())
    val pop = Input(Bool())
    val out_diverge = Output(Bool())
    val empty = Output(Bool())
    val full = Output(Bool())
  })

  val stack_addr = RegInit(0.U(log2Ceil(stackDepth + 1).W))
  val stack_pop_addr = RegInit(0.U(log2Ceil(stackDepth + 1).W))
  val out_diverge = RegInit(0.B)
  val out_data = Wire(new StackData())
  val diverge_status = RegInit(VecInit(Seq.fill(stackDepth)(false.B)))
  val stack_sram = Module(new ReadWriteSmem(io.in_data.getWidth, stackDepth))

  stack_pop_addr := stack_addr - 1.U
  stack_sram.io.enable := io.push || io.pop
  stack_sram.io.write := io.push
  stack_sram.io.addr := Mux(io.push, stack_addr, stack_pop_addr)
  stack_sram.io.dataIn := io.in_data.asUInt
  out_data := stack_sram.io.dataOut.asTypeOf(new StackData())

  when(io.push) {
    stack_addr := stack_addr + 1.U
    stack_pop_addr := stack_addr
  }.elsewhen(io.pop && ~diverge_status(stack_pop_addr)) {
    stack_addr := stack_addr - 1.U
    stack_pop_addr := stack_pop_addr - 1.U
  }

  when(io.push) {
    diverge_status(stack_addr) := io.in_diverge
  }.elsewhen(io.pop) {
    diverge_status(stack_pop_addr) := 0.B
    out_diverge := diverge_status(stack_pop_addr)
  }

  io.empty := stack_addr === 0.U
  io.full := stack_addr === stackDepth.U
  io.out_diverge := out_diverge
  io.out_data := out_data

}
