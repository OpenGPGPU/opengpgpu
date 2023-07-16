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

package opengpgpu.libs

import chisel3._
import chisel3.util._

// no arbiter, user should avoid conflict
class CrossbarIO[T <: Data](gen: T, nInputs: Int, nOutputs: Int) extends Bundle {
  val in = Vec(nInputs, Flipped(Decoupled(gen)))
  val out = Vec(nOutputs, Decoupled(gen))
  val sel = Input(Vec(nOutputs, UInt(log2Ceil(nInputs).W)))
}

class Crossbar[T <: Data](gen: T, nInputs: Int, nOutputs: Int) extends Module {
  val io = IO(new CrossbarIO(gen, nInputs, nOutputs))

  
  for (i <- 0 until nInputs) {
    io.in(i).ready := 0.B
  }

  for (i <- 0 until nOutputs) {
    val selectedInput = io.in(io.sel(i))
    io.out(i).bits := selectedInput.bits
    io.out(i).valid := selectedInput.valid
    selectedInput.ready := io.out(i).ready
  }
}

object CrossbarRTL extends App {
  emitVerilog (new Crossbar(UInt(8.W), 4, 2), Array("--target-dir", "generated"))
}
