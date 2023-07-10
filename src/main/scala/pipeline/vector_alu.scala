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
import chisel3.stage.ChiselStage
import opengpgpu.config.parameters._

class VectorALU(hardThread: Int = HARD_THREAD) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VectorExeData(hardThread)))
    val out = DecoupledIO(new VectorData(hardThread))
    val thread_mask_out = DecoupledIO(new ThreadMask(hardThread))
  })

  val alu=VecInit(Seq.fill(hardThread)((Module(new ScalarALU())).io))

  val result = Module(new Queue(new VectorData(hardThread), 1, pipe = true))
  val result2simt = Module(new Queue(new ThreadMask(hardThread), 1, pipe = true))

  for (x <- 0 until hardThread) {
    alu(x).op1 := io.in.bits.op1(x)
    alu(x).op2 := io.in.bits.op2(x)
    alu(x).func := io.in.bits.func
    result.io.enq.bits.data(x) := alu(x).out
    result.io.enq.bits.mask(x) := io.in.bits.mask(x)
    result2simt.io.enq.bits.mask(x) := io.in.bits.mask(x) && alu(x).cmp_out
  }

  io.in.ready := result.io.enq.ready && result2simt.io.enq.ready
  result.io.enq.valid := io.in.valid
  result2simt.io.enq.valid := io.in.valid

  io.out <> result.io.deq
  io.thread_mask_out <> result2simt.io.deq
}

object VectorALURTL extends App {
  emitVerilog (new VectorALU(), Array("--target-dir", "generated"))
}

// object VectorALUGraph extends App {
//   (new ChiselStage).emitGraphML(new VectorALU() , Array("--target-dir", "graphs"))
// }
