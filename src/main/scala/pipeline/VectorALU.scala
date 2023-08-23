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
import circt.stage.ChiselStage
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._
import freechips.rocketchip.rocket._

class VectorALU(implicit p: Parameters) extends Module {
  val numThread = p(ThreadNum)
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ALUData()))
    val out = DecoupledIO(new CommitData())
    val thread_mask_out = DecoupledIO(new ThreadMask())
  })

  val alu = VecInit(Seq.fill(numThread)((Module(new ScalarALU(new ALUFN))).io))

  val result = Module(new Queue(new CommitData(), 1, pipe = true))
  val result2simt = Module(new Queue(new ThreadMask(), 1, pipe = true))

  for (x <- 0 until numThread) {
    alu(x).op1 := io.in.bits.op1(x)
    alu(x).op2 := io.in.bits.op2(x)
    alu(x).func := io.in.bits.func
    result.io.enq.bits.data(x) := alu(x).out
    result.io.enq.bits.mask(x) := io.in.bits.mask(x)
    result2simt.io.enq.bits.mask(x) := io.in.bits.mask(x) && alu(x).cmp_out
  }

  io.in.ready := result.io.enq.ready && result2simt.io.enq.ready
  result.io.enq.valid := io.in.valid
  result.io.enq.bits.wid := io.in.bits.wid
  result.io.enq.bits.pc := io.in.bits.pc
  result.io.enq.bits.rd := io.in.bits.rd
  result.io.enq.bits.eop := 1.B
  result2simt.io.enq.valid := io.in.valid

  io.out <> result.io.deq
  io.thread_mask_out <> result2simt.io.deq
}

object VectorALURTL extends App {
  implicit val p = new CoreConfig
  emitVerilog(new VectorALU(), Array("--target-dir", "generated"))
}

object VectorALUFIR extends App {
  // ChiselStage.emitFirrtl(new VectorALU())
  implicit val p = new CoreConfig
  ChiselStage.emitCHIRRTL(new VectorALU())
}

// object VectorALUGraph extends App {
//   (new ChiselStage).emitGraphML(new VectorALU() , Array("--target-dir", "graphs"))
// }
