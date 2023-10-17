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
    val branch_ctl = DecoupledIO(new BranchData())
  })

  val alu = VecInit(Seq.fill(numThread)((Module(new ScalarALU()).io)))

  val result = Module(new Queue(new CommitData(), 1, pipe = true))
  val branch_result = Module(new Queue(new BranchData(), 1, pipe = true))

  for (x <- 0 until numThread) {
    alu(x).in1 := io.in.bits.op1(x)
    alu(x).in2 := io.in.bits.op2(x)
    alu(x).fn := io.in.bits.func
    result.io.enq.bits.data(x) := alu(x).out
    result.io.enq.bits.mask(x) := io.in.bits.mask(x)
    branch_result.io.enq.bits.mask(x) := alu(x).cmp_out
  }

  branch_result.io.enq.bits.branch := io.in.bits.branch
  branch_result.io.enq.bits.wid := io.in.bits.wid
  branch_result.io.enq.bits.pc := io.in.bits.pc
  branch_result.io.enq.bits.orig_mask := io.in.bits.mask
  branch_result.io.enq.bits.imm := io.in.bits.imm
  branch_result.io.enq.bits.rs1_data := io.in.bits.rs1_data

  io.in.ready := result.io.enq.ready && branch_result.io.enq.ready

  result.io.enq.valid := io.in.valid
  result.io.enq.bits.wid := io.in.bits.wid
  result.io.enq.bits.pc := io.in.bits.pc
  result.io.enq.bits.rd := io.in.bits.rd
  result.io.enq.bits.eop := 1.B
  branch_result.io.enq.valid := io.in.valid

  io.out <> result.io.deq
  io.branch_ctl <> branch_result.io.deq
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
