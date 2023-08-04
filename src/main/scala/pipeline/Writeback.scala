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

class Writeback(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val alu_commit = Flipped(DecoupledIO(new CommitData()))
    val lsu_commit = Flipped(DecoupledIO(new CommitData()))
    val writeback = DecoupledIO(new CommitData())
  })

  val rsp_data = VecInit(
    Seq(
      io.alu_commit,
      io.lsu_commit
    )
  )

  val rsp_arbiter = Module(new RRArbiter(new CommitData(), 2))
  rsp_arbiter.io.in <> rsp_data

  val outQue = Module(new Queue(new CommitData(), 1, pipe = true))
  outQue.io.enq <> rsp_arbiter.io.out

  io.writeback <> outQue.io.deq
}
