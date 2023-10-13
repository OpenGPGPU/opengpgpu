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
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._

class InstFetch(implicit p: Parameters) extends LazyModule {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val axi_m_param = AXI4MasterParameters("myaximaster", IdRange(0, 1))
  val axi_m_port = AXI4MasterPortParameters(Seq(axi_m_param))
  val axi_master = AXI4MasterNode(Seq(axi_m_port))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val inst_fetch = Flipped(DecoupledIO(new InstFetchData()))
      val inst_out = DecoupledIO(new InstData())
    })

    val (out, _) = axi_master.out(0)
    val inQue = Module(new Queue(new InstFetchData(), 1, pipe = true))
    io.inst_fetch <> inQue.io.enq

    val read_valid = RegInit(0.B)
    val arwait = RegInit(0.B)
    val read_inst = RegInit(0.U.asTypeOf(new InstData()))

    inQue.io.deq.ready := out.ar.ready && !arwait

    when(out.r.fire) {
      read_valid := 1.B
    }.elsewhen(io.inst_out.fire) {
      read_valid := 0.B
    }

    when(out.ar.fire) {
      read_inst.pc := inQue.io.deq.bits.pc
      read_inst.mask := inQue.io.deq.bits.mask
      arwait := 1.B
    }

    when(out.r.fire) {
      read_inst.data := out.r.bits.data
      arwait := 0.B
    }

    // output
    io.inst_out.valid := read_valid
    io.inst_out.bits := read_inst

    // axi req
    out.r.ready := !read_valid || io.inst_out.ready
    out.b.ready := 1.B
    out.ar.valid := inQue.io.deq.valid && !arwait
    out.ar.bits.id := 0.U
    out.ar.bits.addr := inQue.io.deq.bits.pc
    out.ar.bits.len := 0.U
    out.ar.bits.size := 4.U
    out.ar.bits.burst := 0.U
    out.ar.bits.lock := 0.U
    out.ar.bits.cache := 0.U
    out.ar.bits.prot := 0.U
    out.ar.bits.qos := 0.U

    out.aw.valid := 0.B
    out.aw.bits.id := 0.U
    out.aw.bits.addr := 0.U
    out.aw.bits.len := 0.U
    out.aw.bits.size := 4.U
    out.aw.bits.burst := 0.U
    out.aw.bits.lock := 0.U
    out.aw.bits.cache := 0.U
    out.aw.bits.prot := 0.U
    out.aw.bits.qos := 0.U

    out.w.valid := 0.B
    out.w.bits.data := 0.U
    out.w.bits.strb := 0.U

  }
}
