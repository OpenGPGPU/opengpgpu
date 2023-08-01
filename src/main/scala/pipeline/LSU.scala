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

class LSU(implicit p: Parameters) extends LazyModule {
  val numThread = p(ThreadNum)
  val axi_m_param = AXI4MasterParameters("myaximaster", IdRange(0, numThread))
  val axi_m_port = AXI4MasterPortParameters(Seq(axi_m_param))
  val axi_master = AXI4MasterNode(Seq(axi_m_port))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val in = Flipped(DecoupledIO(new LSUData()))
      val out_wb = DecoupledIO(new LSUData())
    })

    object State extends ChiselEnum {
      val IDLE, LOAD, STORE = Value
    }
    import State._

    val inQue = Module(new Queue(new LSUData(), 1, pipe = true))
    io.in <> inQue.io.enq

    val deq_ready = Wire(Bool())
    val deq_data = RegInit(0.U.asTypeOf(new LSUData()))
    val out_data = RegInit(0.U.asTypeOf(new LSUData()))
    val out_valid = RegInit(0.B)
    val deq_valid = inQue.io.deq.valid
    inQue.io.deq.ready := deq_ready

    val state = RegInit(State.IDLE)
    val mem_counter = RegInit(0.U((log2Ceil(numThread) + 1).W))
    val req_counter = RegInit(0.U((log2Ceil(numThread) + 1).W))
    val aw_req_counter = RegInit(0.U((log2Ceil(numThread) + 1).W))
    val rsp_counter = RegInit(0.U((log2Ceil(numThread) + 1).W))

    val (out, _) = axi_master.out(0)

    val load_finish = Wire(Bool())
    val store_finish = Wire(Bool())

    load_finish := state === LOAD && io.out_wb.ready && rsp_counter === mem_counter
    store_finish := state === STORE && rsp_counter === mem_counter
    deq_ready := state === IDLE || load_finish || store_finish
    when(inQue.io.deq.fire) {
      mem_counter := PopCount(inQue.io.deq.bits.mask)
    }

    when(load_finish) {
      out_data := deq_data
      out_valid := 1.B
    }.otherwise {
      out_valid := 0.B
    }

    io.out_wb.bits := out_data
    io.out_wb.valid := out_valid

    // state fsm
    switch(state) {
      is(State.IDLE) {
        when(deq_valid && inQue.io.deq.bits.func(0) === 0.U) {
          state := LOAD
        }.elsewhen(deq_valid && inQue.io.deq.bits.func(0) === 1.U) {
          state := STORE
        }
      }
      is(State.LOAD) {
        when(io.out_wb.ready && rsp_counter === mem_counter) {
          when(!deq_valid) {
            state := IDLE
          }.elsewhen(deq_valid && inQue.io.deq.bits.func(0) === 1.U) {
            state := STORE
          }
        }
      }
      is(State.STORE) {
        when(rsp_counter === mem_counter) {
          when(!deq_valid) {
            state := IDLE
          }.elsewhen(deq_valid && inQue.io.deq.bits.func(0) === 0.U) {
            state := LOAD
          }
        }
      }
    }

    // req_counter
    switch(state) {
      is(State.IDLE) {
        req_counter := 0.U
        aw_req_counter := 0.U
      }
      is(State.LOAD) {
        when(out.ar.fire) {
          req_counter := req_counter + 1.U

        }.elsewhen(io.out_wb.ready && rsp_counter === mem_counter) {
          req_counter := 0.U
        }
      }
      is(State.STORE) {
        when(out.w.fire) {
          req_counter := req_counter + 1.U
        }.elsewhen(rsp_counter === mem_counter) {
          req_counter := 0.U
        }

        when(out.aw.fire) {
          aw_req_counter := aw_req_counter + 1.U
        }.elsewhen(rsp_counter === mem_counter) {
          aw_req_counter := 0.U
        }
      }
    }

    // rsp counter
    switch(state) {
      is(State.IDLE) {
        rsp_counter := 0.U
      }
      is(State.LOAD) {
        when(out.r.fire) {
          rsp_counter := rsp_counter + 1.U
        }.elsewhen(io.out_wb.ready && rsp_counter === mem_counter) {
          rsp_counter := 0.U
        }
      }
      is(State.STORE) {
        when(out.b.valid) {
          rsp_counter := rsp_counter + 1.U
        }.elsewhen(rsp_counter === mem_counter) {
          rsp_counter := 0.U
        }
      }
    }

    val bit_loc = PriorityEncoder(deq_data.mask)

    // deq_data
    switch(state) {
      is(State.IDLE) {
        when(deq_valid) {
          deq_data := inQue.io.deq.bits
        }
      }
      is(State.LOAD) {
        when(out.r.fire) {
          deq_data.data(out.r.bits.id) := out.r.bits.data
        }.elsewhen(io.out_wb.ready && rsp_counter === mem_counter) {
          deq_data := inQue.io.deq.bits
        }
      }
      is(State.STORE) {
        when(rsp_counter === mem_counter) {
          deq_data := inQue.io.deq.bits
        }
      }
    }

    when(out.ar.fire || out.w.fire) {
      deq_data.mask(bit_loc) := 0.U
    }

    // axi req
    out.r.ready := 1.B
    out.b.ready := 1.B
    out.ar.valid := state === LOAD && req_counter < mem_counter
    out.ar.bits.id := bit_loc
    out.ar.bits.addr := deq_data.addr(bit_loc)
    out.ar.bits.len := 0.U
    out.ar.bits.size := 4.U
    out.ar.bits.burst := 0.U
    out.ar.bits.lock := 0.U
    out.ar.bits.cache := 0.U
    out.ar.bits.prot := 0.U
    out.ar.bits.qos := 0.U

    out.aw.valid := state === STORE && aw_req_counter === req_counter && aw_req_counter < mem_counter
    out.aw.bits.id := bit_loc
    out.aw.bits.addr := deq_data.addr(bit_loc)
    out.aw.bits.len := 0.U
    out.aw.bits.size := 4.U
    out.aw.bits.burst := 0.U
    out.aw.bits.lock := 0.U
    out.aw.bits.cache := 0.U
    out.aw.bits.prot := 0.U
    out.aw.bits.qos := 0.U

    out.w.valid := state === STORE && aw_req_counter > req_counter
    out.w.bits.data := deq_data.data(bit_loc)
    out.w.bits.strb := ((1 << (p(XLen) / 8)) - 1).U

  }
}
