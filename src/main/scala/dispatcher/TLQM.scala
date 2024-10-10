package ogpu.dispatcher

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import chisel3.util.{is, switch, Enum}

case class QMParams(baseAddress: BigInt = 0x03000000) {
  def address = AddressSet(baseAddress, 0xff)
}

class TLQM(
  params: QMParams
)(
  implicit p: Parameters)
    extends LazyModule {

  val device = new SimpleDevice("qm", Seq("ogpu, qm")) {
    override val alwaysExtended = true
  }

  val node = TLRegisterNode(address = Seq(params.address), device = device, beatBytes = 8)

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(
      TLMasterParameters.v1(
        "tlqm master",
        sourceId = IdRange(0, 16)
      )
    )
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new Impl(this)
  class Impl(
    outer: TLQM
  )(
    implicit p: Parameters)
      extends LazyModuleImp(outer) {
    val io = IO(new Bundle {
      // val tlb = new TlbRequestIO(1)
    })

    // io.tlb.req_kill := false.B

    // val (tl_out, edge_out) = outer.clientNode.out(0)
    // val base_addr = RegInit(0.U(64.W))
    // val rptr = RegInit(0.U(64.W))
    // val wptr = RegInit(0.U(64.W))
    // val size = RegInit(0.U(64.W))
    // val enable = RegInit(0.B)
    // val data = RegInit(0.U(512.W))

    // val pending = WireInit(rptr =/= wptr)

    // // step1 issue tlb request, update rptr
    // val s1_idle :: s1_req :: s1_ack :: Nil = Enum(3)

    // val s1_state = RegInit(s1_idle)
    // val s1_rptr = RegInit(0.U(64.W))

    // // s1 state transition
    // switch(s1_state) {
    //   is(s1_idle) {
    //     when(pending & enable) {
    //       s1_state := s1_req
    //     }
    //   }
    //   is(s1_req) {
    //     when(io.tlb.req.fire) {
    //       s1_state := s1_ack
    //     }
    //   }
    //   is(s1_ack) {
    //     when(io.tlb.resp.fire) {
    //       s1_state := s1_idle
    //     }
    //   }
    // }

    // // s1 state action
    // switch(s1_state) {
    //   is(s1_idle) {
    //     when(pending & enable) {
    //       io.tlb.req.valid := true.B
    //       io.tlb.req.bits.vaddr := base_addr + (rptr % size)
    //     }.otherwise {
    //       io.tlb.req.valid := false.B
    //     }
    //   }
    //   is(s1_req) {
    //     when(io.tlb.req.fire) {
    //       io.tlb.req.valid := false.B
    //     }
    //   }
    //   is(s1_ack) {
    //     when(io.tlb.resp.fire) {
    //       rptr := rptr + 8.U
    //     }
    //   }
    // }

    // // s2 get paddr and read aql package
    // val s2_idle :: s2_req :: s2_ack :: Nil = Enum(3)
    // io.tlb.resp.ready := s2_state === s2_idle
    // val s2_state = RegInit(s2_idle)

    // // s2 state transition
    // switch(s2_state) {
    //   is(s2_idle) {
    //     when(io.tlb.resp.fire & enable) {
    //       s2_state := s2_req
    //     }
    //   }
    //   is(s2_req) {
    //     when(tl_out.a.fire) {
    //       s2_state := s2_ack
    //     }
    //   }
    //   is(s2_ack) {
    //     when(tl_out.d.fire) {
    //       s2_state := s2_idle
    //     }
    //   }
    // }

    // // s2 state action
    // switch(s2_state) {
    //   is(s2_idle) {
    //     when(io.tlb.resp.fire) {
    //       tl_out.a.valid := 1.B
    //       tl_out.a.bits.address := 0.U
    //       tl_out.a.bits.opcode := 0.U
    //       tl_out.a.bits.size := 0.U
    //       tl_out.a.bits.data := 0.U
    //       tl_out.a.bits.mask := 0.U
    //     }
    //   }
    //   is(s2_req) {
    //     when(tl_out.a.fire) {
    //       tl_out.a.valid := false.B
    //     }
    //   }
    //   is(s2_ack) {
    //     when(tl_out.d.fire) {
    //       data := tl_out.d.bits.data
    //     }
    //   }
    // }

    // // s3 disptach aql data
    // val s3_idle :: s3_req :: s3_ack :: Nil = Enum(3)
    // val s3_state = RegInit(s3_idle)
    // tl_out.d.ready := s3_state === s3_idle

    // // ringbuffer base address
    // // ringbuffer rptr
    // // ringbuffer wptr, doorbell register
    // // ringbuffer size
    // // queue enable
    // node.regmap(
    //   0 -> Seq(RegField(64, base_addr, RegFieldDesc("base", "queue ring buffer base address", reset = Some(0)))),
    //   8 -> Seq(RegField(64, rptr, RegFieldDesc("rptr", "queue ring buffer read offset address", reset = Some(0)))),
    //   16 -> Seq(
    //     RegField(64, wptr, RegFieldDesc("wptr", "queue ring buffer write offset", reset = Some(0)))
    //   ),
    //   24 -> Seq(RegField(64, size, RegFieldDesc("size", "queue ring buffer size address", reset = Some(0)))),
    //   32 -> Seq(RegField(1, enable, RegFieldDesc("enable", "queue enable", reset = Some(0))))
    // )
  }
}
