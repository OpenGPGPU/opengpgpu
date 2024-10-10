package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class InstFetch(
  cfg: ICacheParams
)(
  implicit p: Parameters)
    extends Module {

  val io = IO(new Bundle {
    val inst_fetch = Flipped(Decoupled(new InstFetchData()))
    val to_icache = Flipped(new ICacheBundle(cfg))
    val to_ptw = new TLBPTWIO(cfg.vpnBits, cfg.vaddrBits, cfg.pgLevels)
    val inst_out = Decoupled(new InstData())
  })

  val tlb_param =
    TLBParameter(nSets = cfg.nTLBSets, nWays = cfg.nTLBWays, paddrBits = cfg.paddrBits, vaddrBits = cfg.vaddrBits)
  val tlb = Module(new TLB(true, tlb_param))

  // fetch inst fetch
  val fetch_idle :: fetch_req :: fetch_wait1 :: fetch_wait2 :: dispatch_wait :: Nil = Enum(5)
  val s_state = RegInit(fetch_idle)

  val ifetch_data = RegInit(0.U.asTypeOf(new InstFetchData))

  switch(s_state) {
    is(fetch_idle) {
      when(io.inst_fetch.fire) {
        s_state := fetch_req
        ifetch_data := io.inst_fetch.bits
      }
    }
    is(fetch_req) {
      when(io.to_icache.req.fire) {
        s_state := fetch_wait1
      }
    }
    is(fetch_wait1) {
      when(io.to_icache.resp.valid) {
        s_state := dispatch_wait
      }.otherwise {
        s_state := fetch_wait2
      }
    }
    is(fetch_wait2) {
      when(io.to_icache.resp.valid) {
        s_state := dispatch_wait
      }.otherwise {
        s_state := fetch_req
      }
    }
    is(dispatch_wait) {
      when(io.inst_out.fire) {
        s_state := fetch_idle
      }
    }
  }

  io.to_icache.req.valid := s_state === fetch_req
  io.to_icache.req.bits.addr := ifetch_data.pc
  io.to_icache.s1_paddr := tlb.io.resp.paddr
  io.to_icache.s1_kill := tlb.io.resp.miss
  io.to_icache.s2_kill := false.B
  io.to_icache.s2_cacheable := true.B
  io.to_icache.s2_prefetch := false.B
  io.to_icache.invalidate := false.B

  tlb.io.req.valid := (s_state === fetch_req)
  tlb.io.req.bits.vaddr := ifetch_data.pc
  tlb.io.req.bits.passthrough := false.B
  tlb.io.req.bits.size := 2.U
  tlb.io.req.bits.cmd := 0.U
  tlb.io.req.bits.prv := 0.U

  tlb.io.sfence := 0.U.asTypeOf(tlb.io.sfence)
  tlb.io.kill := false.B
  io.to_ptw <> tlb.io.ptw

  io.inst_fetch.ready := (s_state === fetch_idle)

  val cache_data = RegInit(0.U.asTypeOf(new ICacheResp(cfg.dataBits)))

  when(io.to_icache.resp.valid) {
    cache_data := io.to_icache.resp.bits
  }

  io.inst_out.valid := s_state === dispatch_wait
  io.inst_out.bits.pc := ifetch_data.pc
  io.inst_out.bits.mask := ifetch_data.mask
  io.inst_out.bits.wid := ifetch_data.wid
  io.inst_out.bits.data := (cache_data.data >> (ifetch_data.pc(5, 2) * 32.U))(31, 0)
}
