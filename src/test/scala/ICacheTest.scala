import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import ogpu.core._
import ogpu.config._

class ICacheTestTop(
)(
  implicit p: Parameters)
    extends LazyModule {

  val cfg = ICacheParams(
    nSets = 64,
    nWays = 4,
    paddrBits = 48,
    vaddrBits = 48,
    pgIdxBits = 48,
    dataBits = 64,
    coreId = 0,
    tagECC = None,
    dataECC = None
  )

  val ram = LazyModule(new TLRAM(AddressSet(0x80000000L, 0xffffL), beatBytes = 8))
  val icache = LazyModule(new ICache(cfg))
  ram.node :=*
    TLXbar() :=*
    TLFragmenter(8, 64) :=*
    TLCacheCork() :=*
    icache.masterNode

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val icache = new ICacheBundle(cfg)
    })
    icache.module.io <> io.icache
  }
}

class ICacheTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("ICacheTest")

  it should "perfrom icache test correctly" in {
    implicit val p = new OGPUDefaultConfig
    val top = LazyModule(new ICacheTestTop())
    test(top.module).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step()
      dut.io.icache.req.valid.poke(true.B)
      dut.io.icache.req.bits.addr.poke(0x1000.U)
      dut.clock.step()
      dut.io.icache.req.valid.poke(false.B)
      dut.io.icache.s1_paddr.poke(0x80002000L.U)
      dut.clock.step(10)
      // cache miss and request again
      dut.io.icache.req.valid.poke(true.B)
      dut.io.icache.req.bits.addr.poke(0x1000.U)
      dut.clock.step()
      dut.io.icache.req.valid.poke(false.B)
      dut.io.icache.s1_paddr.poke(0x80002000L.U)
      while (dut.io.icache.resp.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }
      dut.clock.step(5)
      // request offset
      dut.io.icache.req.valid.poke(true.B)
      dut.io.icache.req.bits.addr.poke(0x1008.U)
      dut.clock.step()
      dut.io.icache.req.valid.poke(false.B)
      dut.io.icache.s1_paddr.poke(0x80002008L.U)
      // hit again
      while (dut.io.icache.resp.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }
      dut.clock.step(5)
    }
  }
}
