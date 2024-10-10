import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import ogpu.core._
import ogpu.config._

class DCacheTestTop(
)(
  implicit p: Parameters)
    extends LazyModule {

  val cfg = CacheParameter(
    nSets = 64,
    nWays = 4,
    paddrBits = 48,
    vaddrBits = 48,
    pgIdxBits = 12,
    dataBits = 64,
    coreId = 0,
    tagECC = None,
    dataECC = None
  )

  val ram = LazyModule(new TLRAM(AddressSet(0x80000000L, 0xffffL), beatBytes = 8))
  val dcache = LazyModule(new DCache(cfg))
  ram.node :=*
    TLXbar() :=*
    TLFragmenter(8, 64) :=*
    TLCacheCork() :=*
    dcache.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val dcache = new CacheBundle(cfg)
    })
    dcache.module.io <> io.dcache
  }
}

class DCacheTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("DCacheTest")

  it should "perfrom dcache test correctly" in {
    implicit val p = new OGPUDefaultConfig
    val top = LazyModule(new DCacheTestTop())
    test(top.module).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(80)
      dut.io.dcache.ptw.ptbr.mode.poke(0x8.U)
      dut.io.dcache.ptw.req.ready.poke(true.B)
      dut.io.dcache.cpu.req.bits.addr.poke(0x1024000.U)
      dut.io.dcache.cpu.req.valid.poke(true.B)
      dut.clock.step()
      dut.io.dcache.cpu.req.valid.poke(false.B)
      // while (dut.io.dcache.cpu.resp.valid.peek().litToBoolean == false) {
      while (dut.io.dcache.cpu.s2_nack.peek().litToBoolean == false) {
        dut.clock.step()
      }
      dut.clock.step(5) // ptw resp must be delayed
      println("read dcache failed because of tlb miss")
      dut.io.dcache.ptw.resp.valid.poke(true.B)
      dut.io.dcache.ptw.resp.bits.pte.ppn.poke(0x80000.U)
      dut.clock.step()
      dut.io.dcache.ptw.resp.valid.poke(false.B)
      dut.clock.step(5)
      dut.io.dcache.cpu.req.bits.addr.poke(0x1024000.U) // request same addr again
      dut.io.dcache.cpu.req.valid.poke(true.B)
      dut.clock.step()
      dut.io.dcache.cpu.req.valid.poke(false.B)
      dut.clock.step(20)
      dut.io.dcache.cpu.req.bits.addr.poke(0x1024000.U) // request again, cache hit
      dut.io.dcache.cpu.req.valid.poke(true.B)
      dut.clock.step()
      dut.io.dcache.cpu.req.valid.poke(false.B)
      while (dut.io.dcache.cpu.resp.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }
      dut.clock.step(5)

    }
  }
}
