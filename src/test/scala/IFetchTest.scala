import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import ogpu.core._
import ogpu.config._

class IFetchTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("IFetch")

  it should "perform ifetch operations correctly" in {

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

    implicit val p = new OGPUDefaultConfig
    test(new InstFetch(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.to_icache.req.ready.poke(true.B)
      dut.io.inst_out.ready.poke(true.B)
      dut.io.to_ptw.ptbr.mode.poke(0x8.U)
      dut.io.to_ptw.req.ready.poke(true.B)
      dut.clock.step(5)
      dut.io.inst_fetch.valid.poke(true.B)
      dut.io.inst_fetch.bits.pc.poke(0x1024.U)
      dut.clock.step()
      dut.io.inst_fetch.valid.poke(false.B)
      dut.clock.step(5)
      dut.io.to_ptw.resp.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.to_ptw.resp.valid.poke(false.B)
      dut.clock.step(1)
      dut.io.to_icache.resp.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.to_icache.resp.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.to_icache.resp.valid.poke(false.B)

      dut.clock.step(20)

    }
  }
}
