import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import ogpu.config._
import ogpu.core._

class WarpSchedulerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("WarpScheduler")

  it should "perform warp scheduler operations correctly" in {
    implicit val p = new OGPUDefaultConfig
    test(new WarpScheduler()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      println("warp sched test start")
      dut.io.warp_cmd.valid.poke(1.B)
      dut.io.warp_cmd.bits.mask(0).poke(1.B)
      dut.io.warp_cmd.bits.mask(3).poke(1.B)
      dut.io.warp_cmd.bits.vgpr_num.poke(2.U)
      dut.io.warp_cmd.bits.pc.poke(0x800000000L)
      dut.io.vgpr_commit.ready.poke(1.B)
      if (dut.io.warp_cmd.ready.peek().litToBoolean == false)
        println("warp cmd ready is false")
      while (dut.io.warp_cmd.ready.peek().litToBoolean == false) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.warp_cmd.valid.poke(0.B)
      dut.clock.step(5)
    }
  }
}
