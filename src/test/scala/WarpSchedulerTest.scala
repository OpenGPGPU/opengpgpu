import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import opengpgpu.config._
import opengpgpu.pipeline._


class WarpSchedulerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "WarpScheduler"

  it should "perform warp scheduler operations correctly" in {
    implicit val p = new CoreConfig
    test(new WarpScheduler()) { dut =>
      // 设置输入信号
      dut.io.warp_cmd.valid.poke(true.B)
      dut.io.warp_cmd.bits.pc.poke(0x100.U)
      dut.io.warp_cmd.bits.mask(0).poke(1.B)
      dut.io.inst_fetch.ready.poke(true.B)
      dut.clock.step()
      dut.io.warp_cmd.valid.poke(false.B)
      dut.io.inst_fetch.bits.pc.expect(0x100.U)
      dut.io.inst_fetch.bits.mask(0).expect(1.B)
      dut.clock.step()
      dut.io.inst_fetch.valid.expect(0.B)
    }
  }
}