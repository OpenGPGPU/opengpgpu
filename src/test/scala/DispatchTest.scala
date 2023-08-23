import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import opengpgpu.pipeline._
import opengpgpu.config._
import freechips.rocketchip.rocket._


class DispatchTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Issue"
  it should "perform Dispatch operations correctly" in {
    implicit val p = new CoreConfig
    test(new Dispatch()) { dut =>
      dut.io.ibuffer.valid.poke(1.B)
      dut.io.ibuffer.bits.ex_type.poke(ExType.ALU)
      dut.io.alu.ready.poke(1.B)
      dut.io.lsu.ready.poke(1.B)
      dut.clock.step(1)
      dut.io.alu.valid.expect(1.B)
      dut.io.lsu.valid.expect(0.B)
    }
  }
}
