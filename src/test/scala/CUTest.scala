import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import chiseltest.simulator.WriteVcdAnnotation
import chisel3.util.experimental.loadMemoryFromFile

import org.chipsalliance.cde.config._

import org.scalatest.flatspec.AnyFlatSpec
import opengpgpu.config._
import opengpgpu.pipeline._
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._

class CUTestTop(implicit p: Parameters) extends LazyModule {
  val numThread   = p(ThreadNum)
  val address = AddressSet(0x0, 0xffff)
  val ram = LazyModule(new AXI4RAM(address, false))
  val cu = LazyModule(new CU())
  ram.node := AXI4Buffer() := cu.xbar

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val in     = Flipped(DecoupledIO(new WarpCommandData()))
      val out = Decoupled(new WarpEndData)
    })

    cu.module.io.in <> io.in
    cu.module.io.out <> io.out
    loadMemoryFromFile(ram.module.mem, "src/test/data/cu/add_test/add.txt")

  }

}

class CUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CU"

  it should "perform cu operations correctly" in {
    implicit val p = new CoreConfig 
    val cu = LazyModule(new CUTestTop)
    test (cu.module).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step()
      dut.io.in.valid.poke(1.B)
      dut.io.in.bits.pc.poke(0.U)
      dut.io.in.bits.mask.map(_ -> 1.B)
      dut.clock.step()
      dut.io.in.valid.poke(0.B)
      while(dut.io.out.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }
      dut.clock.step(5)
    }
  }
}
