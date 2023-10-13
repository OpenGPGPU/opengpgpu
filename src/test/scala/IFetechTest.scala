import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import chiseltest.simulator.WriteVcdAnnotation
import chisel3.util.experimental.loadMemoryFromFileInline
import org.chipsalliance.cde.config._

import org.scalatest.flatspec.AnyFlatSpec
import opengpgpu.config._
import opengpgpu.pipeline._
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._


class InstFetchTestTop(implicit p: Parameters) extends LazyModule {
  val numThread   = p(ThreadNum)
  val address = AddressSet(0x0, 0xffff)
  val ram = LazyModule(new AXI4RAM(address, false))
  val ifetch = LazyModule(new InstFetch())
  ram.node := AXI4Buffer() := ifetch.axi_master

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val inst_fetch    = Flipped(DecoupledIO(new InstFetchData()))
      val inst_out = DecoupledIO(new InstData())
    })
    ifetch.module.io.inst_fetch <> io.inst_fetch
    ifetch.module.io.inst_out <> io.inst_out
  }

}

// (implicit p: Parameters) 
class InstFetchTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "InstFetch"

  it should "perform inst fetch operations correctly" in {
    implicit val p = new CoreConfig 
    val ifetch = LazyModule(new InstFetchTestTop)
    test (ifetch.module).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step()
      dut.io.inst_fetch.valid.poke(1.U)
      dut.io.inst_fetch.bits.pc.poke(0x100.U)
      dut.io.inst_fetch.bits.mask(0).poke(1.U)
      dut.clock.step()
      dut.io.inst_fetch.bits.pc.poke(0x104.U)
      dut.io.inst_out.ready.poke(0.U)
      dut.clock.step()
      dut.io.inst_fetch.valid.poke(0.U)
      while(dut.io.inst_out.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }
      dut.io.inst_out.ready.poke(1.U)
      dut.io.inst_out.bits.pc.expect(0x100.U)

      dut.clock.step()
      while(dut.io.inst_out.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }
      dut.io.inst_out.bits.pc.expect(0x104.U)
      
    }
  }
}
