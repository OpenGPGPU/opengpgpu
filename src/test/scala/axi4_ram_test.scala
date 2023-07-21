import chisel3._
import chiseltest._
import org.scalatest._

import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system._

import org.scalatest.flatspec.AnyFlatSpec


class AXI4SlaveRAM(implicit p: Parameters) extends LazyModule {
  val ram = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))
  val axi_m_param = AXI4MasterParameters("myaximaster")
  val axi_m_port = AXI4MasterPortParameters(Seq(axi_m_param))
  val axi_master = AXI4MasterNode(Seq(axi_m_port))
  val ios = InModuleBody(axi_master.makeIOs())

  ram.node := AXI4Buffer() := axi_master

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = ios.head
  }

}

// (implicit p: Parameters) 
class AXI4RAMTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AXI4RAM"

  it should "perform axi rw operations correctly" in {
    implicit val p = new BaseConfig
    val axiram = LazyModule(new AXI4SlaveRAM())
    // val mymod = Module(axiram.module)
    test (axiram.module) { dut =>
      // Write data to the axi4ram module
      dut.io.aw.valid.poke(true.B)
      dut.io.aw.bits.addr.poke(0x00000000L.U)
      dut.io.aw.bits.len.poke(0.U)
      dut.io.aw.bits.size.poke(2.U)
      dut.io.w.valid.poke(true.B)
      dut.io.w.bits.data.poke(0xABCD.U)
      dut.io.w.bits.strb.poke("b1111".U)
      dut.clock.step()
      println(dut.io.aw.bits.id.getClass) // .getSimpleName)
      println(dut.io.aw.bits.id.getWidth)
      // Wait for write transaction to finish
      while(dut.io.b.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }

      dut.io.aw.valid.poke(false.B)
      dut.io.w.valid.poke(false.B)
      dut.io.ar.valid.poke(true.B)
      dut.io.ar.bits.addr.poke(0x00000000L.U)
      dut.io.ar.bits.len.poke(0.U)
      dut.io.ar.bits.size.poke(2.U)
      dut.clock.step()

      // Wait for read transaction to finish
      while(dut.io.r.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }

      // Read data from the axi4ram module
      dut.io.r.bits.data.expect(0xABCD.U)
    }
  }
}
