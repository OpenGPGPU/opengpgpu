import chisel3._
import chiseltest._
import org.scalatest._

import org.chipsalliance.cde.config.Parameters

import org.scalatest.flatspec.AnyFlatSpec
import opengpgpu.config._
import opengpgpu.pipeline._
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._


// (implicit p: Parameters) 
class LSUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "LSU"

  it should "perform lsu axi rw operations correctly" in {
    implicit val p = new CoreConfig 
    val lsu = LazyModule(new LSU())
    val ram = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))
    ram.node := lsu.axi_master
    test (lsu.module) { dut =>
      dut.io.out_WB.ready.poke(1.U)
      dut.io.in.valid.poke(1.U)
      dut.io.in.bits.addr.poke(0.U)
      dut.io.in.bits.func.poke(1.U)
    }
  }
}
