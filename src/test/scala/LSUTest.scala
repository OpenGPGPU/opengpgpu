import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import chiseltest.simulator.WriteVcdAnnotation

import org.chipsalliance.cde.config._

import org.scalatest.flatspec.AnyFlatSpec
import opengpgpu.config._
import opengpgpu.pipeline._
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._

class MyConfig extends Config(
  new BaseConfig ++
  new CoreConfig
)

class LSUTestTop(implicit p: Parameters) extends LazyModule {
  val numThread   = p(ThreadNum)
  val address = AddressSet(0x0, 0xffff)
  val ram = LazyModule(new AXI4RAM(address, false))
  val lsu = LazyModule(new LSU())
  ram.node := AXI4Buffer() := lsu.axi_master

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val in     = Flipped(DecoupledIO(new LSUData()))
      val out_wb = DecoupledIO(new LSUData())
    })
    lsu.module.io.in <> io.in
    lsu.module.io.out_wb <> io.out_wb
  }

}

// (implicit p: Parameters) 
class LSUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "LSU"

  it should "perform lsu axi rw operations correctly" in {
    implicit val p = new MyConfig 
    val lsu = LazyModule(new LSUTestTop)
    test (lsu.module).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.out_wb.ready.poke(1.U)
      dut.io.in.valid.poke(1.U)
      dut.io.in.bits.data(1).poke(1.U)
      dut.io.in.bits.data(3).poke(3.U)
      dut.io.in.bits.mask(1).poke(1.U)
      dut.io.in.bits.mask(3).poke(1.U)
      dut.io.in.bits.addr(1).poke(4.U)
      dut.io.in.bits.addr(3).poke(12.U)
      dut.io.in.bits.func.poke(1.U)
      dut.clock.step()
      dut.io.in.valid.poke(1.U)
      dut.io.in.bits.data(4).poke(4.U)
      dut.io.in.bits.data(6).poke(6.U)
      dut.io.in.bits.mask(4).poke(1.U)
      dut.io.in.bits.mask(6).poke(1.U)
      dut.io.in.bits.addr(4).poke(16.U)
      dut.io.in.bits.addr(6).poke(24.U)
      dut.clock.step()
      dut.io.in.valid.poke(0.U)

      dut.clock.step(30)

      dut.clock.step()
      dut.io.in.bits.data.map(x => x.poke(7.U))
      dut.io.in.valid.poke(1.U)
      dut.io.in.bits.func.poke(0.U)
      dut.clock.step()
      while(dut.io.out_wb.valid.peek().litToBoolean == false) {
        dut.clock.step()
      }

      dut.io.out_wb.bits.data(1).expect(1.U)
      dut.io.out_wb.bits.data(3).expect(3.U)
      dut.io.out_wb.bits.data(4).expect(4.U)
      dut.io.out_wb.bits.data(6).expect(6.U)
    }
  }
}
