import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import ogpu.core._

class PTWTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("PTW")

  it should "perform ptw operations correctly" in {

    val ptw_param = PTWParameter(paddrBits = 48, vaddrBits = 48)
    val cache_param = CacheParameter(paddrBits = 48, vaddrBits = 48)
    test(new PTW(1, ptw_param, cache_param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      println("ptw test start")
      dut.io.mem.req.ready.poke(true.B)
      dut.clock.step(5)
      dut.io.requestor(0).req.valid.poke(true.B)
      dut.io.requestor(0).req.bits.valid.poke(true.B)
      dut.io.requestor(0).req.bits.bits.addr.poke("x0012345678".U)
      dut.clock.step(1)
      dut.io.requestor(0).req.valid.poke(false.B)
      dut.clock.step(5)
      dut.io.mem.resp.valid.poke(true.B)
      dut.io.mem.resp.bits.data.poke("x8000ffc01".U) // level 0 ppn 0x2003ff
      dut.clock.step(1)
      dut.io.mem.resp.valid.poke(false.B)
      dut.clock.step(4)
      dut.io.mem.resp.valid.poke(true.B)
      dut.io.mem.resp.bits.data.poke("x1".U) // level 1
      dut.clock.step(1)
      dut.io.mem.resp.valid.poke(false.B)
      dut.clock.step(3)
      dut.io.mem.resp.valid.poke(true.B)
      dut.io.mem.resp.bits.data.poke("x40001".U) // level 2 ppn 0x100
      dut.clock.step(1)
      dut.io.mem.resp.valid.poke(false.B)
      while (dut.io.requestor(0).resp.valid.peek().litToBoolean == false) {
        dut.clock.step(1)
      }
      dut.io.requestor(0).resp.bits.pte.ppn.expect(0x100.U)
      dut.clock.step(5)
    }
  }
}
