import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import ogpu.core._

class TLBTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("TLB")

  it should "perform tlb operations correctly" in {

    val tlb_param = TLBParameter(nSets = 32, nWays = 4, paddrBits = 48, vaddrBits = 48)
    test(new TLB(true, tlb_param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      println("tlb test start")
      dut.clock.step(5)
      dut.io.ptw.ptbr.mode.poke(0x8.U)
      dut.io.ptw.req.ready.poke(1.B)
      dut.io.req.bits.vaddr.poke(0x1024.U)
      dut.io.req.bits.passthrough.poke(true.B)
      dut.io.req.bits.size.poke(2.U)
      dut.io.req.valid.poke(true.B)
      while (dut.io.req.ready.peek().litToBoolean == false) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.paddr.expect(0x1024.U)
      println(s" tlb return miss? ${dut.io.resp.miss.peek().litToBoolean}")
      println(s" tlb return paddr ${dut.io.resp.paddr.peek()}")
      dut.clock.step(5)
      dut.io.req.bits.vaddr.poke(0x80000.U)
      dut.io.req.bits.passthrough.poke(false.B)
      dut.io.req.valid.poke(true.B)
      while (dut.io.req.ready.peek().litToBoolean == false) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.clock.step(10)
      dut.io.ptw.resp.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.ptw.resp.valid.poke(false.B)
      dut.clock.step(5)
      dut.io.req.bits.vaddr.poke(0x80008.U)
      dut.io.req.valid.poke(true.B)
      while (dut.io.req.ready.peek().litToBoolean == false) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.paddr.expect(0x8.U)
      dut.io.resp.miss.expect(false.B)
      dut.clock.step(5)
      dut.io.req.bits.vaddr.poke(0x180008.U) // cache conflict
      dut.io.req.valid.poke(true.B)
      while (dut.io.req.ready.peek().litToBoolean == false) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.clock.step(10)
      dut.io.ptw.resp.valid.poke(true.B)
      dut.io.ptw.resp.bits.pte.ppn.poke(0x3030.U)
      dut.clock.step(1)
      dut.io.ptw.resp.valid.poke(false.B)
      dut.clock.step(10)
    }
  }
}
