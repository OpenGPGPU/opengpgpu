import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import opengpgpu.pipeline._
import opengpgpu.config._
import freechips.rocketchip.rocket._


class IssueTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Issue"
  it should "perform Issue operations correctly" in {
    implicit val p = new CoreConfig
    test(new Issue()) { dut =>
      // issue
      dut.clock.step()
      dut.io.alu.ready.poke(1.U)
      dut.io.lsu.ready.poke(1.U)
      dut.io.decode.bits.rd.poke(5.U)
      dut.io.decode.valid.poke(1.U)
      dut.io.decode.bits.sel_alu1.poke(1.U)
      dut.io.decode.bits.ex_type.alu.poke(1.B)
      dut.clock.step()
      dut.io.decode.valid.poke(0.U)
      dut.clock.step()
      dut.io.alu.valid.expect(1.U)
      // writeback
      dut.clock.step()
      dut.io.alu.valid.expect(0.U)
      dut.io.writeback.valid.poke(1.U)
      dut.io.writeback.bits.rd.poke(5.U)
      dut.io.writeback.bits.mask(3).poke(1.U)
      dut.io.writeback.bits.data(3).poke(123.U)
      // issue again, check rs data
      dut.clock.step()
      dut.io.writeback.valid.poke(0.U)
      dut.io.decode.bits.rs1.poke(5.U)
      dut.io.decode.valid.poke(1.U)
      dut.clock.step()
      dut.io.decode.valid.poke(0.U)
      dut.clock.step()
      dut.io.alu.valid.expect(1.U)
      dut.io.alu.bits.op1(3).expect(123.U) 
      dut.io.alu.bits.op2(3).expect(0.U) 
      dut.io.alu.bits.op1(14).expect(0.U) 
    }
  }
}
