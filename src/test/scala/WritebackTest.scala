import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import chiseltest.simulator.WriteVcdAnnotation

import org.chipsalliance.cde.config._

import org.scalatest.flatspec.AnyFlatSpec
import opengpgpu.config._
import opengpgpu.pipeline._

class WritebackTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Writeback"

  it should "write back correct data" in {
    implicit val p = new CoreConfig
    test(new Writeback) { c =>
      c.io.alu_commit.bits.wid.poke(2.U)
      c.io.lsu_commit.bits.wid.poke(3.U)
      c.io.alu_commit.valid.poke(true.B)
      c.io.lsu_commit.valid.poke(true.B)
      c.io.writeback.ready.poke(true.B)
      c.clock.step()

      // ???
      c.io.writeback.valid.expect(true.B)
      c.io.writeback.bits.wid.expect(3.U) 
      c.clock.step()
      c.io.writeback.valid.expect(true.B)
      c.io.writeback.bits.wid.expect(2.U) 
      c.clock.step()
      c.io.writeback.valid.expect(true.B)
      c.io.writeback.bits.wid.expect(3.U) 
      c.io.alu_commit.valid.poke(false.B)
      c.clock.step()
      c.io.alu_commit.valid.poke(true.B)
      c.io.writeback.valid.expect(true.B)
      c.io.writeback.bits.wid.expect(3.U) 
      c.clock.step()
      c.io.writeback.valid.expect(true.B)
      c.io.writeback.bits.wid.expect(2.U) 

    }
  }
}
