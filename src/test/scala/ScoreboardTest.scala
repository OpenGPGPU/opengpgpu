import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import chiseltest.simulator.WriteVcdAnnotation

import org.chipsalliance.cde.config._

import org.scalatest.flatspec.AnyFlatSpec
import opengpgpu.config._
import opengpgpu.pipeline._

class ScoreBoardTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ScoreBoard"
  
  it should "reserve and release registers correctly" in {
    implicit val p = new CoreConfig 
    test(new ScoreBoard()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      
      c.clock.step(3)
      // Test case 1: Reserve a register
      c.io.ibuffer.valid.poke(1.B)
      c.io.ibuffer.bits.writeback.poke(1.B)
      c.io.ibuffer.bits.wid.poke(4.U)
      c.io.ibuffer.bits.rd.poke(5.U)
      c.io.ibuffer.ready.expect(1.B)
      
      c.clock.step(1) // One clock cycle
      c.io.ibuffer.ready.expect(0.B)

      // Test case 2: Reserve the same register
      c.io.ibuffer.valid.poke(1.B)
      c.io.ibuffer.bits.writeback.poke(1.B)
      c.io.ibuffer.bits.wid.poke(4.U)
      c.io.ibuffer.bits.rd.poke(5.U)
      
      c.clock.step(1) // One clock cycle
      c.io.ibuffer.ready.expect(0.B)
      
      
      // Test case 3: Release a register
      c.io.writeback.valid.poke(1.B)
      c.io.writeback.bits.eop.poke(1.B)
      c.io.writeback.bits.wid.poke(4.U)
      c.io.writeback.bits.rd.poke(5.U)
      
      c.clock.step(1) // One clock cycle
      c.io.writeback.valid.poke(0.B)
      
      // Check if the register is released
      c.io.ibuffer.ready.expect(1.B)
      c.clock.step(1) // One clock cycle
      c.io.ibuffer.ready.expect(0.B)
      
    }
  }
}
