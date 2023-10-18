import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import opengpgpu.pipeline._
import opengpgpu.config._
import freechips.rocketchip.rocket._


class BranchUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "BranchUnit"

  it should "pass the branch control data" in {
    implicit val p = new CoreConfig
    val aluFn = p(ALUFunc)
    test(new BranchUnit) { c =>
      c.io.branch_data.valid.poke(true.B)
      c.io.branch_data.bits.pc.poke(0.U)
      c.io.branch_data.bits.imm.poke(4.U)
      c.io.branch_data.bits.rs1_data.poke(8.U)
      c.io.branch_data.bits.mask(1).poke(1.B)
      c.io.branch_data.bits.mask(3).poke(1.B)
      c.io.branch_data.bits.orig_mask(0).poke(1.B)
      c.io.branch_data.bits.orig_mask(1).poke(1.B)
      c.io.branch_data.bits.orig_mask(2).poke(1.B)
      c.io.branch_data.bits.orig_mask(3).poke(1.B)
      c.io.branch_data.bits.wid.poke(1.U)
      c.io.branch_data.bits.branch.jal.poke(1.B)
      c.clock.step()

      c.io.branch_ctl.valid.expect(true.B)
      c.io.branch_ctl.bits.pc.expect(4.U)
      c.io.branch_ctl.bits.mask(0).expect(1.B)
      c.io.branch_ctl.bits.mask(1).expect(1.B)
      c.io.branch_ctl.bits.mask(2).expect(1.B)
      c.io.branch_ctl.bits.mask(3).expect(1.B)
      c.io.branch_ctl.bits.wid.expect(1.U)
      c.io.branch_ctl.bits.diverge.expect(false.B)

      
    }
  }
}
