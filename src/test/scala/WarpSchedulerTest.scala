import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import opengpgpu.config._
import opengpgpu.pipeline._


class WarpSchedulerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "WarpScheduler"

  it should "perform warp scheduler operations correctly" in {
    implicit val p = (new CoreConfig).alter((site, here, up) => {case WarpNum => 2})
    test(new WarpScheduler()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.io.warp_cmd.valid.poke(true.B)
      dut.io.warp_cmd.bits.pc.poke(0x100.U)
      dut.io.warp_cmd.bits.mask(0).poke(1.B)
      dut.io.warp_cmd.bits.mask(1).poke(1.B)
      dut.io.inst_fetch.ready.poke(true.B)
      dut.clock.step()
      dut.io.warp_cmd.bits.pc.poke(0x300.U)
      dut.io.inst_fetch.bits.pc.expect(0x100.U)
      dut.io.inst_fetch.bits.mask(0).expect(1.B)
      dut.clock.step()
      // all warps are occupied, check warp_cmd ready
      dut.io.warp_cmd.valid.poke(0.B)
      dut.io.inst_fetch.valid.expect(1.B)
      dut.io.warp_cmd.ready.expect(0.B)

      // warp is stalled after inst fetch
      dut.clock.step()
      dut.io.inst_fetch.valid.expect(0.B)

      // warp ctl restart a warp
      dut.io.warp_ctl.valid.poke(true.B)
      dut.io.warp_ctl.bits.wid.poke(0.U)
      dut.io.warp_ctl.bits.active.poke(1.B)

      dut.clock.step()
      dut.io.inst_fetch.valid.expect(1.B)
      dut.io.inst_fetch.bits.pc.expect(0x104.U)
      dut.io.inst_fetch.bits.mask(0).expect(1.B)
      dut.io.inst_fetch.bits.wid.expect(0.U)

      // warp ctl restart another warp
      dut.io.warp_ctl.bits.wid.poke(1.U)

      dut.clock.step()
      dut.io.warp_ctl.valid.poke(false.B)
      dut.io.inst_fetch.valid.expect(1.B)
      dut.io.inst_fetch.bits.pc.expect(0x304.U)
      dut.io.inst_fetch.bits.mask(0).expect(1.B)
      dut.io.inst_fetch.bits.wid.expect(1.U)

      // branch test
      dut.io.branch_ctl.valid.poke(true.B)
      dut.io.branch_ctl.bits.wid.poke(0.U)
      dut.io.branch_ctl.bits.diverge.poke(true.B)
      dut.io.branch_ctl.bits.pc.poke(0x108.U)
      dut.io.branch_ctl.bits.mask(1).poke(1.B)
      dut.io.branch_ctl.bits.data.pc.poke(0x200.U)
      dut.io.branch_ctl.bits.data.mask(0).poke(1.B)
      dut.io.branch_ctl.bits.data.orig_mask(0).poke(1.B)
      dut.io.branch_ctl.bits.data.orig_mask(1).poke(1.B)

      dut.clock.step()
      dut.io.branch_ctl.valid.poke(false.B)
      dut.io.inst_fetch.valid.expect(1.B)
      dut.io.inst_fetch.bits.pc.expect(0x108.U)
      dut.io.inst_fetch.bits.mask(1).expect(1.B)

      dut.clock.step()
      dut.io.inst_fetch.valid.expect(0.B)

      // assume the inst is join
      dut.io.warp_ctl.valid.poke(true.B)
      dut.io.warp_ctl.bits.wid.poke(0.U)
      dut.io.warp_ctl.bits.active.poke(0.B)
      dut.io.warp_ctl.bits.join.poke(true.B)

      dut.clock.step()
      dut.io.warp_ctl.valid.poke(false.B)
      dut.clock.step()
      dut.io.inst_fetch.valid.expect(1.B)
      dut.io.inst_fetch.bits.pc.expect(0x200.U) // join else to 0x200
      dut.io.inst_fetch.bits.mask(1).expect(0.B)

      // join again
      dut.clock.step()
      dut.io.warp_ctl.valid.poke(true.B)
      dut.io.warp_ctl.bits.wid.poke(0.U)
      dut.io.warp_ctl.bits.active.poke(0.B)
      dut.io.warp_ctl.bits.join.poke(true.B)

      dut.clock.step()
      dut.io.warp_ctl.valid.poke(false.B)
      dut.clock.step()
      dut.io.inst_fetch.valid.expect(1.B)
      dut.io.inst_fetch.bits.pc.expect(0x204.U) // join not change pc
      dut.io.inst_fetch.bits.mask(1).expect(1.B)
    }
  }
}
