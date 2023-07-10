import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._

import opengpgpu.pipeline._
import opengpgpu.config.parameters._

class VectorALUTest extends FlatSpec with ChiselScalatestTester {
  behavior of "VectorALU"

  it should "perform ALU operations correctly" in {
    test(new VectorALU()) { c =>
      c.io.in.valid.poke(1.U)
      c.io.in.bits.op1.map(x => x.poke(1.U))
      c.io.in.bits.op2.map(x => x.poke(1.U))
      c.io.in.bits.func.poke(ALUOps.FN_ADD)
      c.io.in.bits.mask.map(x => x.poke(1.B))
      c.io.out.ready.poke(1.U)
      c.io.thread_mask_out.ready.poke(1.U)
      c.clock.step()
      c.io.out.bits.data(0).expect(2.U)
      c.io.in.bits.mask(0).poke(0.B)
      c.io.in.bits.mask(21).poke(0.B)
      c.clock.step()
      c.io.out.bits.mask(0).expect(0.U)
      c.io.out.bits.mask(6).expect(1.U)
      c.io.out.bits.mask(21).expect(0.U)
      c.io.thread_mask_out.bits.mask(21).expect(0.U)
      c.io.thread_mask_out.bits.mask(0).expect(0.U)
      c.io.thread_mask_out.bits.mask(1).expect(1.U)

      c.io.out.ready.poke(0.U)
      c.io.in.bits.op1.map(x => x.poke(4.U))
      c.io.in.bits.op2.map(x => x.poke(2.U))

      c.clock.step()
      c.clock.step()
      c.io.in.ready.expect(0.B)
      c.io.out.bits.data(15).expect(2.U)
      c.io.out.ready.poke(1.U)
      c.clock.step()
      c.io.out.bits.data(15).expect(6.U)
      c.io.out.valid.expect(1.U)

      c.io.in.valid.poke(0.U)
      c.clock.step()
      c.io.out.valid.expect(0.U)
    }
  }
}
