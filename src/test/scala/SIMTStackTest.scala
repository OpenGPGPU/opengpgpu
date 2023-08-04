import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import opengpgpu.config._
import opengpgpu.pipeline._
class SIMTStackTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SIMTStack"

  it should "perform push and pop operations correctly" in {
    implicit val p = new CoreConfig 
    test(new SIMTStack()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // 设置输入信号的值
      dut.io.in_diverge.poke(false.B)
      dut.io.in_data.pc.poke(10.U)
      dut.io.in_data.mask(0).poke(1.B)
      dut.io.in_data.mask(1).poke(1.B)
      dut.io.in_data.mask(2).poke(1.B)
      dut.io.in_data.orig_mask(0).poke(1.B)
      dut.io.in_data.orig_mask(1).poke(1.B)
      dut.io.in_data.orig_mask(2).poke(1.B)
      dut.io.push.poke(true.B)
      dut.io.pop.poke(false.B)

      dut.clock.step()

      // 验证堆栈是否为空
      dut.io.empty.expect(false.B)
      // 验证堆栈是否已满
      dut.io.full.expect(false.B)

      // 进行一系列的推入和弹出操作
      dut.io.in_data.pc.poke(20.U)
      dut.io.in_data.mask(2).poke(0.B)
      dut.io.in_diverge.poke(true.B)
      dut.io.push.poke(true.B)
      dut.io.pop.poke(false.B)

      dut.clock.step()
      dut.io.push.poke(false.B)
      dut.clock.step(5)

      // pop 1
      dut.io.push.poke(false.B)
      dut.io.pop.poke(true.B)

      dut.clock.step()
      // pop 2
      // 验证堆栈是否为空
      dut.io.empty.expect(false.B)
      // 验证堆栈是否已满
      dut.io.full.expect(false.B)
      // 验证输出信号的值
      dut.io.out_data.pc.expect(20.U)
      dut.io.out_data.mask(2).expect(0.B)
      dut.io.out_diverge.expect(true.B)

      dut.clock.step()
      // pop 3
      dut.io.out_data.orig_mask(2).expect(1.B)
      dut.io.out_diverge.expect(false.B)

      dut.clock.step()
      dut.io.out_diverge.expect(false.B)
      dut.io.out_data.mask(2).expect(1.B)
      dut.io.out_data.orig_mask(1).expect(1.B)
      dut.io.empty.expect(true.B)
      dut.io.out_data.pc.expect(10.U)
    }
  }
}
