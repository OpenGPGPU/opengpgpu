import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import opengpgpu.libs._

class CrossbarSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Crossbar"
  
  it should "route data based on sel signals" in {
    test(new Crossbar(UInt(8.W), 4, 2)) { c =>
      // 初始化输入
      for (i <- 0 until 4) {
        c.io.in(i).valid.poke(true.B)
        c.io.in(i).bits.poke(i.U)
      }

      // 输出处于ready状态
      for (i <- 0 until 2) {
        c.io.out(i).ready.poke(true.B)
      }

      // 设置选择信号
      c.io.sel(0).poke(1.U)
      c.io.sel(1).poke(2.U)

      // 执行一步时钟
      c.clock.step(1)


      // 检查输出
      c.io.out(0).valid.expect(true.B)
      c.io.out(0).bits.expect(1.U)
      c.io.out(1).valid.expect(true.B)
      c.io.out(1).bits.expect(2.U)

      c.io.in(0).ready.expect(false.B)
      c.io.in(3).ready.expect(false.B)
      c.io.in(1).ready.expect(true.B)

      // 执行一步时钟
      c.clock.step(1)

      for (i <- 0 until 4) {
        c.io.in(i).valid.poke(false.B)
        c.io.in(i).bits.poke(i.U)
      }

      c.io.out(0).valid.expect(false.B)
    }
  }
}
