import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import opengpgpu.config._
import opengpgpu.pipeline._


class GPRTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "GPR"

  it should "read GPR RAM correctly" in {
    implicit val p = new CoreConfig 
    test(new GPR) { dut =>
      dut.clock.step()

      // 验证读取零寄存器时输出为零
      dut.io.read_req.wid.poke(0.U)
      dut.io.read_req.rs1.poke(0.U)
      dut.io.read_req.rs2.poke(0.U)
      dut.clock.step()
      dut.io.read_rsp.rs1_data(0).expect(0.U)
      dut.io.read_rsp.rs2_data(0).expect(0.U)

      // 验证写入和读取非零数据
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.wid.poke(3.U)
      dut.io.writeback.bits.rd.poke(1.U)
      dut.io.writeback.bits.mask(12).poke(1.B)
      dut.io.writeback.bits.data(12).poke(14.U)
      dut.clock.step()
      dut.io.read_req.wid.poke(3.U)
      dut.io.read_req.rs1.poke(1.U)
      dut.io.read_req.rs2.poke(15.U)
      dut.clock.step()
      dut.io.read_rsp.rs1_data(12).expect(14.U)
      dut.io.read_rsp.rs1_data(23).expect(0.U)
      dut.io.read_rsp.rs2_data(16).expect(0.U)

      // write x0
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.wid.poke(2.U)
      dut.io.writeback.bits.rd.poke(0.U)
      dut.io.writeback.bits.mask(16).poke(1.B)
      dut.io.writeback.bits.data(16).poke(17.U)
      dut.clock.step()
      dut.io.read_req.wid.poke(2.U)
      dut.io.read_req.rs1.poke(0.U)
      dut.clock.step()
      dut.io.read_rsp.rs1_data(16).expect(0.U)
      
    }
  }
}
