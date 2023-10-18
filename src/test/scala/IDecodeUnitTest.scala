import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import opengpgpu.config._
import opengpgpu.pipeline._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config._
import chiseltest.simulator.WriteVcdAnnotation

class IDecodeUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "IDecodeUnit"

  it should "generate correct output" in {
    implicit val p = new CoreConfig 
    test(new IDecodeUnit()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // addi x2 , x1,   2000
      var instData = 0x7d008113
      dut.io.inst.bits.data.poke(instData.U)
      dut.io.inst.valid.poke(true.B)

      dut.io.decode.ready.poke(true.B)
      dut.clock.step()

      val decodeData = dut.io.decode.bits
      decodeData.wid.expect(0.U)
      decodeData.mask(0).expect(0.B)
      decodeData.pc.expect(0.U)
      decodeData.ex_type.alu.expect(1.U)
      decodeData.mem_cmd.expect(0.U)
      decodeData.imm.expect(2000.U)
      decodeData.wb.expect(1.U)
      decodeData.rd.expect(2.U)
      decodeData.rs1.expect(1.U)

      // sub x10, x8, x1
      instData = 0x40140533
      dut.io.inst.bits.data.poke(instData.U)
      dut.clock.step()
      decodeData.ex_type.alu.expect(1.U)
      //decodeData.func.expect(10.U)
      // decodeData.use_imm.expect(false.B)
      decodeData.wb.expect(1.U)
      decodeData.rd.expect(10.U)

      // jal x15, -1024
      instData = 0x000007ef
      dut.io.inst.bits.data.poke(instData.U)
      dut.clock.step()
      decodeData.ex_type.alu.expect(1.U)
      //decodeData.func.expect(0.U)
      // decodeData.use_imm.expect(false.B)
      decodeData.wb.expect(1.U)
      decodeData.rd.expect(15.U)
      
      // lw x17, 16(x3)
      instData = 0x0101a883
      dut.io.inst.bits.data.poke(instData.U)
      dut.clock.step()
      decodeData.ex_type.lsu.expect(1.U)
      // decodeData.use_imm.expect(true.B)

    }
  }
}
