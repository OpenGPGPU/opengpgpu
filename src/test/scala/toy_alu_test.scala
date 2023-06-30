package opengpgpu.alu

import chisel3._
import chiseltest._
import org.scalatest._

class Riscv32ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Riscv32ALU"

  it should "perform addition correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(10.U)
      dut.io.op2.poke(5.U)
      dut.io.aluOp.poke("b0000".U)
      dut.io.result.expect(15.U)
    }
  }

  it should "perform subtraction correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(10.U)
      dut.io.op2.poke(5.U)
      dut.io.aluOp.poke("b0001".U)
      dut.io.result.expect(5.U)
    }
  }

  it should "perform bitwise AND operation correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(7.U)
      dut.io.op2.poke(5.U)
      dut.io.aluOp.poke("b0010".U)
      dut.io.result.expect(5.U)
    }
  }

  it should "perform bitwise OR operation correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(7.U)
      dut.io.op2.poke(5.U)
      dut.io.aluOp.poke("b0011".U)
      dut.io.result.expect(7.U)
    }
  }

  it should "perform bitwise XOR operation correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(7.U)
      dut.io.op2.poke(5.U)
      dut.io.aluOp.poke("b0100".U)
      dut.io.result.expect(2.U)
    }
  }

  it should "perform left shift operation correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(5.U)
      dut.io.op2.poke(2.U)
      dut.io.aluOp.poke("b0101".U)
      dut.io.result.expect(20.U)
    }
  }

  it should "perform arithmetic right shift operation correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke("xffffffec".U)
      dut.io.op2.poke(2.U)
      dut.io.aluOp.poke("b0110".U)
      dut.io.result.expect("xfffffffb".U)
    }
  }

  it should "perform right shift operation correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(20.U)
      dut.io.op2.poke(2.U)
      dut.io.aluOp.poke("b0111".U)
      dut.io.result.expect(5.U)
    }
  }

  it should "perform equality comparison correctly" in {
    test(new ALU) { dut =>
      dut.io.op1.poke(5.U)
      dut.io.op2.poke(5.U)
      dut.io.aluOp.poke("b1000".U)
      dut.io.result.expect(1.U)
    }
  }
}
