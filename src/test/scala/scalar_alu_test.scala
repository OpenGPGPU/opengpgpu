import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._

import opengpgpu.pipeline._

class ScalarALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ScalarALU"

  it should "perform ADD operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_ADD)
      c.io.op1.poke(5.U)
      c.io.op2.poke(7.U)
      c.io.out.expect(12.U)
    }
  }

  it should "perform SUB operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_SUB)
      c.io.op1.poke(10.U)
      c.io.op2.poke(3.U)
      c.io.out.expect(7.U)
    }
  }

  it should "perform SLT operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_SLT)
      c.io.op1.poke(5.U)
      c.io.op2.poke(7.U)
      c.io.cmp_out.expect(true.B)
    }
  }


  it should "perform SLTU operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_SLTU)
      c.io.op1.poke(5.U)
      c.io.op2.poke(7.U)
      c.io.cmp_out.expect(true.B)
    }
  }

  it should "perform AND operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_AND)
      c.io.op1.poke(0x0F.U)
      c.io.op2.poke(0x33.U)
      c.io.out.expect(0x03.U)
    }
  }

  it should "perform OR operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_OR)
      c.io.op1.poke(0x0F.U)
      c.io.op2.poke(0x33.U)
      c.io.out.expect(0x3F.U)
    }
  }

  it should "perform XOR operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_XOR)
      c.io.op1.poke(0x0F.U)
      c.io.op2.poke(0x33.U)
      c.io.out.expect(0x3C.U)
    }
  }

  it should "perform SLL operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_SL)
      c.io.op1.poke(0x01.U)
      c.io.op2.poke(0x03.U)
      c.io.out.expect(0x08.U)
    }
  }

  it should "perform SRL operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_SR)
      c.io.op1.poke(0x10.U)
      c.io.op2.poke(0x02.U)
      c.io.out.expect(0x04.U)
    }
  }

  it should "perform SRA operation correctly" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_SRA)
      c.io.op1.poke(0x10.U)
      c.io.op2.poke(0x02.U)
      c.io.out.expect(0x04.U)
    }
  }

  it should "perform SLTU operation correctly with unsigned inputs" in {
    test(new ScalarALU()) { c =>
      c.io.func.poke(ALUOps.FN_SLTU)
      c.io.op1.poke(10.U)
      c.io.op2.poke(3.U)
      c.io.cmp_out.expect(false.B)
    }
  }

  // Add more test cases for other operations
}
