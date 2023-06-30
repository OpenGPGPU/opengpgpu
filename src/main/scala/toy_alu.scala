package opengpgpu.alu

import chisel3._
import chisel3.util._


class ALU extends Module {
  val io = IO(new Bundle {
    val op1 = Input(UInt(32.W))
    val op2 = Input(UInt(32.W))
    val aluOp = Input(UInt(4.W))
    val result = Output(UInt(32.W))
  })

  val result = Wire(UInt(32.W))

  // 加法
  when(io.aluOp === "b0000".U) {
    result := io.op1 + io.op2
  }
  // 减法
  .elsewhen(io.aluOp === "b0001".U) {
    result := io.op1 - io.op2
  }
  // 与运算
  .elsewhen(io.aluOp === "b0010".U) {
    result := io.op1 & io.op2
  }
  // 或运算
  .elsewhen(io.aluOp === "b0011".U) {
    result := io.op1 | io.op2
  }
  // 异或运算
  .elsewhen(io.aluOp === "b0100".U) {
    result := io.op1 ^ io.op2
  }
  // 左移
  .elsewhen(io.aluOp === "b0101".U) {
    result := io.op1 << io.op2(4, 0)
  }
  // 右移（算术右移）
  .elsewhen(io.aluOp === "b0110".U) {
    result := (io.op1.asSInt() >> io.op2(4, 0)).asUInt()
  }
  // 无符号右移
  .elsewhen(io.aluOp === "b0111".U) {
    result := io.op1 >> io.op2(4, 0)
  }
  // 等于
  .elsewhen(io.aluOp === "b1000".U) {
    result := io.op1 === io.op2
  }
  // 默认输出0
  .otherwise {
    result := 0.U
  }

  io.result := result
}


object ALURTL extends App {
  emitVerilog (new ALU(), Array("--target-dir", "generated"))
}


