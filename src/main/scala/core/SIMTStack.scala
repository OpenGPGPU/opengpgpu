package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._
import ogpu.lib._

class SIMTStack(
  implicit p: Parameters)
    extends Module {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val stackDepth = p(StackDepth)

  val io = IO(new Bundle {
    val in_diverge = Input(Bool())
    val in_data = Input(new StackData())
    val out_data = Output(new StackData())
    val push = Input(Bool())
    val pop = Input(Bool())
    val out_diverge = Output(Bool())
    val empty = Output(Bool())
    val full = Output(Bool())
  })

  val stack_addr = RegInit(0.U(log2Ceil(stackDepth + 1).W))
  val stack_pop_addr = RegInit(0.U(log2Ceil(stackDepth + 1).W))
  val out_diverge = RegInit(0.B)
  val out_data = Wire(new StackData())
  val diverge_status = RegInit(VecInit(Seq.fill(stackDepth)(false.B)))
  val stack_sram = Module(new ReadWriteSmem(io.in_data.getWidth, stackDepth))

  stack_pop_addr := stack_addr - 1.U
  stack_sram.io.enable := io.push || io.pop
  stack_sram.io.write := io.push
  stack_sram.io.addr := Mux(io.push, stack_addr, stack_pop_addr)
  stack_sram.io.dataIn := io.in_data.asUInt
  out_data := stack_sram.io.dataOut.asTypeOf(new StackData())

  when(io.push) {
    stack_addr := stack_addr + 1.U
    stack_pop_addr := stack_addr
  }.elsewhen(io.pop && ~diverge_status(stack_pop_addr)) {
    stack_addr := stack_addr - 1.U
    stack_pop_addr := stack_pop_addr - 1.U
  }

  when(io.push) {
    diverge_status(stack_addr) := io.in_diverge
  }.elsewhen(io.pop) {
    diverge_status(stack_pop_addr) := 0.B
    out_diverge := diverge_status(stack_pop_addr)
  }

  io.empty := stack_addr === 0.U
  io.full := stack_addr === stackDepth.U
  io.out_diverge := out_diverge
  io.out_data := out_data

}
