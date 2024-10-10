package ogpu.util

import chisel3._
import chisel3.util._

object AddPipelineReg {

  class PipelineRegModule[T <: Data](gen: T) extends Module {

    val io = IO(new Bundle() {
      val in = Flipped(DecoupledIO(gen.cloneType))
      val out = DecoupledIO(gen.cloneType)
      val isFlush = Input(Bool())
    })

    val valid = RegInit(false.B)
    valid.suggestName("pipeline_reg_valid")
    when(io.out.fire) { valid := false.B }
    when(io.in.fire) { valid := true.B }
    when(io.isFlush) { valid := false.B }

    io.in.ready := !valid || io.out.ready
    io.out.bits := RegEnable(io.in.bits, io.in.fire)
    io.out.valid := valid // && !isFlush
  }

  def apply[T <: Data](
    left:       DecoupledIO[T],
    right:      DecoupledIO[T],
    isFlush:    Bool,
    moduleName: Option[String] = None
  ) {
    val pipelineReg = Module(new PipelineRegModule[T](left.bits.cloneType))
    if (moduleName.nonEmpty) pipelineReg.suggestName(moduleName.get)
    pipelineReg.io.in <> left
    right <> pipelineReg.io.out
    pipelineReg.io.isFlush := isFlush
  }

}
