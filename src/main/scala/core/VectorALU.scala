package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._

class VectorALU(
  implicit p: Parameters)
    extends Module {
  val numThread = p(ThreadNum)
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VALUData()))
    val out = DecoupledIO(new CommitVData())
    val branch_data = DecoupledIO(new BranchData())
  })

  val alu = VecInit(Seq.fill(numThread)((Module(new ScalarALU()).io)))

  val result = Module(new Queue(new CommitVData(), 1, pipe = true))
  val branch_result = Module(new Queue(new BranchData(), 1, pipe = true))

  for (x <- 0 until numThread) {
    alu(x).in1 := io.in.bits.op1(x)
    alu(x).in2 := io.in.bits.op2(x)
    alu(x).fn := io.in.bits.func
    result.io.enq.bits.data(x) := alu(x).out
    result.io.enq.bits.mask(x) := io.in.bits.mask(x)
    branch_result.io.enq.bits.mask(x) := alu(x).cmp_out & io.in.bits.mask(x)
  }

  branch_result.io.enq.bits.branch := io.in.bits.branch
  branch_result.io.enq.bits.wid := io.in.bits.wid
  branch_result.io.enq.bits.pc := io.in.bits.pc
  branch_result.io.enq.bits.orig_mask := io.in.bits.mask
  branch_result.io.enq.bits.imm := io.in.bits.imm
  branch_result.io.enq.bits.rs1_data := io.in.bits.rs1_data

  io.in.ready := result.io.enq.ready && branch_result.io.enq.ready

  result.io.enq.valid := io.in.valid
  result.io.enq.bits.wid := io.in.bits.wid
  result.io.enq.bits.pc := io.in.bits.pc
  result.io.enq.bits.rd := io.in.bits.rd
  result.io.enq.bits.eop := 1.B

  val is_branch = io.in.bits.branch.jal | io.in.bits.branch.jalr | io.in.bits.branch.branch
  branch_result.io.enq.valid := io.in.valid && is_branch

  io.out <> result.io.deq
  io.branch_data <> branch_result.io.deq
}

// object VectorALURTL extends App {
//   implicit val p = new CoreConfig
//   emitVerilog(new VectorALU(), Array("--target-dir", "generated"))
// }
//
// object VectorALUFIR extends App {
//   // ChiselStage.emitFirrtl(new VectorALU())
//   implicit val p = new CoreConfig
//   ChiselStage.emitCHIRRTL(new VectorALU())
// }

// object VectorALUGraph extends App {
//   (new ChiselStage).emitGraphML(new VectorALU() , Array("--target-dir", "graphs"))
// }
