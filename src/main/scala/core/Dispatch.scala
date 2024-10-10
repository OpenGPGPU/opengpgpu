package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._
import freechips.rocketchip.rocket._

class Dispatch(
  implicit p: Parameters)
    extends Module {
  val numThread = p(ThreadNum)
  val xLen = p(XLen)
  val io = IO(new Bundle {
    val ibuffer = Flipped(DecoupledIO(new DecodeData()))
    val vgpr_rsp = Flipped(new ReadVGPRRsp())

    val alu = DecoupledIO(new VALUData())
    val lsu = DecoupledIO(new LSUData())
  })

  val buffer = Module(
    new Queue(
      new Bundle {
        val decode = new DecodeData
        val vgpr_rsp = new ReadVGPRRsp()
      },
      1,
      pipe = true
    )
  )

  val buffer_deq = buffer.io.deq.bits
  buffer.io.enq.valid := io.ibuffer.valid
  buffer.io.enq.bits.decode := io.ibuffer.bits
  buffer.io.enq.bits.vgpr_rsp := io.vgpr_rsp
  io.ibuffer.ready := buffer.io.enq.ready

  val ex_op1 = Wire(Vec(numThread, UInt(xLen.W)))
  val ex_op2 = Wire(Vec(numThread, UInt(xLen.W)))
  val pc_vec = Wire(Vec(numThread, UInt(xLen.W)))
  val imm_vec = Wire(Vec(numThread, UInt(xLen.W)))
  val const_vec = Wire(Vec(numThread, UInt(xLen.W)))

  pc_vec := VecInit.tabulate(numThread) { _ => buffer_deq.decode.pc }
  imm_vec := VecInit.tabulate(numThread) { _ => buffer_deq.decode.imm }
  const_vec := VecInit.tabulate(numThread) { _ => 4.U }

  ex_op1 := MuxLookup(buffer_deq.decode.sel_alu1.asUInt, 0.U.asTypeOf(ex_op1))(
    Seq(A1_RS1.asUInt -> buffer_deq.vgpr_rsp.rs1_data, A1_PC.asUInt -> pc_vec)
  )
  ex_op2 := MuxLookup(buffer_deq.decode.sel_alu2.asUInt, 0.U.asTypeOf(ex_op1))(
    Seq(A2_RS2.asUInt -> buffer_deq.vgpr_rsp.rs2_data, A2_IMM.asUInt -> imm_vec, A2_SIZE.asUInt -> const_vec)
  )

  io.alu.valid := buffer.io.deq.valid && buffer_deq.decode.ex_type.alu
  io.alu.bits.op1 := ex_op1
  io.alu.bits.op2 := ex_op2
  io.alu.bits.func := buffer_deq.decode.func
  io.alu.bits.mask := buffer_deq.decode.mask
  io.alu.bits.wid := buffer_deq.decode.wid
  io.alu.bits.pc := buffer_deq.decode.pc
  io.alu.bits.rd := buffer_deq.decode.rd
  io.alu.bits.imm := buffer_deq.decode.imm
  io.alu.bits.rs1_data := buffer_deq.vgpr_rsp.rs1_data(PriorityEncoder(buffer_deq.decode.mask))
  io.alu.bits.branch := buffer_deq.decode.branch

  io.lsu.valid := buffer.io.deq.valid && buffer_deq.decode.ex_type.lsu
  io.lsu.bits.func := buffer_deq.decode.mem_cmd
  io.lsu.bits.wid := buffer_deq.decode.wid
  io.lsu.bits.mask := buffer_deq.decode.mask
  io.lsu.bits.addr := buffer_deq.vgpr_rsp.rs1_data
  io.lsu.bits.rd := buffer_deq.decode.rd
  io.lsu.bits.data := buffer_deq.vgpr_rsp.rs2_data
  io.lsu.bits.offset := buffer_deq.decode.imm
  io.lsu.bits.pc := buffer_deq.decode.pc

  val mapping = Seq((1.U, io.alu.ready), (2.U, io.lsu.ready))
  buffer.io.deq.ready := MuxLookup(Cat(buffer_deq.decode.ex_type.lsu, buffer_deq.decode.ex_type.alu), 1.B)(mapping)
}
