/*
 * Copyright (c) 2023 OpenGPGPU
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package opengpgpu.pipeline
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

// add join inst based on jal, use wxd to distinguish
/* Automatically generated by parse_opcodes */
object GPUInstructions {
  def JOIN = BitPat("b?????????????????000?????1101011")

}

import GPUInstructions._
class GPUDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    // format: off
    JOIN -> List(
      Y,N,N,N,Y,N,N,N,N,N,N,N,A2_SIZE,A1_PC,IMM_UJ,DW_XPR,aluFn.FN_ADD,N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N))
    // format: on
}

// reuse rocketchip decode
class IDecodeUnit(implicit p: Parameters) extends Module() {
  val io = IO(new Bundle {
    val inst = Flipped(DecoupledIO(new InstData()))
    val decode = DecoupledIO(new DecodeData())
    val wcontrol = DecoupledIO(new WarpControlData())
  })

  val aluFn = new ALUFN
  val decode_table = {
    Seq(new IDecode(aluFn))
  } flatMap (_.table)
  val id_ctrl = Wire(new IntCtrlSigs(aluFn)).decode(io.inst.bits.data, decode_table)
  val exec_ctrl = RegInit(0.U.asTypeOf(new IntCtrlSigs(aluFn)))
  val ctrl_valid = RegInit(0.B)
  val inst_reg = RegInit(0.U.asTypeOf(io.inst.bits))
  when(io.inst.fire) {
    exec_ctrl := id_ctrl
    ctrl_valid := 1.B
    inst_reg := io.inst.bits
  }.elsewhen(io.decode.fire) {
    ctrl_valid := 0.B
  }
  io.inst.ready := !io.decode.valid || io.decode.fire
  io.decode.valid := ctrl_valid
  io.wcontrol.valid := ctrl_valid

  val ctrl = exec_ctrl
  val is_alu = ctrl.wxd && !(ctrl.mem || ctrl.fp || ctrl.mul || ctrl.div || ctrl.csr =/= CSR.N)
  val is_lsu = ctrl.mem
  val is_csr = ctrl.csr =/= CSR.N

  // branch inst break warp schedule
  val is_jal = ctrl.wxd && ctrl.jal
  val is_jalr = ctrl.jalr
  val is_join = !ctrl.wxd && ctrl.jal
  val is_branch = ctrl.branch

  val imm = ImmGen(ctrl.sel_imm, inst_reg.data)

  // output
  io.decode.bits.wid := inst_reg.wid
  io.decode.bits.mask := inst_reg.mask
  io.decode.bits.pc := inst_reg.pc
  io.decode.bits.ex_type := Cat(is_csr, is_lsu, is_alu)
  io.decode.bits.func := ctrl.alu_fn
  io.decode.bits.mem_cmd := ctrl.mem_cmd(0)
  io.decode.bits.wb := ctrl.wxd
  io.decode.bits.use_imm := ctrl.sel_alu2 === A2_IMM
  io.decode.bits.use_pc := ctrl.sel_alu1 === A1_PC
  io.decode.bits.imm := imm.asUInt
  io.decode.bits.branch := Cat(is_jal, is_jalr, is_join, is_branch)
  io.decode.bits.rd := inst_reg.data(11, 7)
  io.decode.bits.rs1 := inst_reg.data(19, 15)
  io.decode.bits.rs2 := inst_reg.data(24, 20)

  io.wcontrol.bits.wid := inst_reg.wid
  io.wcontrol.bits.join := is_join
  io.wcontrol.bits.stall := is_branch || is_jal || is_jalr
}
