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
import opengpgpu.libs._

class GPR(implicit p: Parameters) extends Module() {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)

  val io = IO(new Bundle {
    val writeback = Flipped(DecoupledIO(new CommitData()))
    val writeback_cmd = Flipped(DecoupledIO(new CommitData()))
    val read_req = Flipped(new ReadGPRReq())
    val read_rsp = new ReadGPRRsp()
  })

  val gpr_ram = VecInit(Seq.fill(numWarps)((Module(new MaskedSmem_2R1W(xLen, numRegs, numThreads)).io)))
  val raddr_reg = RegInit(0.U)
  val raddr2_reg = RegInit(0.U)
  val rwid_reg = RegInit(0.U(log2Ceil(numWarps).W))
  val ready_reg = RegInit(0.B)
  val cmd_ready_reg = RegInit(0.B)

  raddr_reg := io.read_req.rs1
  raddr2_reg := io.read_req.rs2
  rwid_reg := io.read_req.wid

  io.writeback.ready := ready_reg
  io.writeback_cmd.ready := cmd_ready_reg

  for (i <- 0 until numWarps) {
    // init
    gpr_ram(i).write_en := 0.B
    gpr_ram(i).waddr := 0.U
    gpr_ram(i).raddr := io.read_req.rs1
    gpr_ram(i).raddr2 := io.read_req.rs2
    gpr_ram(i).mask := 0.U.asTypeOf(io.writeback.bits.mask)
    gpr_ram(i).dataIn := 0.U.asTypeOf(io.writeback.bits.data)

    when(io.writeback_cmd.valid && i.U === io.writeback_cmd.bits.wid) {
      gpr_ram(i).write_en := io.writeback_cmd.valid
      gpr_ram(i).waddr := io.writeback_cmd.bits.rd
      gpr_ram(i).mask := io.writeback_cmd.bits.mask
      gpr_ram(i).dataIn := io.writeback_cmd.bits.data
    }.elsewhen(io.writeback.valid && i.U === io.writeback.bits.wid) {
      gpr_ram(i).write_en := io.writeback.valid
      gpr_ram(i).waddr := io.writeback.bits.rd
      gpr_ram(i).mask := io.writeback.bits.mask
      gpr_ram(i).dataIn := io.writeback.bits.data
    }

  }

  ready_reg := 0.B
  cmd_ready_reg := 0.B
  when(io.writeback_cmd.valid) {
    cmd_ready_reg := 1.B
  }.elsewhen(io.writeback.valid) {
    ready_reg := 1.B
  }

  io.read_rsp.rs1_data := Mux(raddr_reg === 0.U, 0.U.asTypeOf(gpr_ram(0).dataOut), gpr_ram(rwid_reg).dataOut)
  io.read_rsp.rs2_data := Mux(raddr2_reg === 0.U, 0.U.asTypeOf(gpr_ram(0).dataOut), gpr_ram(rwid_reg).dataOut2)
}
