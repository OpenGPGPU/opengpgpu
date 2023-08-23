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
    val read_req = Flipped(new ReadGPRReq())
    val read_rsp = new ReadGPRRsp()
  })

  io.writeback.ready := 1.B

  val gpr_ram = Module(new MaskedSmem_2R1W(xLen, numWarps * numRegs, numThreads))
  val write_addr = Cat(io.writeback.bits.wid, io.writeback.bits.rd)
  val read_addr = Cat(io.read_req.wid, io.read_req.rs1)
  val read_addr2 = Cat(io.read_req.wid, io.read_req.rs2)

  val raddr_reg = RegInit(0.U)
  val raddr2_reg = RegInit(0.U)

  gpr_ram.io.write_en := io.writeback.valid
  gpr_ram.io.waddr := write_addr
  gpr_ram.io.raddr := read_addr
  gpr_ram.io.raddr2 := read_addr2
  gpr_ram.io.mask := io.writeback.bits.mask
  gpr_ram.io.dataIn := io.writeback.bits.data

  raddr_reg := io.read_req.rs1
  raddr2_reg := io.read_req.rs2
  io.read_rsp.rs1_data := Mux(raddr_reg === 0.U, 0.U.asTypeOf(gpr_ram.io.dataOut), gpr_ram.io.dataOut)
  io.read_rsp.rs2_data := Mux(raddr2_reg === 0.U, 0.U.asTypeOf(gpr_ram.io.dataOut), gpr_ram.io.dataOut2)
}
