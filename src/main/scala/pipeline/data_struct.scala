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
import opengpgpu.config.parameters._


object ALUOps {
  val SZ_ALU_FUNC = 5
  def FN_X = BitPat("b?????")
  def FN_ADD = 0.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SL = 1.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SEQ = 2.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SNE = 3.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_XOR = 4.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SR = 5.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_OR = 6.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_AND = 7.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SUB = 10.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SRA = 11.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SLT = 12.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SGE = 13.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SLTU = 14.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_SGEU = 15.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MAX=16.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MIN=17.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MAXU=18.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MINU=19.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_A1ZERO = 8.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MUL = 20.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MULH = 21.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MULHU = 22.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MULHSU = 23.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MACC = 24.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_NMSAC = 25.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_MADD = 26.U(ALUOps.SZ_ALU_FUNC.W)
  def FN_NMSUB = 27.U(ALUOps.SZ_ALU_FUNC.W)

  def isSub(cmd: UInt) = (cmd >= FN_SUB) & (cmd <= FN_SGEU)
  def isCmp(cmd: UInt) = (cmd >= FN_SLT) & (cmd <= FN_SGEU)
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
  def isMIN(cmd:UInt)=(cmd(4,2)===("b100").U)
  def isMUL(cmd:UInt)=(cmd(4,2)===("b101").U)
  def isMAC(cmd:UInt)=(cmd(4,2)===("b110").U)
}

class VectorExeData(num_thread: Int = SOFT_THREAD) extends Bundle {
  val op1 = Vec(num_thread, UInt(xLen.W))
  val op2 = Vec(num_thread, UInt(xLen.W))
  val func = UInt(ALUOps.SZ_ALU_FUNC.W)
  val mask = Vec(num_thread, Bool())
}

class VectorData(num_thread: Int = SOFT_THREAD) extends Bundle {
  val data = Vec(num_thread, UInt(xLen.W))
  val mask = Vec(num_thread, Bool())
}

class ThreadMask(num_thread: Int = SOFT_THREAD) extends Bundle {
  val mask = Vec(num_thread, Bool())
}
