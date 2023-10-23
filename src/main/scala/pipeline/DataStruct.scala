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

class BranchSignal(implicit p: Parameters) extends Bundle {
  val jal = Bool()
  val jalr = Bool()
  val branch = Bool()
}

class ALUData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val numWarps = p(WarpNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val regIDWidth = p(RegIDWidth)
  val aluFn = p(ALUFunc)

  val op1 = Vec(numThreads, UInt(xLen.W))
  val op2 = Vec(numThreads, UInt(xLen.W))
  val func = UInt(aluFn.SZ_ALU_FN.W)
  val mask = Vec(numThreads, Bool())
  val wid = UInt(log2Ceil(numWarps).W)
  val pc = UInt(addrWidth.W)
  val rd = UInt(regIDWidth.W)
  val branch = new BranchSignal()
  val imm = UInt(xLen.W)
  val rs1_data = UInt(xLen.W)
}

class BranchData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val numWarps = p(WarpNum)
  val addrWidth = p(AddrWidth)
  val xLen = p(XLen)

  val branch = new BranchSignal()
  val mask = Vec(numThreads, Bool())
  val orig_mask = Vec(numThreads, Bool())
  val wid = UInt(log2Ceil(numWarps).W)
  val pc = UInt(addrWidth.W)
  val imm = UInt(xLen.W)
  val rs1_data = UInt(xLen.W)
}

class LSUData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)
  val regIDWidth = p(RegIDWidth)
  val aluFn = p(ALUFunc)

  val addr = Vec(numThreads, UInt(addrWidth.W))
  val data = Vec(numThreads, UInt(xLen.W))
  val mask = Vec(numThreads, Bool())
  val func = UInt(1.W)
  val wid = UInt(log2Ceil(numWarps).W)

  val pc = UInt(addrWidth.W)
  // val fence = Bool()
  val offset = UInt(xLen.W)
  val rd = UInt(regIDWidth.W)
}

class CommitData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)
  val regIDWidth = p(RegIDWidth)

  val wid = UInt(log2Ceil(numWarps).W)
  val mask = Vec(numThreads, Bool())
  val pc = UInt(addrWidth.W)
  val eop = Bool()
  val rd = UInt(regIDWidth.W)
  val data = Vec(numThreads, UInt(xLen.W))
}

class StackData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val mask = Vec(numThreads, Bool())
  val pc = UInt(addrWidth.W)
  val orig_mask = Vec(numThreads, Bool())
}

class InstData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)

  val mask = Vec(numThreads, Bool())
  val wid = UInt(log2Ceil(numWarps).W)
  val pc = UInt(addrWidth.W)
  val data = UInt(32.W)
}

class ExType(implicit p: Parameters) extends Bundle {
  val lsu = Bool()
  val alu = Bool()
}

class DecodeData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)
  val regIDWidth = p(RegIDWidth)
  val aluFn = p(ALUFunc)

  val wid = UInt(log2Ceil(numWarps).W)
  val mask = Vec(numThreads, Bool())
  val wb = Bool()
  val imm = UInt(xLen.W)
  val sel_alu1 = UInt(A1_X.getWidth.W)
  val sel_alu2 = UInt(A2_X.getWidth.W)
  val ex_type = new ExType()
  val func = UInt(aluFn.SZ_ALU_FN.W)
  val mem_cmd = UInt(1.W)
  val branch = new BranchSignal()
  val pc = UInt(addrWidth.W)
  val rd = UInt(regIDWidth.W)
  val rs1 = UInt(regIDWidth.W)
  val rs2 = UInt(regIDWidth.W)
}

class WarpControlData(implicit p: Parameters) extends Bundle {
  val numWarps = p(WarpNum)

  val wid = UInt(log2Ceil(numWarps).W)
  val active = Bool()
  val join = Bool()
  val end = Bool()
}

class InstFetchData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)

  val mask = Vec(numThreads, Bool())
  val wid = UInt(log2Ceil(numWarps).W)
  val pc = UInt(addrWidth.W)
}

class WarpCommandData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)

  val mask = Vec(numThreads, Bool())
  val wid = UInt(log2Ceil(numWarps).W)
  val pc = UInt(addrWidth.W)
}

class WarpEndData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)

  val wid = UInt(log2Ceil(numWarps).W)
}

class BranchControlData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)

  val mask = Vec(numThreads, Bool())
  val wid = UInt(log2Ceil(numWarps).W)
  val pc = UInt(addrWidth.W)
  val data = new StackData()
  val diverge = Bool()
}

class WritebackData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)
  val regIDWidth = p(RegIDWidth)

  val wid = UInt(log2Ceil(numWarps).W)
  val mask = Vec(numThreads, Bool())
  val pc = UInt(addrWidth.W)
  val eop = Bool()
  val rd = UInt(regIDWidth.W)
  val data = Vec(numThreads, UInt(xLen.W))
}

class ReadGPRReq(implicit p: Parameters) extends Bundle {
  val numWarps = p(WarpNum)
  val regIDWidth = p(RegIDWidth)

  val wid = UInt(log2Ceil(numWarps).W)
  val rs1 = UInt(regIDWidth.W)
  val rs2 = UInt(regIDWidth.W)
}

class ReadGPRRsp(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)

  val rs1_data = Vec(numThreads, UInt(xLen.W))
  val rs2_data = Vec(numThreads, UInt(xLen.W))
}

class ThreadMask(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)

  val mask = Vec(numThreads, Bool())
}
