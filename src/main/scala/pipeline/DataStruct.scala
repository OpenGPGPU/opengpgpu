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

class ALUData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val numWarps = p(WarpNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val regIDWidth = p(RegIDWidth)
  val aluFn = new ALUFN

  val op1 = Vec(numThreads, UInt(xLen.W))
  val op2 = Vec(numThreads, UInt(xLen.W))
  val func = UInt(aluFn.SZ_ALU_FN.W)
  val mask = Vec(numThreads, Bool())
  val wid = UInt(log2Ceil(numWarps).W)
  val pc = UInt(addrWidth.W)
  val rd = UInt(regIDWidth.W)
}

class LSUData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)
  val regIDWidth = p(RegIDWidth)
  val aluFn = new ALUFN

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

object ExType {
  val SZ_EX_TYPE = 3
  val ALU = 0.U
  val LSU = 1.U
  val CSR = 2.U
}

class DecodeData(implicit p: Parameters) extends Bundle {
  val numThreads = p(ThreadNum)
  val xLen = p(XLen)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)
  val regIDWidth = p(RegIDWidth)
  val aluFn = new ALUFN

  val wid = UInt(log2Ceil(numWarps).W)
  val mask = Vec(numThreads, Bool())
  val wb = Bool()
  val imm = UInt(xLen.W)
  val use_pc = Bool()
  val use_imm = Bool()
  // onehot
  val ex_type = UInt(ExType.SZ_EX_TYPE.W)
  val func = UInt(aluFn.SZ_ALU_FN.W)
  val mem_cmd = UInt(1.W)
  val branch = UInt(4.W)
  val pc = UInt(addrWidth.W)
  val rd = UInt(regIDWidth.W)
  val rs1 = UInt(regIDWidth.W)
  val rs2 = UInt(regIDWidth.W)
}

class WarpControlData(implicit p: Parameters) extends Bundle {
  val numWarps = p(WarpNum)

  val wid = UInt(log2Ceil(numWarps).W)
  val join = Bool()
  val stall = Bool()
}

// class IBufferData(implicit p: Parameters) extends Bundle {
//   val numThreads = p(ThreadNum)
//   val xLen = p(XLen)
//   val addrWidth = p(AddrWidth)
//   val numWarps = p(WarpNum)
//   val regIDWidth = p(RegIDWidth)
//
//   val wid = UInt(log2Ceil(numWarps).W)
//   val mask = Vec(numThreads, Bool())
//   val writeback = Bool()
//   val pc = UInt(addrWidth.W)
//   val rd = UInt(regIDWidth.W)
//   val rs1 = UInt(regIDWidth.W)
//   val rs2 = UInt(regIDWidth.W)
//   val rs3 = UInt(regIDWidth.W)
// }

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
