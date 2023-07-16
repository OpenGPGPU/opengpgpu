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

import chisel3._
import opengpgpu.config.parameters._

class ReadWriteSmem(width: Int = xLen, depth: Int = 1024, addrWidth: Int = 10) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val dataIn = Input(UInt(width.W))
    val dataOut = Output(UInt(width.W))
  })

  val mem = SyncReadMem(depth, UInt(width.W))
  mem.write(io.addr, io.dataIn)
  io.dataOut := mem.read(io.addr, io.enable)
}

class MaskedReadWriteSmem(width: Int = xLen, depth: Int = 1024, addrWidth: Int = 10, vecLen: Int = SOFT_THREAD) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val mask = Input(Vec(vecLen, Bool()))
    val dataIn = Input(Vec(vecLen, UInt(width.W)))
    val dataOut = Output(Vec(vecLen, UInt(width.W)))
  })

  val mem = SyncReadMem(depth, Vec(vecLen, UInt(width.W)))
  mem.write(io.addr, io.dataIn, io.mask)
  io.dataOut := mem.read(io.addr, io.enable)
}
