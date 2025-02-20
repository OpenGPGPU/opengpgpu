package ogpu.lib

import chisel3._
import chisel3.util._

class ReadWriteSmem(width: Int = 32, depth: Int = 1024) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(log2Ceil(depth).W))
    val dataIn = Input(UInt(width.W))
    val dataOut = Output(UInt(width.W))
  })

  val mem = SyncReadMem(depth, UInt(width.W))
  when(io.enable && io.write) {
    mem.write(io.addr, io.dataIn)
  }
  io.dataOut := mem.read(io.addr, io.enable)
}

class MaskedSmem_2R1W(width: Int = 32, depth: Int = 1024, vecLen: Int = 32) extends Module {
  val io = IO(new Bundle {
    val write_en = Input(Bool())
    val waddr = Input(UInt(log2Ceil(depth).W))
    val raddr = Input(UInt(log2Ceil(depth).W))
    val raddr2 = Input(UInt(log2Ceil(depth).W))
    val mask = Input(Vec(vecLen, Bool()))
    val dataIn = Input(Vec(vecLen, UInt(width.W)))
    val dataOut = Output(Vec(vecLen, UInt(width.W)))
    val dataOut2 = Output(Vec(vecLen, UInt(width.W)))
  })

  val mem = SyncReadMem(depth, Vec(vecLen, UInt(width.W)))
  when(io.write_en) {
    mem.write(io.waddr, io.dataIn, io.mask)
  }
  io.dataOut := mem.read(io.raddr, 1.B)
  io.dataOut2 := mem.read(io.raddr2, 1.B)
}
