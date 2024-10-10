package ogpu.tile

import chisel3._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._

class CuTaskBundle(
  implicit p: Parameters)
    extends Bundle {
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)
  val numWarps = p(WarpNum)
  val dimWidth = p(DimWidth)
  val xLen = p(XLen)

  val mask = Vec(numThreads, Bool())
  // max threads num in a workgroup
  val thread_dims = Vec(3, UInt(dimWidth.W))
  val vgpr_num = UInt(2.W)
  val sgprs = Vec(16, UInt(xLen.W))
  val sgpr_num = UInt(4.W)
  val reg_index = UInt(p(RegIDWidth).W)
  val pc = UInt(addrWidth.W)
}

class CuTaskRespBundle(
  implicit p: Parameters)
    extends Bundle {
  val finish = Bool()
}
