package ogpu.dispatcher

import chisel3._

class AQLBundle() extends Bundle {
  val header = UInt(16.W)
  val dimensions = UInt(2.W)
  val reserved1 = UInt(14.W)
  val workgroup_size_x = UInt(16.W)
  val workgroup_size_y = UInt(16.W)
  val workgroup_size_z = UInt(16.W)
  val reserved2 = UInt(16.W)
  val grid_size_x = UInt(32.W)
  val grid_size_y = UInt(32.W)
  val grid_size_z = UInt(32.W)
  val private_sgement_size = UInt(32.W)
  val group_segment_size = UInt(32.W)
  val kernel_object = UInt(64.W)
  val kernargs_address = UInt(64.W)
  val completion_signal = UInt(64.W)
}

class WorkGroupTaskBundle() extends Bundle {
  val workgroup_size_x = UInt(16.W)
  val workgroup_size_y = UInt(16.W)
  val workgroup_size_z = UInt(16.W)
  // val grid_size_x = UInt(32.W)
  // val grid_size_y = UInt(32.W)
  // val grid_size_z = UInt(32.W)
  val grid_id_x = UInt(32.W)
  val grid_id_y = UInt(32.W)
  val grid_id_z = UInt(32.W)
  val private_sgement_size = UInt(32.W)
  val group_segment_size = UInt(32.W)
  val kernel_object = UInt(64.W)

  val kernargs_address = UInt(64.W)
}

class WorkGroupTaskRespBundle() extends Bundle {
  val finish = Bool()
}
