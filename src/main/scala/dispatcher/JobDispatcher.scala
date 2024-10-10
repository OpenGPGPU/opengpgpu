package ogpu.dispatcher

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

case class JobDispatchParams() {
  def buffer_num = 1
}

class JobDispatcher(
  params: JobDispatchParams
)(
  implicit p: Parameters)
    extends LazyModule {

  lazy val module = new Impl(this)

  class Impl(
    outer: JobDispatcher
  )(
    implicit p: Parameters)
      extends LazyModuleImp(outer) {
    val io = IO(new Bundle {
      val aql = Flipped(DecoupledIO(new AQLBundle))
      val task = DecoupledIO(new WorkGroupTaskBundle)
      val task_resp = Flipped(DecoupledIO(new WorkGroupTaskRespBundle))
    })

    val s_idle :: s_working :: s_finish :: Nil = Enum(3)
    val state = RegInit(s_idle)

    io.aql.ready := state === s_idle

    val grid_x = RegInit(UInt(32.W))
    val grid_y = RegInit(UInt(32.W))
    val grid_z = RegInit(UInt(32.W))
    val workgroup_x = RegInit(UInt(16.W))
    val workgroup_y = RegInit(UInt(16.W))
    val workgroup_z = RegInit(UInt(16.W))
    val grid_counter_x = RegInit(UInt(32.W))
    val grid_counter_y = RegInit(UInt(32.W))
    val grid_counter_z = RegInit(UInt(32.W))

    val taskDone = grid_counter_x === (grid_x - 1.U) &
      grid_counter_y === (grid_y - 1.U) &
      grid_counter_z === (grid_z - 1.U)

    val grid_x_acc = (grid_counter_x === (grid_x - 1.U))
    val grid_y_acc = (grid_counter_x === (grid_x - 1.U)) & (grid_counter_y =/= (grid_y - 1.U))
    val grid_z_acc = (grid_counter_x === (grid_x - 1.U)) & (grid_counter_y === (grid_y - 1.U))

    val grid_rcounter_x = RegInit(UInt(32.W))
    val grid_rcounter_y = RegInit(UInt(32.W))
    val grid_rcounter_z = RegInit(UInt(32.W))

    val grid_x_racc = (grid_rcounter_x === (grid_x - 1.U))
    val grid_y_racc = (grid_rcounter_x === (grid_x - 1.U)) & (grid_rcounter_y =/= (grid_y - 1.U))
    val grid_z_racc = (grid_rcounter_x === (grid_x - 1.U)) & (grid_rcounter_y === (grid_y - 1.U))

    val recDone = grid_rcounter_x === (grid_x - 1.U) &
      grid_rcounter_y === (grid_y - 1.U) &
      grid_rcounter_z === (grid_z - 1.U)

    // state transition
    switch(state) {
      is(s_idle) {
        when(io.aql.fire) {
          state := s_working
        }
      }
      is(s_working) {
        when(taskDone & io.task.fire) {
          state := s_finish
        }
      }
      is(s_finish) {
        when(state_rec === s_rec_finish) {
          state := s_idle
        }
      }
    }

    // state action
    switch(state) {
      is(s_idle) {
        when(io.aql.fire) {
          grid_x := io.aql.bits.grid_size_x
          grid_y := io.aql.bits.grid_size_y
          grid_z := io.aql.bits.grid_size_z
          workgroup_x := io.aql.bits.workgroup_size_x
          workgroup_y := io.aql.bits.workgroup_size_y
          workgroup_z := io.aql.bits.workgroup_size_z
          grid_counter_x := 0.U
          grid_counter_y := 0.U
          grid_counter_z := 0.U
          io.task.valid := true.B
          io.task.bits.workgroup_size_x := io.aql.bits.workgroup_size_x
          io.task.bits.workgroup_size_y := io.aql.bits.workgroup_size_y
          io.task.bits.workgroup_size_z := io.aql.bits.workgroup_size_z
          io.task.bits.grid_id_x := 0.U
          io.task.bits.grid_id_y := 0.U
          io.task.bits.grid_id_z := 0.U
        }
      }
      is(s_working) {
        io.task.bits.workgroup_size_x := workgroup_x
        io.task.bits.workgroup_size_y := workgroup_y
        io.task.bits.workgroup_size_z := workgroup_z
        io.task.bits.grid_id_x := grid_counter_x
        io.task.bits.grid_id_y := grid_counter_y
        io.task.bits.grid_id_z := grid_counter_z
        when(io.task.fire) {
          when(grid_x_acc) {
            grid_counter_x := grid_counter_x + 1.U
          }.otherwise {
            grid_counter_x := 0.U
          }

          when(grid_y_acc) {
            grid_counter_y := grid_counter_y + 1.U
          }.otherwise {
            grid_counter_y := 0.U
          }

          when(grid_z_acc) {
            grid_counter_z := grid_counter_z + 1.U
          }.otherwise {
            grid_counter_z := 0.U
          }
        }

        when(taskDone & io.task.fire) {
          io.task.valid := false.B
        }
      }
    }

    val s_rec_idle :: s_rec_working :: s_rec_finish :: Nil = Enum(3)
    val state_rec = RegInit(s_rec_idle)

    io.task_resp.ready := state_rec === s_rec_working

    switch(state_rec) {
      is(s_rec_idle) {
        when(io.task.fire) {
          state_rec := s_rec_working
        }
      }
      is(s_rec_working) {
        when(recDone & io.task_resp.fire) {
          state_rec := s_rec_finish
        }
      }
      is(s_rec_finish) {
        state_rec := s_rec_idle
      }
    }

    switch(state_rec) {
      is(s_rec_idle) {
        grid_rcounter_x := 0.U
        grid_rcounter_y := 0.U
        grid_rcounter_z := 0.U
      }
      is(s_rec_working) {
        when(io.task_resp.fire) {
          when(grid_x_racc) {
            grid_rcounter_x := grid_rcounter_x + 1.U
          }.otherwise {
            grid_rcounter_x := 0.U
          }

          when(grid_y_racc) {
            grid_rcounter_y := grid_rcounter_y + 1.U
          }.otherwise {
            grid_rcounter_y := 0.U
          }

          when(grid_z_racc) {
            grid_rcounter_z := grid_rcounter_z + 1.U
          }.otherwise {
            grid_rcounter_z := 0.U
          }
        }
        when(recDone & io.task_resp.fire) {
          // io.intr.valid := true.B
        }
      }
      is(s_rec_finish) {
        // io.intr.valid := false.B
      }
    }
  }
}
