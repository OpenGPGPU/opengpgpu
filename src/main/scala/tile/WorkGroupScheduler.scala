package ogpu.tile

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.util._

import ogpu.dispatcher._

// init register and send warp task to warp scheduler in cu

case class WgSchedParams() {
  def haha = 1
}

class WorkGroupScheduler(
  params: WgSchedParams
)(
  implicit p: Parameters)
    extends LazyModule {
  lazy val module = new Impl(this)

  class Impl(
    outer: WorkGroupScheduler
  )(
    implicit p: Parameters)
      extends LazyModuleImp(outer) {
    val io = IO(new Bundle {
      val task = Flipped(DecoupledIO(new WorkGroupTaskBundle))
      val task_resp = DecoupledIO(new WorkGroupTaskRespBundle)
      val cu_task = DecoupledIO(new CuTaskBundle)
      val cu_task_resp = Flipped(DecoupledIO(new CuTaskRespBundle))
    })

    val workgroup_x = RegInit(UInt(16.W))
    val workgroup_y = RegInit(UInt(16.W))
    val workgroup_z = RegInit(UInt(16.W))
    val workgroup_counter_x = RegInit(UInt(16.W))
    val workgroup_counter_y = RegInit(UInt(16.W))
    val workgroup_counter_z = RegInit(UInt(16.W))

    val workgroup_rcounter_x = RegInit(UInt(16.W))
    val workgroup_rcounter_y = RegInit(UInt(16.W))
    val workgroup_rcounter_z = RegInit(UInt(16.W))

    val s_idle :: s_working :: s_finish :: Nil = Enum(3)
    val state = RegInit(s_idle)

    val taskDone = workgroup_counter_x === (workgroup_x - 32.U) &
      workgroup_counter_y === (workgroup_y - 1.U) &
      workgroup_counter_z === (workgroup_z - 1.U)

    val recDone = workgroup_rcounter_x === (workgroup_x - 32.U) &
      workgroup_rcounter_y === (workgroup_y - 1.U) &
      workgroup_rcounter_z === (workgroup_z - 1.U)

    val workgroup_x_acc = (workgroup_counter_x =/= (workgroup_x - 32.U))
    val workgroup_y_acc = (workgroup_counter_x === (workgroup_x - 1.U)) & (workgroup_counter_y =/= (workgroup_y - 1.U))
    val workgroup_z_acc = (workgroup_counter_x === (workgroup_x - 1.U)) & (workgroup_counter_y === (workgroup_y - 1.U))

    val workgroup_x_racc = (workgroup_rcounter_x =/= (workgroup_x - 32.U))
    val workgroup_y_racc =
      (workgroup_rcounter_x === (workgroup_x - 1.U)) & (workgroup_rcounter_y =/= (workgroup_y - 1.U))
    val workgroup_z_racc =
      (workgroup_rcounter_x === (workgroup_x - 1.U)) & (workgroup_rcounter_y === (workgroup_y - 1.U))

    // state transition
    switch(state) {
      is(s_idle) {
        when(io.task.fire) {
          state := s_working
        }
      }
      is(s_working) {
        when(taskDone & io.cu_task.fire) {
          state := s_finish
        }
      }
      is(s_finish) {
        when(io.task_resp.fire) {
          state := s_idle
        }
      }
    }

    io.cu_task.bits.thread_dims := VecInit(Seq(workgroup_counter_x, workgroup_counter_y, workgroup_counter_z))
    io.cu_task.valid := state === s_working
    // state action
    switch(state) {
      is(s_idle) {
        when(io.task.fire) {
          workgroup_x := io.task.bits.workgroup_size_x
          workgroup_y := io.task.bits.workgroup_size_y
          workgroup_z := io.task.bits.workgroup_size_z
          workgroup_counter_x := 0.U
          workgroup_counter_y := 0.U
          workgroup_counter_z := 0.U
        }
      }
      is(s_working) {
        when(io.task.fire) {
          when(workgroup_x_acc) {
            workgroup_counter_x := workgroup_counter_x + 32.U
          }.otherwise {
            workgroup_counter_x := 0.U
          }

          when(workgroup_y_acc) {
            workgroup_counter_y := workgroup_counter_y + 1.U
          }.otherwise {
            workgroup_counter_y := 0.U
          }

          when(workgroup_z_acc) {
            workgroup_counter_z := workgroup_counter_z + 1.U
          }.otherwise {
            workgroup_counter_z := 0.U
          }
        }
      }
    }

    val s_rec_idle :: s_rec_working :: s_rec_finish :: Nil = Enum(3)
    val state_rec = RegInit(s_rec_idle)

    io.cu_task_resp.ready := state_rec === s_rec_working

    switch(state_rec) {
      is(s_rec_idle) {
        when(io.cu_task.fire) {
          state_rec := s_rec_working
        }
      }
      is(s_rec_working) {
        when(recDone & io.task_resp.fire) {
          state_rec := s_rec_finish
        }
      }
      is(s_rec_finish) {
        when(io.task_resp.fire) {
          state_rec := s_rec_idle
        }
      }
    }

    io.task_resp.valid := state_rec === s_rec_finish
    switch(state_rec) {
      is(s_rec_idle) {
        workgroup_rcounter_x := 0.U
        workgroup_rcounter_y := 0.U
        workgroup_rcounter_z := 0.U
      }
      is(s_rec_working) {
        when(io.task_resp.fire) {
          when(workgroup_x_racc) {
            workgroup_rcounter_x := workgroup_rcounter_x + 1.U
          }.otherwise {
            workgroup_rcounter_x := 0.U
          }

          when(workgroup_y_racc) {
            workgroup_rcounter_y := workgroup_rcounter_y + 1.U
          }.otherwise {
            workgroup_rcounter_y := 0.U
          }

          when(workgroup_z_racc) {
            workgroup_rcounter_z := workgroup_rcounter_z + 1.U
          }.otherwise {
            workgroup_rcounter_z := 0.U
          }
        }
      }
    }
  }
}
