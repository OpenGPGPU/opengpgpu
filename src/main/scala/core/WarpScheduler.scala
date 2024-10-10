package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._
import ogpu.tile._

class VGPRWriter(
  implicit p: Parameters)
    extends Module {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val numThreads = p(ThreadNum)
  val xlen = p(XLen)
  val addrWidth = p(AddrWidth)

  val io = IO(new Bundle {
    val warp_cmd = Input(Valid(new CuTaskBundle()))
    val wid = Input(UInt(log2Ceil(numWarps).W))
    val commit_data = DecoupledIO(new CommitVData())
    val finish = DecoupledIO(new Bool())
    val idle = Output(Bool())
  })

  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val commit_counter = RegInit(0.U(2.W))
  io.idle := state === s_idle

  val counter_add1 = commit_counter + 1.U

  val tid_data = Wire(Vec(3, Vec(numThreads, UInt(xlen.W))))
  tid_data(0) := VecInit.tabulate(numThreads) { i => io.warp_cmd.bits.thread_dims(0) | i.U }
  tid_data(1) := VecInit.tabulate(numThreads) { _ => io.warp_cmd.bits.thread_dims(1) }
  tid_data(2) := VecInit.tabulate(numThreads) { _ => io.warp_cmd.bits.thread_dims(2) }

  switch(state) {
    is(s_idle) {
      when(io.warp_cmd.valid) {
        state := s_working
      }
    }
    is(s_working) {
      when(((counter_add1 === io.warp_cmd.bits.vgpr_num) & io.commit_data.fire) | io.warp_cmd.bits.vgpr_num === 0.U) {
        state := s_finish
      }
    }
    is(s_finish) {
      when(io.finish.fire) {
        state := s_idle
      }
    }
  }

  io.commit_data.bits.wid := io.wid
  io.commit_data.bits.mask := io.warp_cmd.bits.mask
  io.commit_data.bits.rd := counter_add1
  io.commit_data.bits.eop := true.B
  io.commit_data.bits.pc := 0.U
  io.commit_data.valid := false.B
  io.commit_data.bits.data := tid_data(commit_counter)
  io.finish.bits := 0.U
  io.commit_data.valid := state === s_working
  io.finish.valid := state === s_finish
  switch(state) {
    is(s_idle) {
      commit_counter := 0.U
    }
    is(s_working) {
      when(io.commit_data.fire & counter_add1 =/= io.warp_cmd.bits.vgpr_num) {
        commit_counter := counter_add1
      }
    }
  }
}

class SGPRWriter(
  implicit p: Parameters)
    extends Module {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val addrWidth = p(AddrWidth)

  val io = IO(new Bundle {
    val warp_cmd = Input(Valid(new CuTaskBundle()))
    val wid = Input(UInt(log2Ceil(numWarps).W))
    val commit_data = DecoupledIO(new CommitSData())
    val finish = DecoupledIO(Bool())
    val idle = Output(Bool())
  })

  val s_idle :: s_working :: s_finish :: Nil = Enum(3)

  val commit_counter = RegInit(0.U(5.W))
  val state = RegInit(s_idle)

  val counter_add1 = commit_counter + 1.U
  val commit_data = io.warp_cmd.bits.sgprs(commit_counter)

  io.idle := state === s_idle
  switch(state) {
    is(s_idle) {
      when(io.warp_cmd.valid) {
        state := s_working
      }
    }
    is(s_working) {
      when(((commit_counter === io.warp_cmd.bits.sgpr_num) & io.commit_data.fire) | io.warp_cmd.bits.sgpr_num === 0.U) {
        state := s_finish
      }
    }
    is(s_finish) {
      when(io.finish.fire) {
        state := s_idle
      }
    }
  }

  io.commit_data.bits.wid := io.wid
  io.commit_data.bits.rd := commit_counter
  io.commit_data.bits.eop := true.B
  io.commit_data.bits.pc := 0.U
  io.commit_data.bits.data := commit_data
  io.commit_data.bits.mask := io.warp_cmd.bits.mask(0)
  io.finish.valid := state === s_finish
  io.commit_data.valid := state === s_working
  io.finish.bits := 0.U
  switch(state) {
    is(s_idle) {
      io.commit_data.valid := false.B
      commit_counter := 0.U
    }
    is(s_working) {
      when(io.commit_data.fire & commit_counter =/= io.warp_cmd.bits.sgpr_num) {
        commit_counter := counter_add1
      }
    }
  }
}

class WarpScheduler(
  implicit p: Parameters)
    extends Module {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val xLen = p(XLen)

  val io = IO(new Bundle {
    val warp_cmd = Flipped(DecoupledIO(new CuTaskBundle()))
    val warp_ctl = Flipped(DecoupledIO(new WarpControlData()))
    val branch_ctl = Flipped(DecoupledIO(new BranchControlData()))
    val inst_fetch = DecoupledIO(new InstFetchData())
    val warp_end = DecoupledIO(new WarpEndData())
    val sgpr_commit = DecoupledIO(new CommitSData())
    val vgpr_commit = DecoupledIO(new CommitVData())
  })

  val warp_idle = RegInit(VecInit(Seq.fill(numWarps)(1.B)))
  val warp_active = RegInit(VecInit(Seq.fill(numWarps)(0.B)))
  val warp_pc = RegInit(VecInit(Seq.fill(numWarps)(0.U(addrWidth.W))))
  val warp_tmask = RegInit(VecInit(Seq.fill(numWarps)(VecInit(Seq.fill(numThreads)(0.B)))))
  val pop_valid = RegInit(0.B)

  val pop_wid = RegInit(0.U(log2Ceil(numWarps).W))
  io.warp_ctl.ready := true.B
  io.branch_ctl.ready := true.B

  val has_idle = warp_idle.asUInt.orR
  val has_active = warp_active.asUInt.orR
  val idle_id = PriorityEncoder(warp_idle)
  val active_id = PriorityEncoder(warp_active)

  val simt_stack = VecInit(Seq.fill(numWarps)(Module(new SIMTStack()).io))

  val pop_diverge = Wire(Bool())
  val pop_data = Wire(new StackData())

  val vgpr_writer = Module(new VGPRWriter())
  val sgpr_writer = Module(new SGPRWriter())

  val lock_warp = RegInit(0.U(log2Ceil(numWarps).W))

  vgpr_writer.io.warp_cmd.bits := io.warp_cmd.bits
  sgpr_writer.io.warp_cmd.bits := io.warp_cmd.bits
  vgpr_writer.io.wid := lock_warp
  sgpr_writer.io.wid := lock_warp

  val writer_finish = RegInit(false.B)
  writer_finish := sgpr_writer.io.finish.valid & vgpr_writer.io.finish.valid

  io.warp_cmd.ready := writer_finish
  vgpr_writer.io.finish.ready := writer_finish
  sgpr_writer.io.finish.ready := writer_finish

  sgpr_writer.io.wid := lock_warp
  vgpr_writer.io.wid := lock_warp
  io.sgpr_commit <> sgpr_writer.io.commit_data
  io.vgpr_commit <> vgpr_writer.io.commit_data

  val s_idle :: s_waiting :: Nil = Enum(2)
  val state = RegInit(s_idle)

  switch(state) {
    is(s_idle) {
      when(io.warp_cmd.valid & sgpr_writer.io.idle & vgpr_writer.io.idle) {
        state := s_waiting
      }
    }
    is(s_waiting) {
      when(writer_finish) {
        state := s_idle
      }
    }
  }

  vgpr_writer.io.warp_cmd.valid := false.B
  sgpr_writer.io.warp_cmd.valid := false.B

  switch(state) {
    is(s_idle) {
      when(io.warp_cmd.valid & sgpr_writer.io.idle & vgpr_writer.io.idle & has_idle) {
        lock_warp := idle_id
        vgpr_writer.io.warp_cmd.valid := true.B
        sgpr_writer.io.warp_cmd.valid := true.B
      }
    }
    is(s_waiting) {
      vgpr_writer.io.warp_cmd.valid := false.B
      sgpr_writer.io.warp_cmd.valid := false.B
    }
  }

  for (i <- 0 until numWarps) {
    simt_stack(i).in_diverge := io.branch_ctl.bits.wid === i.U && io.branch_ctl.valid && io.branch_ctl.bits.diverge
    simt_stack(i).in_data := io.branch_ctl.bits.data
    simt_stack(i).push := io.branch_ctl.bits.wid === i.U && io.branch_ctl.valid
    simt_stack(i).pop := io.warp_ctl.bits.wid === i.U && io.warp_ctl.valid && io.warp_ctl.bits.join
  }

  io.warp_end.bits.wid := io.warp_ctl.bits.wid
  io.warp_end.valid := io.warp_ctl.valid & io.warp_ctl.bits.end

  pop_valid := io.warp_ctl.valid & io.warp_ctl.bits.join
  pop_wid := io.warp_ctl.bits.wid

  pop_diverge := simt_stack(pop_wid).out_diverge
  pop_data := simt_stack(pop_wid).out_data

  for (i <- 0 until numWarps) {
    when(io.warp_cmd.fire && i.U === lock_warp) {
      warp_idle(i) := false.B
      warp_active(i) := true.B
      warp_pc(i) := io.warp_cmd.bits.pc
      warp_tmask(i) := io.warp_cmd.bits.mask
    }

    when(io.warp_ctl.fire && io.warp_ctl.bits.end && i.U === io.warp_ctl.bits.wid) {
      warp_idle(i) := true.B
      warp_active(i) := false.B
    }

    when(io.warp_ctl.valid && i.U === io.warp_ctl.bits.wid) {
      warp_active(i) := io.warp_ctl.bits.active
    }

    when(io.branch_ctl.valid && i.U === io.branch_ctl.bits.wid) {
      warp_pc(i) := io.branch_ctl.bits.pc
      warp_active(i) := 1.B
      warp_tmask(i) := io.branch_ctl.bits.mask
    }

    when(pop_valid && i.U === pop_wid) {
      warp_active(i) := 1.B
      when(pop_diverge) {
        warp_pc(i) := pop_data.pc
        warp_tmask(i) := pop_data.mask
      }.otherwise {
        warp_tmask(i) := pop_data.orig_mask
      }
    }

    when(io.inst_fetch.fire && i.U === active_id) {
      warp_active(i) := 0.B
      warp_pc(i) := warp_pc(i) + 4.U
    }
  } // loop num warps

  io.inst_fetch.bits.pc := 0.U
  io.inst_fetch.bits.mask := VecInit(Seq.fill(numThreads)(0.B))
  io.inst_fetch.bits.wid := 0.U
  when(has_active) {
    io.inst_fetch.valid := !warp_idle(active_id)
    io.inst_fetch.bits.pc := warp_pc(active_id)
    io.inst_fetch.bits.mask := warp_tmask(active_id)
    io.inst_fetch.bits.wid := active_id
  }.otherwise {
    io.inst_fetch.valid := 0.B
  }
}
