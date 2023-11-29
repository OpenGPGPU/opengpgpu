package opengpgpu.pipeline

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._

class RegWriter(implicit p: Parameters) extends Module {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val io = IO(new Bundle {
    val warp_cmd = Flipped(DecoupledIO(new WarpCommandData()))
    val wid = Input(UInt(log2Ceil(numWarps).W))
    val commit_data = DecoupledIO(new CommitData())
    val warp_cmd_out = DecoupledIO(new WarpCommandData())
  })

  object State extends ChiselEnum {
    val IDLE, RUN = Value
  }
  import State._

  val commit_counter = RegInit(0.U(2.W))
  val state = RegInit(IDLE)
  val warp_id = RegInit(0.U(log2Ceil(numWarps).W))
  val warp_cmd = Reg(new WarpCommandData())
  val cmd_finish = commit_counter === warp_cmd.thread_dims

  val tid_x = VecInit.tabulate(numThreads) { i => warp_cmd.thread_id_x & i.U }
  val tid_y = VecInit.tabulate(numThreads) { i => warp_cmd.thread_id_y }
  val tid_z = VecInit.tabulate(numThreads) { i => warp_cmd.thread_id_z }
  val mapping = Seq((0.U, tid_x), (1.U, tid_y), (2.U, tid_z))

  io.warp_cmd.ready := state === IDLE || cmd_finish

  // init
  io.warp_cmd_out.bits := 0.U.asTypeOf(warp_cmd)
  io.commit_data.bits := 0.U.asTypeOf(io.commit_data.bits)
  io.commit_data.valid := 0.U
  io.commit_data.bits.mask := 0.U.asTypeOf(io.commit_data.bits.mask)
  io.commit_data.bits.data := 0.U.asTypeOf(io.commit_data.bits.data)
  io.commit_data.bits.eop := 1.B
  io.warp_cmd_out.valid := 0.U

  when(state === IDLE) {
    when(io.warp_cmd.fire) {
      state := RUN
      warp_id := io.wid
      warp_cmd := io.warp_cmd.bits
    }
  }.elsewhen(state === RUN) {
    when(!(warp_cmd.thread_dims === 0.U)) {
      io.commit_data.valid := 1.B
      io.commit_data.bits.wid := warp_id
      io.commit_data.bits.mask := VecInit(Seq.fill(numThreads)(1.B))
      io.commit_data.bits.rd := warp_cmd.reg_index + commit_counter
      io.commit_data.bits.data := MuxLookup(commit_counter, 0.U.asTypeOf(tid_x))(mapping)
    }
    when(cmd_finish) {
      io.warp_cmd_out.valid := 1.B
      io.warp_cmd_out.bits := warp_cmd
      io.warp_cmd_out.bits.wid := warp_id
      when(!io.warp_cmd.valid) {
        state := IDLE
      }.elsewhen(io.warp_cmd.valid) {
        warp_id := io.wid
        warp_cmd := io.warp_cmd.bits
      }
    }
  }

  when(cmd_finish) {
    commit_counter := 0.U
  }.elsewhen(io.commit_data.fire) {
    commit_counter := commit_counter + 1.U
  }

  val occuppied_warp = RegInit(0.U(log2Ceil(numWarps).W))
}

class WarpScheduler(implicit p: Parameters) extends Module {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val numThreads = p(ThreadNum)
  val addrWidth = p(AddrWidth)

  val xLen = p(XLen)

  val io = IO(new Bundle {
    val warp_cmd = Flipped(DecoupledIO(new WarpCommandData()))
    val warp_ctl = Flipped(DecoupledIO(new WarpControlData()))
    val branch_ctl = Flipped(DecoupledIO(new BranchControlData()))
    val inst_fetch = DecoupledIO(new InstFetchData())
    val warp_end = DecoupledIO(new WarpEndData())
    val commit_data = DecoupledIO(new CommitData())
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

  val reg_writer = Module(new RegWriter())
  reg_writer.io.warp_cmd.bits := io.warp_cmd.bits
  reg_writer.io.warp_cmd.valid := io.warp_cmd.valid
  io.warp_cmd.ready := reg_writer.io.warp_cmd.ready & warp_idle.asUInt.orR
  reg_writer.io.wid := idle_id
  io.commit_data <> reg_writer.io.commit_data
  reg_writer.io.warp_cmd_out.ready := 1.B

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
    when(io.warp_cmd.fire && i.U === idle_id) {
      warp_idle(i) := 0.B
    }

    when(reg_writer.io.warp_cmd_out.fire && i.U === reg_writer.io.warp_cmd_out.bits.wid) {
      warp_active(i) := 1.B
      warp_pc(i) := reg_writer.io.warp_cmd_out.bits.pc
      warp_tmask(i) := reg_writer.io.warp_cmd_out.bits.mask
    }

    when(io.warp_ctl.fire && io.warp_ctl.bits.end && i.U === io.warp_ctl.bits.wid) {
      warp_idle(i) := 1.B
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

  when(has_active) {
    io.inst_fetch.valid := 1.B
    io.inst_fetch.bits.pc := warp_pc(active_id)
    io.inst_fetch.bits.mask := warp_tmask(active_id)
    io.inst_fetch.bits.wid := active_id
  }.otherwise {
    io.inst_fetch.valid := 0.B
    io.inst_fetch.bits.pc := 0.U
    io.inst_fetch.bits.mask := VecInit(Seq.fill(numThreads)(0.B))
    io.inst_fetch.bits.wid := 0.U
  }
}
