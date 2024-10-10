package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._

class ScoreBoard(
  implicit p: Parameters)
    extends Module {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val io = IO(new Bundle {
    val ibuffer = Flipped(DecoupledIO(new DecodeData()))
    val writeback = Flipped(DecoupledIO(new WritebackData()))
  })

  // Registers to hold the state of inuse_regs
  val inuseRegs = RegInit(VecInit(Seq.fill(numWarps)(VecInit(Seq.fill(numRegs)(false.B)))))

  // Wires to get the state immediately
  val inuseRegsCurrent = Wire(Vec(numWarps, Vec(numRegs, Bool())))

  // Reserve a register when instruction is valid, ready, and writeback is enabled
  val reserveReg = io.ibuffer.valid && io.ibuffer.ready && io.ibuffer.bits.wb

  // Release a register when writeback to a register is complete (and it is the last instruction of a packet)
  val releaseReg = io.writeback.valid && io.writeback.ready && io.writeback.bits.eop

  // Update `inuseRegsCurrent` with `reserveReg` and `releaseReg`
  for (i <- 0 until numWarps) {
    for (j <- 0 until numRegs) {
      inuseRegsCurrent(i)(j) := inuseRegs(i)(j)
      when(reserveReg && io.ibuffer.bits.wid === i.U && io.ibuffer.bits.rd === j.U) {
        inuseRegsCurrent(i)(j) := true.B
      }
      when(releaseReg && io.writeback.bits.wid === i.U && io.writeback.bits.rd === j.U) {
        inuseRegsCurrent(i)(j) := false.B
      }
    }
  }

  // Update `inuseRegs` with `inuseRegsCurrent` on rising edge of clock
  inuseRegs := inuseRegsCurrent

  // Check if the requested registers are free
  val deqInuseRd = RegInit(false.B)
  val deqInuseRs1 = RegInit(false.B)
  val deqInuseRs2 = RegInit(false.B)

  deqInuseRd := inuseRegsCurrent(io.ibuffer.bits.wid)(io.ibuffer.bits.rd)
  deqInuseRs1 := inuseRegsCurrent(io.ibuffer.bits.wid)(io.ibuffer.bits.rs1)
  deqInuseRs2 := inuseRegsCurrent(io.ibuffer.bits.wid)(io.ibuffer.bits.rs2)

  io.writeback.ready := true.B
  io.ibuffer.ready := !(deqInuseRd || deqInuseRs1 || deqInuseRs2)

  // Check and assert if any deadlock is detected
  val deadlockCtr = RegInit(0.U(32.W))
  val deadlockTimeout = 100000.U

  when(io.ibuffer.valid && !io.ibuffer.ready) {
    deadlockCtr := deadlockCtr + 1.U
    assert(
      deadlockCtr < deadlockTimeout,
      cf"Deadlock detected - PC: 0x${Hexadecimal(io.ibuffer.bits.pc)}, wid: ${io.ibuffer.bits.wid}, rd: ${io.ibuffer.bits.rd}"
    )
  }.elsewhen(io.ibuffer.valid && io.ibuffer.ready) {
    deadlockCtr := 0.U
  }.elsewhen(io.writeback.valid && io.writeback.ready && io.writeback.bits.eop) {
    assert(
      inuseRegs(io.writeback.bits.wid)(io.writeback.bits.rd),
      cf"Invalid writeback register - PC: 0x${Hexadecimal(io.writeback.bits.pc)}, wid: ${io.writeback.bits.wid}, rd: ${io.writeback.bits.rd}"
    )
  }
}
