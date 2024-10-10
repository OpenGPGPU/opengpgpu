package ogpu.core
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.lib._
import ogpu.config._

class SGPR(
  implicit p: Parameters)
    extends Module() {
  val numWarps = p(WarpNum)
  val numRegs = p(RegNum)
  val xLen = p(XLen)

  val io = IO(new Bundle {
    val writeback = Flipped(DecoupledIO(new CommitSData()))
    val writeback_cmd = Flipped(DecoupledIO(new CommitSData()))
    val read_req = Flipped(new ReadGPRReq())
    val read_rsp = new ReadSGPRRsp()
  })

  val gpr_ram = VecInit(Seq.fill(numWarps)((Module(new MaskedSmem_2R1W(xLen, numRegs, 1)).io)))
  val raddr_reg = RegInit(0.U)
  val raddr2_reg = RegInit(0.U)
  val rwid_reg = RegInit(0.U(log2Ceil(numWarps).W))
  val ready_reg = RegInit(0.B)
  val cmd_ready_reg = RegInit(0.B)

  raddr_reg := io.read_req.rs1
  raddr2_reg := io.read_req.rs2
  rwid_reg := io.read_req.wid

  io.writeback.ready := ready_reg
  io.writeback_cmd.ready := cmd_ready_reg

  for (i <- 0 until numWarps) {
    // init
    gpr_ram(i).write_en := 0.B
    gpr_ram(i).waddr := 0.U
    gpr_ram(i).raddr := io.read_req.rs1
    gpr_ram(i).raddr2 := io.read_req.rs2
    gpr_ram(i).mask := 0.U.asTypeOf(io.writeback.bits.mask)
    gpr_ram(i).dataIn := 0.U.asTypeOf(io.writeback.bits.data)

    when(io.writeback_cmd.valid && i.U === io.writeback_cmd.bits.wid) {
      gpr_ram(i).write_en := io.writeback_cmd.valid
      gpr_ram(i).waddr := io.writeback_cmd.bits.rd
      gpr_ram(i).mask := io.writeback_cmd.bits.mask
      gpr_ram(i).dataIn := io.writeback_cmd.bits.data
    }.elsewhen(io.writeback.valid && i.U === io.writeback.bits.wid) {
      gpr_ram(i).write_en := io.writeback.valid
      gpr_ram(i).waddr := io.writeback.bits.rd
      gpr_ram(i).dataIn := io.writeback.bits.data
    }

  }

  ready_reg := 0.B
  cmd_ready_reg := 0.B
  when(io.writeback_cmd.valid) {
    cmd_ready_reg := 1.B
  }.elsewhen(io.writeback.valid) {
    ready_reg := 1.B
  }

  io.read_rsp.rs1_data := Mux(raddr_reg === 0.U, 0.U.asTypeOf(gpr_ram(0).dataOut), gpr_ram(rwid_reg).dataOut)
  io.read_rsp.rs2_data := Mux(raddr2_reg === 0.U, 0.U.asTypeOf(gpr_ram(0).dataOut), gpr_ram(rwid_reg).dataOut2)
}
