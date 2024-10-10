package ogpu.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import ogpu.config._
import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.rocket.{SZ_DW, DW_64, DW_32}

class ScalarALU(
  implicit p: Parameters)
    extends Module {
  val xLen = p(XLen)
  val io = IO(new Bundle() {
    val dw = Input(UInt(SZ_DW.W))
    val fn = Input(UInt(SZ_ALU_FN.W))
    val in1 = Input(UInt(xLen.W))
    val in2 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
    val adder_out = Output(UInt(xLen.W))
    val cmp_out = Output(Bool())
  })

  override def desiredName = "CHIPALU"

  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  val in1_and_in2 = io.in1 & in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)

  // SLT, SLTU
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === 0.U, slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4,0), io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  val shin = Mux(shiftReverse(io.fn), Reverse(shin_r), shin_r)
  val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA || io.fn === FN_BEXT, shout_r, 0.U) |
              Mux(io.fn === FN_SL,                                          shout_l, 0.U)

  // CZEQZ, CZNEZ
  val in2_not_zero = io.in2.orR
  val usingConditionalZero = true
  val cond_out = Option.when(usingConditionalZero)(
    Mux((io.fn === FN_CZEQZ && in2_not_zero) || (io.fn === FN_CZNEZ && !in2_not_zero), io.in1, 0.U)
  )

  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR || io.fn === FN_ORN || io.fn === FN_XNOR, in1_xor_in2, 0.U) |
              Mux(io.fn === FN_OR || io.fn === FN_AND || io.fn === FN_ORN || io.fn === FN_ANDN, in1_and_in2, 0.U)

  val useZbs = true
  val bext_mask = Mux(useZbs.B && io.fn === FN_BEXT, 1.U, ~(0.U(xLen.W)))
  val shift_logic = (isCmp (io.fn) && slt) | logic | (shout & bext_mask)
  val shift_logic_cond = cond_out match {
    case Some(co) => shift_logic | co
    case _ => shift_logic
  }

  // CLZ, CTZ, CPOP
  val tz_in = MuxLookup((io.dw === DW_32) ## !io.in2(0), 0.U)(Seq(
    0.U -> io.in1,
    1.U -> Reverse(io.in1),
    2.U -> 1.U ## io.in1(31,0),
    3.U -> 1.U ## Reverse(io.in1(31,0))
  ))
  val popc_in = Mux(io.in2(1),
    Mux(io.dw === DW_32, io.in1(31,0), io.in1),
    PriorityEncoderOH(1.U ## tz_in) - 1.U)(xLen-1,0)
  val count = PopCount(popc_in)
  val in1_bytes = io.in1.asTypeOf(Vec(xLen / 8, UInt(8.W)))
  val orcb = VecInit(in1_bytes.map(b => Fill(8, b =/= 0.U))).asUInt
  val rev8 = VecInit(in1_bytes.reverse).asUInt
  val unary = MuxLookup(io.in2(11,0), count)(Seq(
    0x287.U -> orcb,
    (if (xLen == 32) 0x698 else 0x6b8).U -> rev8,
    0x080.U -> io.in1(15,0),
    0x604.U -> Fill(xLen-8, io.in1(7)) ## io.in1(7,0),
    0x605.U -> Fill(xLen-16, io.in1(15)) ## io.in1(15,0)
  ))

  // MAX, MIN, MAXU, MINU
  val maxmin_out = Mux(io.cmp_out, io.in2, io.in1)

  // ROL, ROR
  val rot_shamt = Mux(io.dw === DW_32, 32.U, xLen.U) - shamt
  val rotin = Mux(io.fn(0), shin_r, Reverse(shin_r))
  val rotout_r = (rotin >> rot_shamt)(xLen-1,0)
  val rotout_l = Reverse(rotout_r)
  val rotout = Mux(io.fn(0), rotout_r, rotout_l) | Mux(io.fn(0), shout_l, shout_r)

  val useZbb = true
  val out = MuxLookup(io.fn, shift_logic_cond)(Seq(
    FN_ADD -> io.adder_out,
    FN_SUB -> io.adder_out
  ) ++ (if (useZbb) Seq(
    FN_UNARY -> unary,
    FN_MAX -> maxmin_out,
    FN_MIN -> maxmin_out,
    FN_MAXU -> maxmin_out,
    FN_MINU -> maxmin_out,
    FN_ROL -> rotout,
    FN_ROR -> rotout,
  ) else Nil))


  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}

// object ALURTL extends App {
//   implicit val p = new CoreConfig
//   emitVerilog(new ScalarALU(), Array("--target-dir", "generated"))
// }
