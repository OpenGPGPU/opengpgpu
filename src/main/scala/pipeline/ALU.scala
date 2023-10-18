/*
 * Copyright (c) 2023 OpenGPGPU
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package opengpgpu.pipeline

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import opengpgpu.config._
import freechips.rocketchip.rocket._

class ScalarALU(implicit p: Parameters) extends Module {
  val xLen = p(XLen)
  val aluFn = p(ALUFunc)
  val io = IO(new Bundle() {
    val fn = Input(UInt(aluFn.SZ_ALU_FN.W))
    val in1 = Input(UInt(xLen.W))
    val in2 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
    val adder_out = Output(UInt(xLen.W))
    val cmp_out = Output(Bool())
  })
  val isSub = aluFn.isSub(io.fn)
  val isIn2Inv = aluFn.isIn2Inv(io.fn)
  val isZBS = aluFn.isZBS(io.fn)
  val isUW = aluFn.isUW(io.fn)
  val isSRA = aluFn.isSRA(io.fn)
  val isRotate = aluFn.isRotate(io.fn)
  val isLeft = aluFn.isLeft(io.fn)
  val isLeftZBS = aluFn.isLeftZBS(io.fn)
  val isCZ = aluFn.isCZ(io.fn)
  val isBCLR = aluFn.isBCLR(io.fn)
  val isCZBCLR = aluFn.isCZBCLR(io.fn)
  val isCZZBS = aluFn.isCZZBS(io.fn)
  val isUnsigned = aluFn.isUnsigned(io.fn)
  val isInverted = aluFn.isInverted(io.fn)
  val isSEQSNE = aluFn.isSEQSNE(io.fn)
  val isSEXT = aluFn.isSEXT(io.fn)
  val isORC = aluFn.isORC(io.fn)
  val shxadd1H = aluFn.shxadd1H(io.fn)
  val out1H = aluFn.out1H(io.fn)

  // process input
  // used by SUB, ANDN, ORN, XNOR
  val in2_inv = Mux(isIn2Inv, ~io.in2, io.in2)
  val shamt = io.in2(4, 0)
  val in1_ext = io.in1

  // one arm: SL, ROL, SLLIUW, CLZ
  // another arm: SR, SRA, ROR, CTZ, ADD, SUB, ZBS
  // note that CLZW is not included here
  // in1 capable of right hand operation
  // isLeft
  val in1_r = Mux(isLeft, Reverse(in1_ext), in1_ext)

  // shifter
  val shin = Mux(isZBS, (BigInt(1) << 31).U(32.W), in1_r)
  // TODO: Merge shift and rotate (manual barrel or upstream to Chisel)
  val shout_r = (Cat(isSRA & shin(xLen - 1), shin).asSInt >> shamt)(xLen - 1, 0)
  val roout_r = shin.rotateRight(shamt)(xLen - 1, 0)
  val shro_r = Mux(isRotate, roout_r, shout_r)
  // one arm: SL, ROL, SLLIUW, ZBS
  // another arm: SR, SRA, ROR
  val shro = Mux(isLeftZBS, Reverse(shro_r), shro_r)

  // adder
  val adder_in1 =
    Mux1H(shxadd1H, Seq(in1_r, (in1_ext << 1)(xLen - 1, 0), (in1_ext << 2)(xLen - 1, 0), (in1_ext << 3)(xLen - 1, 0)))
  // out = in1 - 1 when isCLZ/isCTZ
  // note that when isCZ, isSub is 0 as ~0 = ~1+1 = -1
  val adder_in2 = Mux(isCZ, ~0.U(xLen.W), in2_inv)
  // adder_out = adder_in1 + adder_in2 + isSub
  val adder_out = (Cat(adder_in1, 1.U(1.W)) + Cat(adder_in2, isSub))(xLen, 1)
  io.adder_out := adder_out

  // logic
  // AND, OR, XOR
  // ANDN, ORN, XNOR
  // BCLR, BEXT, BINV, BSET
  val out_inv = Mux(isCZBCLR, ~Mux(isBCLR, shro, adder_out), shro)
  val logic_in2 = Mux(isCZZBS, out_inv, in2_inv)
  // also BINV
  val xor = adder_in1 ^ logic_in2
  // also BCLR
  val and = adder_in1 & logic_in2
  // also BSET
  val or = adder_in1 | logic_in2
  val bext = and.orR

  // SLT, SLTU
  // BEQ, BNE, BLT, BGE
  // MAX, MIN
  val slt =
    Mux(io.in1(xLen - 1) === io.in2(xLen - 1), adder_out(xLen - 1), Mux(isUnsigned, io.in2(xLen - 1), io.in1(xLen - 1)))
  val cmp = isInverted ^ Mux(isSEQSNE, ~(xor.orR), slt)
  io.cmp_out := cmp
  // MAX, MAXU, MIN, MINU
  val max_min = Mux(cmp, io.in2, io.in1)

  // counter
  // CLZ, CPOP, CTZ
  val cpop = PopCount(io.in1)
  // ctz_in = ~adder_out & adder_in1 // all zero or one hot
  val ctz_in = and
  val ctz_out = Cat(
    ~ctz_in.orR,
    VecInit(
      (0 to log2Ceil(xLen) - 1)
        .map(x => {
          val bits = ctz_in.asBools.zipWithIndex
          VecInit(
            bits
              filter { case (_, i) => i % (1 << (x + 1)) >= (1 << x) }
              map { case (b, _) => b }
          ).asUInt.orR
        })
        .toSeq
    ).asUInt
  )

  // ZEXT/SEXT
  val exth = Cat(Fill(xLen - 16, Mux(isSEXT, io.in1(15), 0.U)), io.in1(15, 0))
  val extb = Cat(Fill(xLen - 8, io.in1(7)), io.in1(7, 0))

  // REV/ORC
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  val in1_bytes = asBytes(io.in1)
  val rev8 = VecInit(in1_bytes.reverse.toSeq).asUInt
  val orc_brev8 = VecInit(
    in1_bytes
      .map(x => {
        val orc = Mux(x.orR, 0xff.U(8.W), 0.U(8.W))
        // BREV8 only in Zbk
        orc
      })
      .toSeq
  ).asUInt

  // pack
  def sext(in: UInt): UInt = {
    val in_hi_32 = Fill(32, in(31))
    Cat(in_hi_32, in)
  }
  val pack = 0.U
  val packh = 0.U

  // zip
  val zip = 0.U
  val unzip = 0.U

  val out = Mux1H(
    out1H,
    Seq(
      adder_out,
      shro,
      and,
      xor,
      //
      or,
      bext,
      cmp,
      max_min,
      //
      cpop,
      ctz_out,
      exth,
      extb,
      //
      rev8,
      orc_brev8,
      pack,
      packh,
      //
      zip,
      unzip
    )
  )

  val out_w = out
  io.out := out_w

}

object ALURTL extends App {
  implicit val p = new CoreConfig
  emitVerilog(new ScalarALU(), Array("--target-dir", "generated"))
}
