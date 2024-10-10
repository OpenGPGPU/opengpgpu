package ogpu.util

import chisel3.util._

object VaddrHelper {

  def vaddrBits(xlen: Int, pglevel: Int, pgsize: Int, hvbits: Int): Int = {
    val pgLevelBits = 10 - log2Ceil(xlen / 32)
    val maxVAddrBits = pgLevelBits * pglevel + log2Ceil(pgsize)
    maxVAddrBits + hvbits
  }

}
