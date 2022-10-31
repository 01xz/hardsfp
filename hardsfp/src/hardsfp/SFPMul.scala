package hardsfp

import spinal.core._
import spinal.lib._

case class SFPMulCore(expWidth: Int, fracWidth: Int) extends Component {
  val io = new Bundle {
    val a, b   = in(SFP(expWidth, fracWidth))
    val zero   = out Bool ()
    val result = out(SFP(expWidth + 1, fracWidth + 1))
  }

  private def getExpHi(x: Int) = x >>> expWidth
  private def getExpLo(x: Int) = x & (0xffffffff >>> (32 - expWidth))

  private def getFracHi(x: Int) = (x >>> fracWidth) << (32 - fracWidth)
  private def getFracLo(x: Int) = x << (32 - fracWidth)

  private def fracBeforeRound(x: Int): Int = {
    val fracHi = (getFracHi(x) >>> 1) | 0x80000000L
    val fracLo = (getFracLo(x) >>> 1) | 0x80000000L
    return (fracHi * fracLo >>> 32).toInt
  }

  private def fracWithRound(x: Int): Int = {
    val frac = fracBeforeRound(x)
    if (frac < 0) {
      return frac + (0x80000000 >>> (fracWidth + 2))
    } else {
      return frac + (0x80000000 >>> (fracWidth + 3))
    }
  }

  private def checkZero(x: Int): Boolean =
    (getExpHi(x) == 0) | (getExpLo(x) == 0)

  private def getCarry(x: Int) = fracWithRound(x) < 0

  private def getFrac(x: Int): Int = {
    val frac = fracWithRound(x)
    if (frac < 0) {
      return (frac << 1) >>> (32 - (fracWidth + 1))
    } else {
      return (frac << 2) >>> (32 - (fracWidth + 1))
    }
  }

  val zeroLUT = Vec.tabulate(1 << 2 * expWidth) { i: Int =>
    Bool(checkZero(i))
  }

  val carryLUT = Vec.tabulate(1 << 2 * fracWidth) { i: Int =>
    U(getCarry(i).toInt)
  }

  val fracLUT = Vec.tabulate(1 << 2 * fracWidth) { i: Int =>
    U(getFrac(i))
  }

  val carry = carryLUT(io.a.frac @@ io.b.frac)

  io.zero        := zeroLUT(io.a.exp @@ io.b.exp)
  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp  := io.a.exp +^ io.b.exp + carry
  io.result.frac := fracLUT(io.a.frac @@ io.b.frac)
}

case class SFPMul(expWidth: Int, fracWidth: Int) extends Component {
  val io = new Bundle {
    val a, b   = in UInt (1 + expWidth + fracWidth bits)
    val result = out UInt (1 + expWidth + fracWidth + 2 bits)
  }.setName("")

  val mul = SFPMulCore(expWidth, fracWidth)

  mul.io.a.sign := io.a.msb
  mul.io.b.sign := io.b.msb
  mul.io.a.exp  := io.a(expWidth + fracWidth - 1 downto fracWidth)
  mul.io.b.exp  := io.b(expWidth + fracWidth - 1 downto fracWidth)
  mul.io.a.frac := io.a(fracWidth - 1 downto 0)
  mul.io.b.frac := io.b(fracWidth - 1 downto 0)

  val areaRstWhenZero = new ResetArea(mul.io.zero, false) {
    val result = Reg(UInt(1 + expWidth + fracWidth + 2 bits)) init (0)
    result := mul.io.result.sign.asUInt @@ mul.io.result.exp @@ mul.io.result.frac
  }

  io.result := areaRstWhenZero.result
}
