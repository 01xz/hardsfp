package hardsfp

import spinal.core._
import spinal.lib._

case class SFPMulCore(expWidth: Int, fracWidth: Int) extends Component {
  val io = new Bundle {
    val a, b   = in(SFP(expWidth, fracWidth))
    val result = out(SFP(expWidth + 1, fracWidth + 1))
  }

  def getFracHi(x: Int) = (x >>> fracWidth) << (32 - fracWidth)
  def getFracLo(x: Int) = x << (32 - fracWidth)

  def fracBeforeRound(x: Int): Long = {
    val fracHi = (getFracHi(x) >>> 1) | 0x80000000L
    val fracLo = (getFracLo(x) >>> 1) | 0x80000000L
    return fracHi * fracLo
  }

  def getCarry(x: Int): Boolean = {
    val frac = (fracBeforeRound(x) >>> 32).toInt
    return (frac & 0x80000000) != 0
  }

  def getFrac(x: Int): Int = {
    val frac = (fracBeforeRound(x) >>> 32).toInt
    if (getCarry(x)) {
      return (frac << 1) >>> (32 - (fracWidth + 1))
    } else {
      return (frac << 2) >>> (32 - (fracWidth + 1))
    }
  }

  val carryLUT = Vec.tabulate(1 << 2 * fracWidth) { i: Int =>
    U(getCarry(i).toInt)
  }

  val fracLUT = Vec.tabulate(1 << 2 * fracWidth) { i: Int =>
    U(getFrac(i))
  }

  val carry = carryLUT(io.a.frac @@ io.b.frac)

  io.result.sign := io.a.sign ^ io.b.sign
  io.result.exp  := (io.a.exp +^ io.b.exp) + carry
  io.result.frac := fracLUT(io.a.frac @@ io.b.frac)
}

case class SFPMul(expWidth: Int, fracWidth: Int) extends Component {
  val io = new Bundle {
    val a, b   = in UInt (1 + expWidth + fracWidth bits)
    val result = out UInt (1 + expWidth + fracWidth + 2 bits)
  }.setName("")

  val sfpMulCore = SFPMulCore(expWidth, fracWidth)

  sfpMulCore.io.a.sign := io.a.msb
  sfpMulCore.io.b.sign := io.b.msb
  sfpMulCore.io.a.exp  := io.a(expWidth + fracWidth - 1 downto fracWidth)
  sfpMulCore.io.b.exp  := io.b(expWidth + fracWidth - 1 downto fracWidth)
  sfpMulCore.io.a.frac := io.a(fracWidth - 1 downto 0)
  sfpMulCore.io.b.frac := io.b(fracWidth - 1 downto 0)
  io.result := sfpMulCore.io.result.sign.asUInt @@ sfpMulCore.io.result.exp @@ sfpMulCore.io.result.frac
}
