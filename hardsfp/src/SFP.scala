package hardsfp

import spinal.core._
import spinal.lib._

case class SFP(expWidth: Int, fracWidth: Int) extends Bundle {
  val sign = Bool()
  val exp  = UInt(expWidth bits)
  val frac = UInt(fracWidth bits)
}
