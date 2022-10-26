package hardsfp

import spinal.core._
import spinal.lib._

object CodeGenConfig
    extends SpinalConfig(
      mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW),
      targetDirectory = "rtl"
    )

object CodeGen {
  def main(args: Array[String]): Unit = {
    CodeGenConfig.generate(SFPMul(3, 3))
  }
}
