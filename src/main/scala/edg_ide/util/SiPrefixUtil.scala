package edg_ide.util

import java.math.MathContext

object SiPrefixUtil {
  private val PREFIXES_POW3_HIGH = Seq("k", "M", "G", "T", "P", "E", "Z", "Y")
  private val PREFIXES_POW3_LOW = Seq("m", "μ", "n", "p", "f", "a", "z", "y")

  // prepends a space if the string is nonempty
  // used to add a space between the number and units, except when it's dimensionless (no units) and no SI prefix
  private def prependSpaceNonempty(str: String): String = {
    if (str.nonEmpty) {
      " " + str
    } else {
      str
    }
  }

  def unitsToString(value: Double, units: String): String = {
    if (value.isPosInfinity) {
      s"+∞ $units"
    } else if (value.isNegInfinity) {
      s"-∞ $units"
    } else {
      val roundedValue = BigDecimal(value).round(new MathContext(3)).doubleValue

      val pwr3 = math.floor(math.log10(math.abs(roundedValue)) / 3).toInt
      if (pwr3 > PREFIXES_POW3_HIGH.length || pwr3 < PREFIXES_POW3_LOW.length * -1 || pwr3 == 0) {
        // Out of bounds or no prefix, render as scientific notation
        f"$roundedValue%.03g${prependSpaceNonempty(units)}"
      } else if (pwr3 > 0) {
        // positive power (value >= 1)
        val prefix = PREFIXES_POW3_HIGH(pwr3 - 1)
        val valuePrefixed = roundedValue / math.pow(10, 3 * pwr3)
        f"$valuePrefixed%.03g${prependSpaceNonempty(prefix + units)}"
      } else {
        // negative power (value < 1)
        val prefix = PREFIXES_POW3_LOW(-pwr3 - 1)
        val valuePrefixed = roundedValue / math.pow(10, 3 * pwr3)
        f"$valuePrefixed%.03g${prependSpaceNonempty(prefix + units)}"
      }
    }
  }
}
