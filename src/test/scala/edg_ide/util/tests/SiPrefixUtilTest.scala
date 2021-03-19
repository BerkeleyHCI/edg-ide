package edg_ide.util.tests

import edg_ide.util.SiPrefixUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SiPrefixUtilTest extends AnyFlatSpec with Matchers {
  behavior of "SiPrefixUtil"

  it should "display no prefixes with values [1, 1000)" in {
    SiPrefixUtil.unitsToString(1, "A") should equal("1.00 A")
    SiPrefixUtil.unitsToString(1.5, "A") should equal("1.50 A")
    SiPrefixUtil.unitsToString(1.25, "A") should equal("1.25 A")
    SiPrefixUtil.unitsToString(10, "A") should equal("10.0 A")
    SiPrefixUtil.unitsToString(15, "A") should equal("15.0 A")
    SiPrefixUtil.unitsToString(15.5, "A") should equal("15.5 A")
    SiPrefixUtil.unitsToString(100, "A") should equal("100 A")
    SiPrefixUtil.unitsToString(140, "A") should equal("140 A")
    SiPrefixUtil.unitsToString(142, "A") should equal("142 A")

    SiPrefixUtil.unitsToString(999, "A") should equal("999 A")
    SiPrefixUtil.unitsToString(999.4, "A") should equal("999 A")
    SiPrefixUtil.unitsToString(999.49, "A") should equal("999 A")
  }

  it should "display prefixes with values >= 1k" in {
    SiPrefixUtil.unitsToString(1e3, "A") should equal("1.00 kA")
    SiPrefixUtil.unitsToString(1.5e3, "A") should equal("1.50 kA")
    SiPrefixUtil.unitsToString(142e3, "A") should equal("142 kA")

    SiPrefixUtil.unitsToString(8e6, "A") should equal("8.00 MA")
    SiPrefixUtil.unitsToString(6e9, "A") should equal("6.00 GA")
    SiPrefixUtil.unitsToString(42e12, "A") should equal("42.0 TA")
    SiPrefixUtil.unitsToString(999e24, "A") should equal("999 YA")
    SiPrefixUtil.unitsToString(999.49e24, "A") should equal("999 YA")
  }

  it should "display prefixes with values < 1" in {
    SiPrefixUtil.unitsToString(1e-3, "A") should equal("1.00 mA")
    SiPrefixUtil.unitsToString(1.5e-3, "A") should equal("1.50 mA")
    SiPrefixUtil.unitsToString(142e-3, "A") should equal("142 mA")

    SiPrefixUtil.unitsToString(8e-6, "A") should equal("8.00 μA")
    SiPrefixUtil.unitsToString(6e-9, "A") should equal("6.00 nA")
    SiPrefixUtil.unitsToString(42e-12, "A") should equal("42.0 pA")
    SiPrefixUtil.unitsToString(1e-24, "A") should equal("1.00 yA")
    SiPrefixUtil.unitsToString(999e-24, "A") should equal("999 yA")
    SiPrefixUtil.unitsToString(999.49e-24, "A") should equal("999 yA")
  }

  it should "display scientific notation for out of range values" in {
    SiPrefixUtil.unitsToString(1e27, "A") should equal("1.00e+27 A")
    SiPrefixUtil.unitsToString(4e30, "A") should equal("4.00e+30 A")
    SiPrefixUtil.unitsToString(1e-27, "A") should equal("1.00e-27 A")
    SiPrefixUtil.unitsToString(4e-30, "A") should equal("4.00e-30 A")
  }

  it should "handle infinity" in {
    SiPrefixUtil.unitsToString(Double.PositiveInfinity, "A") should equal("+∞ A")
    SiPrefixUtil.unitsToString(Double.NegativeInfinity, "A") should equal("-∞ A")
  }

  it should "display prefixes accounting for rounding" in {
    SiPrefixUtil.unitsToString(999.9, "A") should equal("1.00 kA")
    SiPrefixUtil.unitsToString(999.9e24, "A") should equal("1.00e+27 A")
    SiPrefixUtil.unitsToString(0.9999e-24, "A") should equal("1.00 yA")

    SiPrefixUtil.unitsToString(999.5, "A") should equal("1.00 kA")
    SiPrefixUtil.unitsToString(999.5e24, "A") should equal("1.00e+27 A")
    SiPrefixUtil.unitsToString(0.9995e-24, "A") should equal("1.00 yA")
  }
}
