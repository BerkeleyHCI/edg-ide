package edg_ide.ui
import edg.compiler.{ExprValue, FloatValue, IntValue, RangeValue}
import edg_ide.util.SiPrefixUtil

object ParamToUnitsStringUtil {
  private val TOLERANCE_THRESHOLD = 0.25

  def paramToUnitsString(value: ExprValue, units: String): String = {
    value match { // ints not parsed like this to avoid loss of precision
      case FloatValue(value) => SiPrefixUtil.unitsToString(value, units)
      case RangeValue(minValue, maxValue) =>
        val centerValue = (minValue + maxValue) / 2
        if (centerValue != 0) {
          val tolerance = (centerValue - minValue) / centerValue
          if (math.abs(tolerance) <= TOLERANCE_THRESHOLD) { // within tolerance, display as center + tol
            f"${SiPrefixUtil.unitsToString(centerValue, units)} ± ${(tolerance * 100)}%.02f%%"
          } else { // out of tolerance, display as ranges
            s"(${SiPrefixUtil.unitsToString(minValue, units)}, ${SiPrefixUtil.unitsToString(maxValue, units)})"
          }
        } else {
          s"±${SiPrefixUtil.unitsToString(maxValue, units)}"
        }
      case value => s"unexpected ${value.getClass}(${value.toStringValue})"
    }
  }

  def toString(value: ExprValue): String = {
    value match {
      case FloatValue(_)    => paramToUnitsString(value, "")
      case RangeValue(_, _) => paramToUnitsString(value, "")
      case _                => value.toStringValue
    }
  }
}
