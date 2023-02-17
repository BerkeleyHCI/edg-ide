package edg_ide.ui
import edg.compiler.{Compiler, FloatValue, IntValue, RangeValue}
import edg.wir.IndirectDesignPath
import edg_ide.util.SiPrefixUtil

object ParamToUnitsStringUtil {
  private val TOLERANCE_THRESHOLD = 0.25

  def paramToUnitsString(path: IndirectDesignPath, units: String, compiler: Compiler): String = {
    compiler.getParamValue(path) match {
      case Some(FloatValue(value)) => SiPrefixUtil.unitsToString(value, units)
      case Some(IntValue(value)) => SiPrefixUtil.unitsToString(value.toDouble, units)
      case Some(RangeValue(minValue, maxValue)) =>
        val centerValue = (minValue + maxValue) / 2
        if (centerValue != 0) {
          val tolerance = (centerValue - minValue) / centerValue
          if (math.abs(tolerance) <= TOLERANCE_THRESHOLD) {  // within tolerance, display as center + tol
            f"${SiPrefixUtil.unitsToString(centerValue, units)} ± ${(tolerance*100)}%.02f%%"
          } else {  // out of tolerance, display as ranges
            s"(${SiPrefixUtil.unitsToString(minValue, units)}, ${SiPrefixUtil.unitsToString(maxValue, units)})"
          }
        } else {
          s"±${SiPrefixUtil.unitsToString(maxValue, units)}"
        }
      case Some(value) => s"unexpected ${value.getClass}(${value.toStringValue})"
      case None => "unknown"
    }
  }
}
