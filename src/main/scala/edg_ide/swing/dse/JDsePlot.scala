package edg_ide.swing.dse

import com.intellij.ui.JBColor
import edg.compiler.FloatValue
import edg_ide.ui.ParamToUnitsStringUtil

import java.awt.Color
import scala.collection.mutable

object JDsePlot {
  // GUI constants
  private val kDefaultRangeMarginFactor = 1.2f // factor to extend the default range by

  val kPointAlpha: Int = 191
  val kBackgroundBlend: Float = 0.25f
  val kBackgroundAlpha: Int = 127

  val kPointSizePx: Int = 8  // diameter in px
  val kSnapDistancePx: Int = kPointSizePx * 3/2 // distance (radius) to snap for a click
  val kPointSelectedSizePx: Int = kPointSizePx * 3/2 // diameter in px
  val kPointHoverOutlinePx: Int = kPointSizePx * 3 // diameter in px

  val kLinePx: Int = 4 // width in px
  val kLineHoverBackgroundPx: Int = kLinePx * 5 // width in px
  val kLineHoverOutlinePx: Int = kLinePx * 3 // width in px

  val kHoverOutlineColor: Color = JBColor.BLUE
  val kHoverOutlineBlend: Float = 0.5f

  val kDragSelectAlpha: Int = 63

  val kTickBrightness: Float = 0.25f
  val kTickSpacingIntervals: Seq[Int] = Seq(1, 2, 5)
  val kMinTickSpacingPx: Int = 96 // min spacing between axis ticks, used to determine tick resolution
  val kTickSizePx: Int = 4

  def defaultValuesRange(values: Seq[Float], factor: Float = kDefaultRangeMarginFactor): (Float, Float) = {
    val rangingValues = values.filter { value => // ignore invalid values for ranging
      value != Float.NegativeInfinity && value != Float.PositiveInfinity && value != Float.NaN
    }
    val range = if (rangingValues.nonEmpty) {
      (rangingValues.min, rangingValues.max) // 0 in case values is empty, and forces the scale to include 0
    } else {
      (0f, 0f)
    }
    val span = range._2 - range._1
    val expansion = if (span > 0) { // range units to expand on each side
      span * (factor - 1) / 2
    } else { // if span is empty, arbitrarily expand by 1 on each side and center the data
      1
    }
    (range._1 - expansion, range._2 + expansion)
  }

  // calculates a new range after applying some scaling factor, but keeping some fractional point of
  // the old and new range static (eg, the point the mouse is over)
  def scrollNewRange(oldRange: (Float, Float), scaleFactor: Float, staticFrac: Float): (Float, Float) = {
    val span = oldRange._2 - oldRange._1
    val mouseValue = oldRange._1 + (span * staticFrac)
    val newSpan = span * scaleFactor
    (mouseValue - (newSpan * staticFrac), mouseValue + (newSpan * (1 - staticFrac)))
  }

  // multiply data by this to get screen coordinates
  def dataScale(dataRange: (Float, Float), screenSize: Int): Float = {
    if (dataRange._1 != dataRange._2) {
      screenSize / (dataRange._2 - dataRange._1)
    } else {
      1
    }
  }

  def orderedValues(v1: Int, v2: Int): (Int, Int) = {
    (math.min(v1, v2), math.max(v1, v2))
  }

  def orderedValues(v1: Float, v2: Float): (Float, Float) = {
    (math.min(v1, v2), math.max(v1, v2))
  }

  // Returns all the axis ticks given some scale, screen origin, screen size, and min screen spacing
  def getAxisTicks(
      range: (Float, Float),
      screenSize: Int,
      minScreenSpacing: Int = kMinTickSpacingPx
  ): Seq[(Float, String)] = {
    val minDataSpacing =
      math.abs(minScreenSpacing / dataScale(range, screenSize)) // min tick spacing in data units
    val tickSpacings = kTickSpacingIntervals.map { factor => // try all the spacings and take the minimum
      math.pow(10, math.log10(minDataSpacing / factor).ceil) * factor
    }
    val tickSpacing = tickSpacings.min

    var tickPos = (math.floor(range._1 / tickSpacing) * tickSpacing).toFloat
    val ticksBuilder = mutable.ArrayBuffer[(Float, String)]()
    while (tickPos <= range._2) {
      ticksBuilder.append((tickPos, ParamToUnitsStringUtil.toString(FloatValue(tickPos))))
      tickPos = (tickPos + tickSpacing).toFloat
    }
    ticksBuilder.toSeq
  }
}
