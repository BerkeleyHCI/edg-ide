package edg_ide.swing.dse

import com.intellij.ui.JBColor
import edg_ide.swing.{ColorUtil, DrawAnchored}

import java.awt.event.{MouseAdapter, MouseEvent, MouseMotionAdapter, MouseWheelEvent, MouseWheelListener}
import java.awt.{Color, Dimension, Graphics, Rectangle}
import javax.swing.{JComponent, Scrollable}
import scala.collection.mutable


object JScatterPlot {
  type AxisType = Option[Seq[(Float, String)]]
}


/** Scatterplot widget with two numerical axes, with labels inside the plot.
  * Data is structured as (x, y, data) coordinates, with some arbitrary data attached to each point.
  *
  * TODO: support other ordinal axes?
  */
class JScatterPlot[ValueType] extends JComponent with Scrollable {
  /** Scatterplot data point. Value (arbitrary data associated with this point), X, Y are required,
    * others are optional with defaults.
    * This allows future extensibility with additional parameters, eg size or marks.
    */
  class Data(val value: ValueType, val x: Float, val y: Float,
             val color: Option[Color] = None, val tooltipText: Option[String] = None) {
  }

  // GUI constants
  private val kDefaultRangeMarginFactor = 1.1f  // factor to extend the default range by

  private val kPointSizePx = 4 // diameter in px
  private val kSnapDistancePx = 6 // distance (radius) to snap for a click
  private val kPointSelectedSizePx = 6 // diameter in px
  private val kPointHoverOutlinePx = 12 // diameter in px
  private val kPointHoverOutlineColor = JBColor.YELLOW

  private val kTickBrightness = 0.25
  private val kTickSpacingIntervals = Seq(1, 2, 5)
  private val kMinTickSpacingPx = 64  // min spacing between axis ticks, used to determine tick resolution
  private val kTickSizePx = 4

  // data state
  private var xAxis: JScatterPlot.AxisType = Some(Seq())  // if text labels are specified, instead of dynamic numbers
  private var yAxis: JScatterPlot.AxisType = Some(Seq())

  private var data: IndexedSeq[Data] = IndexedSeq()
  private var mouseOverIndices: Seq[Int] = Seq()  // sorted by increasing index
  private var selectedIndices: Seq[Int] = Seq()  // unsorted

  // UI state
  private var xRange = (-1.0f, 1.0f)
  private var yRange = (-1.0f, 1.0f)

  // multiply data by this to get screen coordinates
  private def dataScale(dataRange: (Float, Float), screenSize: Int): Float = {
    if (dataRange._1 != dataRange._2) {
      screenSize / (dataRange._2 - dataRange._1)
    } else {
      1
    }
  }

  private def dataToScreenX(dataVal: Float): Int = ((dataVal - xRange._1) * dataScale(xRange, getWidth)).toInt
  private def dataToScreenY(dataVal: Float): Int = ((yRange._2 - dataVal) * dataScale(yRange, getHeight)).toInt

  def setData(xys: IndexedSeq[Data], xAxis: JScatterPlot.AxisType = None, yAxis: JScatterPlot.AxisType = None): Unit = {
    data = xys
    mouseOverIndices = Seq()  // clear
    selectedIndices = Seq()  // clear

    def expandedRange(range: (Float, Float), factor: Float): (Float, Float) = {
      val span = range._2 - range._1
      val expansion = if (span > 0) {  // range units to expand on each side
        span * (factor - 1) / 2
      } else {  // if span is empty, arbitrarily expand by 1 on each side and center the data
        1
      }
      (range._1 - expansion, range._2 + expansion)
    }
    val xs = data.map(_.x)
    xRange = expandedRange(((xs :+ 0f).min, (xs :+ 0f).max), kDefaultRangeMarginFactor)
    val ys = data.map(_.y)
    yRange = expandedRange(((ys :+ 0f).min, (ys :+ 0f).max), kDefaultRangeMarginFactor)

    this.xAxis = xAxis
    this.yAxis = yAxis

    validate()
    repaint()
  }

  // Sets the selected data, which is rendered with a highlight.
  // Unlike the other functions, this just takes values for simplicity and does not require X/Y/etc data.
  // Values not in rendered data are ignored.
  def setSelected(values: Seq[ValueType]): Unit = {
    selectedIndices = values flatMap { value =>
      data.indexWhere(_.value == value) match {
        case index if (index >= 0) => Some(index)
        case _ => None  // ignored
      }
    }

    validate()
    repaint()
  }

  // Returns all the axis ticks given some scale, screen origin, screen size, and min screen spacing
  private def getAxisTicks(range: (Float, Float), screenSize: Int, minScreenSpacing: Int): Seq[(Float, String)] = {
    val minDataSpacing = math.abs(minScreenSpacing / dataScale(range, screenSize))  // min tick spacing in data units
    val tickSpacings = kTickSpacingIntervals.map { factor =>  // try all the spacings and take the minimum
      math.pow(10, math.log10(minDataSpacing / factor).ceil) * factor
    }
    val tickSpacing = tickSpacings.min

    var tickPos = (math.floor(range._1 / tickSpacing) * tickSpacing).toFloat
    val ticksBuilder = mutable.ArrayBuffer[(Float, String)]()
    while (tickPos <= range._2) {
      ticksBuilder.append((tickPos, f"$tickPos%.02g"))
      tickPos = (tickPos + tickSpacing).toFloat
    }
    ticksBuilder.toSeq
  }

  private def paintAxes(paintGraphics: Graphics): Unit = {
    if (xAxis.isEmpty) { // left vertical axis - only on numeric axis
      val screenOriginX = dataToScreenX(0)
      paintGraphics.drawLine(screenOriginX, 0, screenOriginX, getHeight - 1)
    }
    val xTicks = xAxis match {
      case Some(xAxis) => xAxis
      case _ => getAxisTicks(xRange, getWidth, kMinTickSpacingPx)
    }
    xTicks.foreach { case (tickPos, tickVal) =>
      val screenX = dataToScreenX(tickPos)
      paintGraphics.drawLine(screenX, getHeight - 1, screenX, getHeight - 1 - kTickSizePx)
      DrawAnchored.drawLabel(paintGraphics, tickVal,
        (screenX, getHeight - 1 - kTickSizePx), DrawAnchored.Bottom)
    }

    if (yAxis.isEmpty) { // bottom horizontal axis - only on numeric axis
      val screenOriginY = dataToScreenY(0)
      paintGraphics.drawLine(0, screenOriginY, getWidth - 1, screenOriginY)
    }
    val yTicks = yAxis match {
      case Some(yAxis) => yAxis
      case _ => getAxisTicks(yRange, getHeight, kMinTickSpacingPx)
    }
    yTicks.foreach { case (tickPos, tickVal) =>
      val screenY = dataToScreenY(tickPos)
      paintGraphics.drawLine(0, screenY, kTickSizePx, screenY)
      DrawAnchored.drawLabel(paintGraphics, tickVal,
        (kTickSizePx, screenY), DrawAnchored.Left)
    }
  }

  private def paintData(paintGraphics: Graphics): Unit = {
    data.zipWithIndex.foreach { case (data, index) =>
      val dataGraphics = paintGraphics.create()
      data.color.foreach { color =>  // if color is specified, set the color
        dataGraphics.setColor(color)
      }
      val screenX = dataToScreenX(data.x)
      val screenY = dataToScreenY(data.y)

      if (mouseOverIndices.contains(index) || selectedIndices.contains(index)) { // mouseover: highlight
        val hoverGraphics = paintGraphics.create()
        hoverGraphics.setColor(ColorUtil.blendColor(getBackground, kPointHoverOutlineColor, 0.5))
        hoverGraphics.fillOval(screenX - kPointHoverOutlinePx / 2, screenY - kPointHoverOutlinePx / 2,
          kPointHoverOutlinePx, kPointHoverOutlinePx)
      }
      if (selectedIndices.contains(index)) { // selected: thicker
        dataGraphics.fillOval(screenX - kPointSelectedSizePx / 2, screenY - kPointSelectedSizePx / 2,
          kPointSelectedSizePx, kPointSelectedSizePx)
      } else {
        dataGraphics.fillOval(screenX - kPointSizePx / 2, screenY - kPointSizePx / 2, kPointSizePx, kPointSizePx)
      }
    }
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val axesGraphics = paintGraphics.create()
    axesGraphics.setColor(ColorUtil.blendColor(getBackground, paintGraphics.getColor, kTickBrightness))
    paintAxes(axesGraphics)

    paintData(paintGraphics)
  }

  // Returns the points with some specified distance (in screen coordinates, px) of the point.
  // Returns as (index of point, distance)
  def getPointsForLocation(x: Int, y: Int, maxDistance: Int): Seq[(Int, Float)] = {
    data.zipWithIndex.flatMap { case (data, index) =>
      val xDist = dataToScreenX(data.x) - x
      val yDist = dataToScreenY(data.y) - y
      val distance = math.sqrt(xDist * xDist + yDist * yDist).toFloat
      if (distance <= maxDistance) {
        Some(index, distance)
      } else {
        None
      }
    }
  }

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val clickedPoints = getPointsForLocation(e.getX, e.getY, kSnapDistancePx)
      onClick(clickedPoints.sortBy(_._2).map(pair => data(pair._1)))
    }
  })

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)
      val newPoints = getPointsForLocation(e.getX, e.getY, kSnapDistancePx)
      val newIndices = newPoints.map(_._1)
      if (mouseOverIndices != newIndices) {
        mouseOverIndices = newIndices
        validate()
        repaint()

        // sort by distance and call hover
        onHoverChange(newPoints.sortBy(_._2).map(pair => data(pair._1)))
      }
    }
  })

  addMouseWheelListener(new MouseWheelListener {
    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      // calculates a new range after applying some scaling factor, but keeping some fractional point of
      // the old and new range static (eg, the point the mouse is over)
      def calculateNewRange(oldRange: (Float, Float), scaleFactor: Float, staticFrac: Float): (Float, Float) = {
        val span = oldRange._2 - oldRange._1
        val mouseValue = oldRange._1 + (span * staticFrac)
        val newSpan = span * scaleFactor
        (mouseValue - (newSpan * staticFrac), mouseValue + (newSpan * (1 - staticFrac)))
      }

      val zoomFactor = Math.pow(1.1, 1 * e.getPreciseWheelRotation).toFloat
      xRange = calculateNewRange(xRange, zoomFactor, e.getX.toFloat / getWidth)
      yRange = calculateNewRange(yRange, zoomFactor, 1 - (e.getY.toFloat / getHeight))

      validate()
      repaint()
    }
  })

  override def getToolTipText(e: MouseEvent): String = {
    getPointsForLocation(e.getX, e.getY, kSnapDistancePx).headOption match {
      case Some((index, distance)) => data(index).tooltipText.orNull
      case None => null
    }
  }

  // User hooks - can be overridden
  //
  // called when this widget clicked, for all points within some hover radius of the cursor
  // sorted by distance from cursor (earlier = closer), and may be empty
  def onClick(data: Seq[Data]): Unit = { }

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(data: Seq[Data]): Unit = { }


  override def getPreferredScrollableViewportSize: Dimension = getPreferredSize

  // Scrollable APIs
  //
  override def getPreferredSize: Dimension = new Dimension(100, 100)  // TODO arbitrary

  override def getScrollableBlockIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableUnitIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableTracksViewportWidth: Boolean = false

  override def getScrollableTracksViewportHeight: Boolean = false
}
