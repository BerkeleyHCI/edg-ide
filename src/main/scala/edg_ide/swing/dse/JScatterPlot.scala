package edg_ide.swing.dse

import com.intellij.ui.JBColor
import edg_ide.swing.{ColorUtil, DrawAnchored}

import java.awt.event._
import java.awt.{Color, Graphics, Point}
import javax.swing.{JComponent, SwingUtilities}
import scala.collection.mutable


object JScatterPlot {
  type AxisType = Option[Seq[(Float, String)]]

  // GUI constants
  private val kDefaultRangeMarginFactor = 1.1f // factor to extend the default range by

  val kPointSizePx: Int = 4 // diameter in px
  val kSnapDistancePx: Int = 6 // distance (radius) to snap for a click
  val kPointSelectedSizePx: Int = 6 // diameter in px
  val kPointHoverOutlinePx: Int = 12 // diameter in px
  val kPointHoverOutlineColor: Color = JBColor.YELLOW

  val kTickBrightness: Float = 0.25f
  val kTickSpacingIntervals: Seq[Int] = Seq(1, 2, 5)
  val kMinTickSpacingPx: Int = 64 // min spacing between axis ticks, used to determine tick resolution
  val kTickSizePx: Int = 4

  def defaultValuesRange(values: Seq[Float], factor: Float = kDefaultRangeMarginFactor): (Float, Float) = {
    val range = ((values :+ 0f).min, (values :+ 0f).max)  // 0 in case values is empty, and forces the scale to include 0
    val span = range._2 - range._1
    val expansion = if (span > 0) { // range units to expand on each side
      span * (factor - 1) / 2
    } else { // if span is empty, arbitrarily expand by 1 on each side and center the data
      1
    }
    (range._1 - expansion, range._2 + expansion)
  }

  // multiply data by this to get screen coordinates
  def dataScale(dataRange: (Float, Float), screenSize: Int): Float = {
    if (dataRange._1 != dataRange._2) {
      screenSize / (dataRange._2 - dataRange._1)
    } else {
      1
    }
  }

  // Returns all the axis ticks given some scale, screen origin, screen size, and min screen spacing
  def getAxisTicks(range: (Float, Float), screenSize: Int, minScreenSpacing: Int = kMinTickSpacingPx):
      Seq[(Float, String)] = {
    val minDataSpacing = math.abs(minScreenSpacing / dataScale(range, screenSize)) // min tick spacing in data units
    val tickSpacings = JScatterPlot.kTickSpacingIntervals.map { factor => // try all the spacings and take the minimum
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

}


/** Scatterplot widget with two numerical axes, with labels inside the plot.
  * Data is structured as (x, y, data) coordinates, with some arbitrary data attached to each point.
  *
  */
class JScatterPlot[ValueType] extends JComponent {
  /** Scatterplot data point. Value (arbitrary data associated with this point), X, Y are required,
    * others are optional with defaults.
    * This allows future extensibility with additional parameters, eg size or marks.
    */
  class Data(val value: ValueType, val x: Float, val y: Float,
             val color: Option[Color] = None, val tooltipText: Option[String] = None) {
  }

  // data state
  private var xAxis: JScatterPlot.AxisType = Some(Seq())  // if text labels are specified, instead of dynamic numbers
  private var yAxis: JScatterPlot.AxisType = Some(Seq())

  private var data: IndexedSeq[Data] = IndexedSeq()
  private var mouseOverIndices: Seq[Int] = Seq()  // sorted by increasing index
  private var selectedIndices: Seq[Int] = Seq()  // unsorted

  // UI state
  private var xRange = (-1.0f, 1.0f)
  private var yRange = (-1.0f, 1.0f)

  private def dataToScreenX(dataVal: Float): Int = ((dataVal - xRange._1) * JScatterPlot.dataScale(xRange, getWidth)).toInt
  private def dataToScreenY(dataVal: Float): Int = ((yRange._2 - dataVal) * JScatterPlot.dataScale(yRange, getHeight)).toInt

  def setData(xys: IndexedSeq[Data], xAxis: JScatterPlot.AxisType = None, yAxis: JScatterPlot.AxisType = None): Unit = {
    data = xys
    mouseOverIndices = Seq()  // clear
    selectedIndices = Seq()  // clear

    xRange = JScatterPlot.defaultValuesRange(data.map(_.x))
    yRange = JScatterPlot.defaultValuesRange(data.map(_.y))

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

  private def paintAxes(paintGraphics: Graphics): Unit = {
    if (xAxis.isEmpty) { // left vertical axis - only on numeric axis
      val screenOriginX = dataToScreenX(0)
      paintGraphics.drawLine(screenOriginX, 0, screenOriginX, getHeight - 1)
    }
    val xTicks = xAxis match {
      case Some(xAxis) => xAxis
      case _ => JScatterPlot.getAxisTicks(xRange, getWidth)
    }
    xTicks.foreach { case (tickPos, tickVal) =>
      val screenX = dataToScreenX(tickPos)
      paintGraphics.drawLine(screenX, getHeight - 1, screenX, getHeight - 1 - JScatterPlot.kTickSizePx)
      DrawAnchored.drawLabel(paintGraphics, tickVal,
        (screenX, getHeight - 1 - JScatterPlot.kTickSizePx), DrawAnchored.Bottom)
    }

    if (yAxis.isEmpty) { // bottom horizontal axis - only on numeric axis
      val screenOriginY = dataToScreenY(0)
      paintGraphics.drawLine(0, screenOriginY, getWidth - 1, screenOriginY)
    }
    val yTicks = yAxis match {
      case Some(yAxis) => yAxis
      case _ => JScatterPlot.getAxisTicks(yRange, getHeight)
    }
    yTicks.foreach { case (tickPos, tickVal) =>
      val screenY = dataToScreenY(tickPos)
      paintGraphics.drawLine(0, screenY, JScatterPlot.kTickSizePx, screenY)
      DrawAnchored.drawLabel(paintGraphics, tickVal,
        (JScatterPlot.kTickSizePx, screenY), DrawAnchored.Left)
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

      if (mouseOverIndices.contains(index)) { // mouseover: highlight
        val hoverGraphics = paintGraphics.create()
        hoverGraphics.setColor(ColorUtil.blendColor(getBackground, JScatterPlot.kPointHoverOutlineColor, 0.5))
        hoverGraphics.fillOval(screenX - JScatterPlot.kPointHoverOutlinePx / 2, screenY - JScatterPlot.kPointHoverOutlinePx / 2,
          JScatterPlot.kPointHoverOutlinePx, JScatterPlot.kPointHoverOutlinePx)
      }
      if (selectedIndices.contains(index)) { // selected: thicker
        dataGraphics.fillOval(screenX - JScatterPlot.kPointSelectedSizePx / 2, screenY - JScatterPlot.kPointSelectedSizePx / 2,
          JScatterPlot.kPointSelectedSizePx, JScatterPlot.kPointSelectedSizePx)
      } else {
        dataGraphics.fillOval(screenX - JScatterPlot.kPointSizePx / 2, screenY - JScatterPlot.kPointSizePx / 2,
          JScatterPlot.kPointSizePx, JScatterPlot.kPointSizePx)
      }
    }
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val axesGraphics = paintGraphics.create()
    axesGraphics.setColor(ColorUtil.blendColor(getBackground, paintGraphics.getColor, JScatterPlot.kTickBrightness))
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
      val clickedPoints = getPointsForLocation(e.getX, e.getY, JScatterPlot.kSnapDistancePx)
      onClick(e, clickedPoints.sortBy(_._2).map(pair => data(pair._1)))
    }
  })

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)
      val newPoints = getPointsForLocation(e.getX, e.getY, JScatterPlot.kSnapDistancePx)
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

  // TODO unify w/ ZoomDragScrollPanel
  private val dragListener = new MouseAdapter {
    var dragLastPos: Option[Point] = None

    override def mousePressed(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragLastPos = Some(e.getPoint)
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      dragLastPos = None
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragLastPos.foreach { lastPos =>
          val dx = (lastPos.getX - e.getX).toFloat * (xRange._2 - xRange._1) / getWidth
          val dy = (lastPos.getY - e.getY).toFloat * (yRange._2 - yRange._1) / getHeight
          xRange = (xRange._1 + dx, xRange._2 + dx)
          yRange = (yRange._1 - dy, yRange._2 - dy)
          this.dragLastPos = Some(e.getPoint)

          validate()
          repaint()
        }
      }
    }
  }
  addMouseListener(dragListener)  // this registers the press / release
  addMouseMotionListener(dragListener)  // this registers the dragged

  override def getToolTipText(e: MouseEvent): String = {
    getPointsForLocation(e.getX, e.getY, JScatterPlot.kSnapDistancePx).headOption match {
      case Some((index, distance)) => data(index).tooltipText.orNull
      case None => null
    }
  }

  // User hooks - can be overridden
  //
  // called when this widget clicked, for all points within some hover radius of the cursor
  // sorted by distance from cursor (earlier = closer), and may be empty
  def onClick(e: MouseEvent, data: Seq[Data]): Unit = { }

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(data: Seq[Data]): Unit = { }
}
