package edg_ide.swing.dse

import edg_ide.swing.{ColorUtil, DrawAnchored}

import java.awt.event._
import java.awt.{Color, Graphics, Point}
import javax.swing.{JComponent, SwingUtilities}


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
  private var xAxis: PlotAxis.AxisType = Some(Seq())  // if text labels are specified, instead of dynamic numbers
  private var yAxis: PlotAxis.AxisType = Some(Seq())

  private var data: IndexedSeq[Data] = IndexedSeq()
  private var mouseOverIndices: Seq[Int] = Seq()  // sorted by increasing index
  private var selectedIndices: Seq[Int] = Seq()  // unsorted

  // UI state
  private var xRange = (-1.0f, 1.0f)
  private var yRange = (-1.0f, 1.0f)

  private def dataToScreenX(dataVal: Float): Int = dataVal match {
    case Float.PositiveInfinity => getWidth - 2
    case Float.NegativeInfinity => 1
    case _ => ((dataVal - xRange._1) * JDsePlot.dataScale(xRange, getWidth)).toInt
  }
  private def dataToScreenY(dataVal: Float): Int = dataVal match {
    case Float.PositiveInfinity => 1
    case Float.NegativeInfinity => getHeight - 2
    case _ => ((yRange._2 - dataVal) * JDsePlot.dataScale(yRange, getHeight)).toInt
  }

  def setData(xys: IndexedSeq[Data], xAxis: PlotAxis.AxisType = None, yAxis: PlotAxis.AxisType = None): Unit = {
    data = xys
    mouseOverIndices = Seq()  // clear
    selectedIndices = Seq()  // clear

    xRange = JDsePlot.defaultValuesRange(data.map(_.x))
    yRange = JDsePlot.defaultValuesRange(data.map(_.y))

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
      case _ => JDsePlot.getAxisTicks(xRange, getWidth)
    }
    xTicks.foreach { case (tickPos, tickVal) =>
      val screenX = dataToScreenX(tickPos)
      paintGraphics.drawLine(screenX, getHeight - 1, screenX, getHeight - 1 - JDsePlot.kTickSizePx)
      DrawAnchored.drawLabel(paintGraphics, tickVal,
        (screenX, getHeight - 1 - JDsePlot.kTickSizePx), DrawAnchored.Bottom)
    }

    if (yAxis.isEmpty) { // bottom horizontal axis - only on numeric axis
      val screenOriginY = dataToScreenY(0)
      paintGraphics.drawLine(0, screenOriginY, getWidth - 1, screenOriginY)
    }
    val yTicks = yAxis match {
      case Some(yAxis) => yAxis
      case _ => JDsePlot.getAxisTicks(yRange, getHeight)
    }
    yTicks.foreach { case (tickPos, tickVal) =>
      val screenY = dataToScreenY(tickPos)
      paintGraphics.drawLine(0, screenY, JDsePlot.kTickSizePx, screenY)
      DrawAnchored.drawLabel(paintGraphics, tickVal,
        (JDsePlot.kTickSizePx, screenY), DrawAnchored.Left)
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
        hoverGraphics.setColor(ColorUtil.blendColor(getBackground, JDsePlot.kHoverOutlineColor, 0.5))
        hoverGraphics.fillOval(screenX - JDsePlot.kPointHoverOutlinePx / 2, screenY - JDsePlot.kPointHoverOutlinePx / 2,
          JDsePlot.kPointHoverOutlinePx, JDsePlot.kPointHoverOutlinePx)
      }
      if (selectedIndices.contains(index)) { // selected: thicker
        dataGraphics.fillOval(screenX - JDsePlot.kPointSelectedSizePx / 2, screenY - JDsePlot.kPointSelectedSizePx / 2,
          JDsePlot.kPointSelectedSizePx, JDsePlot.kPointSelectedSizePx)
      } else {
        dataGraphics.fillOval(screenX - JDsePlot.kPointSizePx / 2, screenY - JDsePlot.kPointSizePx / 2,
          JDsePlot.kPointSizePx, JDsePlot.kPointSizePx)
      }
    }
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val axesGraphics = paintGraphics.create()
    axesGraphics.setColor(ColorUtil.blendColor(getBackground, paintGraphics.getColor, JDsePlot.kTickBrightness))
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
      val clickedPoints = getPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx)
      onClick(e, clickedPoints.sortBy(_._2).map(pair => data(pair._1)))
    }
  })

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)
      val newPoints = getPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx)
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
      val zoomFactor = Math.pow(1.1, 1 * e.getPreciseWheelRotation).toFloat
      xRange = JDsePlot.scrollNewRange(xRange, zoomFactor, e.getX.toFloat / getWidth)
      yRange = JDsePlot.scrollNewRange(yRange, zoomFactor, 1 - (e.getY.toFloat / getHeight))

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
    getPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx).headOption match {
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
