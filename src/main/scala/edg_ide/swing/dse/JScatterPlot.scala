package edg_ide.swing.dse

import edg_ide.swing.DrawAnchored

import java.awt.event.{MouseEvent, MouseMotionAdapter}
import java.awt.{Dimension, Graphics, Rectangle}
import javax.swing.{JComponent, Scrollable}
import scala.collection.mutable


/** Scatterplot widget with two numerical axes, with labels inside the plot.
  * Data is structured as (x, y, data) coordinates, with some arbitrary data attached to each point.
  *
  * TODO: support other ordinal axes?
  */
class JScatterPlot[ValueType] extends JComponent with Scrollable{
  private var data: Seq[(Float, Float, ValueType)] = Seq()

  private val kPlotMarginPx = 4 // px margin on every side of the plot

  private val kPointSizePx = 4 // diameter in px
  private val kSnapDistancePx = 4 // distance (box) to snap for a click

  private val kTickSpacingIntervals = Seq(1, 2, 5)
  private val kMinTickSpacingPx = 64  // min spacing between axis ticks, used to determine tick resolution
  private val kTickSizePx = 4

  private var xOrigin = 0  // zero data is here in screen coordinates
  private var xScale = 1.0f  // in px/data; multiply data coord by this to get screen pos
  private var yOrigin = 0
  private var yScale = -1.0f  // screen coordinates are +Y = down

  // index into data
  private var mouseOverData: Set[Int] = Set()

  def setData(xys: Seq[(Float, Float, ValueType)]): Unit = {
    data = xys
    mouseOverData = Set()  // clear

    validate()
    repaint()
  }

  // Returns all the axis ticks given some scale, screen origin, screen size, and min screen spacing
  private def getAxisTicks(scale: Float, screenOrigin: Int, screenSize: Int, minScreenSpacing: Int): Seq[Float] = {
    val minDataSpacing = math.abs(minScreenSpacing / scale)  // min tick spacing in data units
    val tickSpacings = kTickSpacingIntervals.map { factor =>  // try all the spacings and take the minimum
      math.pow(10, math.log10(minDataSpacing / factor).ceil) * factor
    }
    val tickSpacing = tickSpacings.min
    val tickBound1 = -screenOrigin / scale  // because scale could be negative this could be the high end
    val tickBound2 = (screenSize - screenOrigin) / scale

    var tickPos = (math.floor(math.min(tickBound1, tickBound2) / tickSpacing) * tickSpacing).toFloat
    val tickEnd = math.max(tickBound1, tickBound2)
    val ticksBuilder = mutable.ArrayBuffer[Float]()
    while (tickPos <= tickEnd) {
      ticksBuilder.append(tickPos)
      tickPos = (tickPos + tickSpacing).toFloat
    }
    ticksBuilder.toSeq
  }

  private def paintAxes(paintGraphics: Graphics): Unit = {
    // bottom horizontal axis
    paintGraphics.drawLine(0, getHeight-1, getWidth-1, getHeight-1)
    getAxisTicks(xScale, xOrigin, getWidth, kMinTickSpacingPx).foreach { tickPos =>
      val screenX = (tickPos * xScale).toInt + xOrigin
      paintGraphics.drawLine(screenX, getHeight - 1, screenX, getHeight - 1 - kTickSizePx)
      DrawAnchored.drawLabel(paintGraphics, f"$tickPos%.02g",
        (screenX, getHeight - 1 - kTickSizePx), DrawAnchored.Bottom)
    }

    // left vertical axis
    paintGraphics.drawLine(0, 0, 0, getHeight-1)
    getAxisTicks(yScale, yOrigin, getHeight, kMinTickSpacingPx).foreach { tickPos =>
      val screenY = (tickPos * yScale).toInt + yOrigin
      paintGraphics.drawLine(0, screenY, kTickSizePx, screenY)
      DrawAnchored.drawLabel(paintGraphics, f"$tickPos%.02g",
        (kTickSizePx, screenY), DrawAnchored.Left)
    }
  }

  private def paintData(paintGraphics: Graphics): Unit = {
    data.zipWithIndex.foreach { case ((rawX, rawY, value), index) =>
      val screenX = (rawX * xScale).toInt + xOrigin
      val screenY = (rawY * yScale).toInt + yOrigin
      paintGraphics.fillOval(screenX - kPointSizePx / 2, screenY - kPointSizePx / 2, kPointSizePx, kPointSizePx)
      if (mouseOverData.contains(index)) {  // makes it thicker
        paintGraphics.drawOval(screenX - kPointSizePx / 2, screenY - kPointSizePx / 2, kPointSizePx, kPointSizePx)
      }
    }
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val xs = data.map(_._1)
    val xMin = math.min(0, xs.min)
    val xMax = math.max(0, xs.max)
    val xSpan = xMax - xMin
    xScale = if (xSpan != 0) (getWidth - 2 * kPlotMarginPx) / (xMax - xMin) else 1
    xOrigin = if (xSpan != 0) (-xMin * xScale).toInt + kPlotMarginPx else getWidth / 2
    val ys = data.map(_._2)
    val yMin = math.min(0, ys.min)
    val yMax = math.max(0, ys.max)
    val ySpan = yMax - yMin
    yScale = if (ySpan != 0) -(getHeight - 2 * kPlotMarginPx) / (yMax - yMin) else -1
    yOrigin = if (ySpan != 0) getHeight - (yMin * yScale).toInt - kPlotMarginPx else getHeight / 2

    paintAxes(paintGraphics)
    paintData(paintGraphics)
  }

  // Returns the points with some specified distance (in screen coordinates, px) of the point.
  def getPointsForLocation(x: Int, y: Int, distance: Int): Seq[Int] = {
    data.zipWithIndex.flatMap { case ((rawX, rawY, value), index) =>
      val screenX = (rawX * xScale).toInt + xOrigin
      val screenY = (rawY * yScale).toInt + yOrigin
      if (math.abs(screenX - x) <= distance && math.abs(screenY - y) <= distance) {
        Some(index)
      } else {
        None
      }
    }
  }

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)
      val newPoints = getPointsForLocation(e.getX, e.getY, kSnapDistancePx).toSet
      if (mouseOverData != newPoints) {
        mouseOverData = newPoints
        validate()
        repaint()
      }
    }
  })


  override def getPreferredScrollableViewportSize: Dimension = getPreferredSize

  // Scrollable APIs
  //
  override def getPreferredSize: Dimension = new Dimension(100, 100)  // TODO arbitrary

  override def getScrollableBlockIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableUnitIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableTracksViewportWidth: Boolean = false

  override def getScrollableTracksViewportHeight: Boolean = false
}
