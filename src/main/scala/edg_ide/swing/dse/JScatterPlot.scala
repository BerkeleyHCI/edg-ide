package edg_ide.swing.dse

import java.awt.{Dimension, Graphics, Rectangle}
import javax.swing.{JComponent, Scrollable}


/** Scatterplot widget with two numerical axes, with labels inside the plot.
  * Data is structured as (x, y, data) coordinates, with some arbitrary data attached to each point.
  *
  * TODO: support other ordinal axes?
  */
class JScatterPlot[ValueType] extends JComponent with Scrollable{
  private var data: Seq[(Float, Float, ValueType)] = Seq()

  private var xOrigin = 0  // zero data is here in screen coordinates
  private var xScale = 1.0f  // multiply data coord by this to get screen pos
  private var yOrigin = 0
  private var yScale = -1.0f  // screen coordinates are +Y = down

  private val kPointSizePx = 4
  private val kPlotMarginPx = 4  // px margin on every side of the plot

  def setData(xys: Seq[(Float, Float, ValueType)]): Unit = {
    data = xys

    validate()
    repaint()
  }

  private def paintAxes(paintGraphics: Graphics): Unit = {
    // bottom horizontal axis
    paintGraphics.drawLine(0, getHeight-1, getWidth-1, getHeight-1)

    // left vertical axis
    paintGraphics.drawLine(0, 0, 0, getHeight-1)
  }

  private def paintData(paintGraphics: Graphics): Unit = {
    data.foreach { case (rawX, rawY, value) =>
      val screenX = (rawX * xScale).toInt + xOrigin
      val screenY = (rawY * yScale).toInt + yOrigin
      paintGraphics.fillOval(screenX - kPointSizePx/2, screenY - kPointSizePx/2, kPointSizePx, kPointSizePx)
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


  override def getPreferredScrollableViewportSize: Dimension = getPreferredSize

  // Scrollable APIs
  //
  override def getPreferredSize: Dimension = new Dimension(100, 100)  // TODO arbitrary

  override def getScrollableBlockIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableUnitIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableTracksViewportWidth: Boolean = false

  override def getScrollableTracksViewportHeight: Boolean = false
}
