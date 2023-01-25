package edg_ide.swing.dse

import java.awt.{Dimension, Graphics, Rectangle}
import javax.swing.{JComponent, Scrollable}


/** Scatterplot widget with two numerical axes, with labels inside the plot.
  * Data is structured as (x, y, data) coordinates, with some arbitrary data attached to each point.
  *
  * TODO: support other ordinal axes?
  */
class JScatterPlot[DataType] extends JComponent with Scrollable{
  private var data: Seq[(Float, Float, DataType)] = Seq()

  private var xCenter = 0f  // center in data coordinates
  private var xScale = 1.0f  // multiply data coord by this to get screen pos
  private var yCenter = 0f
  private var yScale = 1.0f

  def setData(xys: Seq[(Float, Float, DataType)]): Unit = {
    data = xys

    val xs = xys.map(_._1)
    val xMin = math.max(0, xs.min)
    val xMax = math.min(0, xs.max)
    xScale = (xMax - xMin)/getWidth
    val ys = xys.map(_._2)
    val yMin = math.max(0, ys.min)
    val yMax = math.min(0, ys.max)
    yScale = (yMax - yMin)/getHeight

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
    // paintGraphics.drawOval()
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
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
