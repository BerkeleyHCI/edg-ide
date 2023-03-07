package edg_ide.swing.dse

import com.intellij.ui.JBColor
import edg_ide.swing.{ColorUtil, DrawAnchored}

import java.awt.event._
import java.awt.{BasicStroke, Color, Graphics, Graphics2D, Point}
import javax.swing.{JComponent, SwingUtilities}
import scala.collection.mutable


/** Parallel coordinates plot with arbitrary number of axes, with data structured as ((positions ...), data),
  * with arbitrary data of ValueType attached to each point
  */
class JParallelCoordinatesPlot[ValueType] extends JComponent {
  // Data point object
  class Data(val value: ValueType, val positions: IndexedSeq[Float],
             val color: Option[Color] = None, val tooltipText: Option[String] = None) {
  }

  // data state, note axes is considered the authoritative definition of the number of positions
  private var axes: IndexedSeq[JScatterPlot.AxisType] = IndexedSeq()
  private var data: IndexedSeq[Data] = IndexedSeq()
  private var mouseOverIndices: Seq[Int] = Seq() // sorted by increasing index
  private var selectedIndices: Seq[Int] = Seq() // unsorted

  // UI state
  private var axesRange: Seq[(Float, Float)] = Seq()  // range for each axis

  // axis MUST be defined for each position, but can be None for a numeric axis
  def setData(data: IndexedSeq[Data], axes: IndexedSeq[JScatterPlot.AxisType]): Unit = {
    this.data = data
    this.axes = axes
    mouseOverIndices = Seq() // clear
    selectedIndices = Seq() // clear

    axesRange = axes.zipWithIndex.map { case (axis, index) =>
      val values = data.flatMap { elt =>
        if (index >= elt.positions.length) {
          None
        } else {
          Some(elt.positions(index))
        }
      }
      JScatterPlot.defaultValuesRange(values)
    }

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
        case _ => None // ignored
      }
    }

    validate()
    repaint()
  }

  private def paintAxes(paintGraphics: Graphics): Unit = {
    (axes zip axesRange).zipWithIndex.foreach { case ((axis, range), index) =>
      val axisX = getPositionForAxis(index)
      paintGraphics.drawLine(axisX, 0, axisX, getHeight)

      val ticks = axis match {
        case Some(axis) => axis
        case _ => JScatterPlot.getAxisTicks(range, getHeight)
      }
      ticks.foreach { case (tickPos, tickVal) =>
        val screenPos = ((range._2 - tickPos) * JScatterPlot.dataScale(range, getHeight)).toInt
        paintGraphics.drawLine(axisX, screenPos, axisX + JScatterPlot.kTickSizePx, screenPos)
        DrawAnchored.drawLabel(paintGraphics, tickVal,
          (axisX + JScatterPlot.kTickSizePx, screenPos), DrawAnchored.Left)
      }
    }
  }

  private def paintData(paintGraphics: Graphics): Unit = {
    data.zipWithIndex.foreach { case (data, index) =>
      val dataGraphics = paintGraphics.create()
      data.color.foreach { color => // if color is specified, set the color
        dataGraphics.setColor(color)
      }

      // TODO proper layering
      data.positions.sliding(2).zipWithIndex.foreach { case (Seq(prevValue, nextValue), prevIndex) =>
        val prevAxisPos = getPositionForAxis(prevIndex)
        val prevValuePos = getPositionForValue(prevIndex, prevValue)
        val nextAxisPos = getPositionForAxis(prevIndex + 1)
        val nextValuePos = getPositionForValue(prevIndex + 1, nextValue)

        if (mouseOverIndices.contains(index)) { // mouseover: highlight
          val hoverGraphics = dataGraphics.create().asInstanceOf[Graphics2D]
          hoverGraphics.setColor(ColorUtil.blendColor(getBackground, JScatterPlot.kHoverOutlineColor, 0.5))
          hoverGraphics.setStroke(new BasicStroke(JScatterPlot.kLineHoverOutlinePx.toFloat))
          hoverGraphics.drawLine(prevAxisPos, prevValuePos, nextAxisPos, nextValuePos)
        }
        if (selectedIndices.contains(index)) { // selected: thicker
          val lineGraphics = dataGraphics.create().asInstanceOf[Graphics2D]
          lineGraphics.setStroke(new BasicStroke(JScatterPlot.kLineSelectedSizePx.toFloat))
          lineGraphics.drawLine(prevAxisPos, prevValuePos, nextAxisPos, nextValuePos)
        } else {
          dataGraphics.drawLine(prevAxisPos, prevValuePos, nextAxisPos, nextValuePos)
        }
      }

      data.positions.zipWithIndex.foreach { case (value, axisIndex) =>
        val axisPos = getPositionForAxis(axisIndex)
        val dataPos = getPositionForValue(axisIndex, value)
        if (mouseOverIndices.contains(index)) { // mouseover: highlight
          val hoverGraphics = dataGraphics.create()
          hoverGraphics.setColor(ColorUtil.blendColor(getBackground, JScatterPlot.kHoverOutlineColor, 0.5))
          hoverGraphics.fillOval(axisPos - JScatterPlot.kPointHoverOutlinePx / 2, dataPos - JScatterPlot.kPointHoverOutlinePx / 2,
            JScatterPlot.kPointHoverOutlinePx, JScatterPlot.kPointHoverOutlinePx)
        }
        if (selectedIndices.contains(index)) { // selected: thicker
          dataGraphics.fillOval(axisPos - JScatterPlot.kPointSelectedSizePx / 2, dataPos - JScatterPlot.kPointSelectedSizePx / 2,
            JScatterPlot.kPointSelectedSizePx, JScatterPlot.kPointSelectedSizePx)
        } else {
          dataGraphics.fillOval(axisPos - JScatterPlot.kPointSizePx / 2, dataPos - JScatterPlot.kPointSizePx / 2,
            JScatterPlot.kPointSizePx, JScatterPlot.kPointSizePx)
        }
      }
    }
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val axesGraphics = paintGraphics.create()
    axesGraphics.setColor(ColorUtil.blendColor(getBackground, paintGraphics.getColor, JScatterPlot.kTickBrightness))
    paintAxes(axesGraphics)

    paintData(paintGraphics)
  }

  def getAxisForLocation(x: Int): Int = {
    math.min(x * axes.length / getWidth, axes.length - 1)
  }

  // Returns the points with some specified distance (in screen coordinates, px) of the point.
  // Returns as (index of point, distance)
  def getPointsForLocation(x: Int, y: Int, maxDistance: Int): Seq[(Int, Float)] = {
    val axisIndex = getAxisForLocation(x)
    val axisPosition = getPositionForAxis(axisIndex)
    if (math.abs(axisPosition - x) > maxDistance) {
      return Seq()  // quick sanity check
    }

    data.zipWithIndex.flatMap { case (data, index) =>
      val xDist = axisPosition - x
      val yDist = getPositionForValue(axisIndex, data.positions(axisIndex)) - y
      val distance = math.sqrt(xDist * xDist + yDist * yDist).toFloat
      if (distance <= maxDistance) {
        Some(index, distance)
      } else {
        None
      }
    }
  }

  def getPositionForAxis(axisIndex: Int): Int = {
    val axisSpacing = getWidth / (axes.length)
    axisSpacing * axisIndex + axisSpacing / 2
  }

  def getPositionForValue(axisIndex: Int, value: Float): Int = {
    if (axisIndex >= axesRange.length) {
      return Int.MinValue
    }
    val range = axesRange(axisIndex)
    ((range._2 - value) * JScatterPlot.dataScale(range, getHeight)).toInt
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
      // TODO maybe require the mouse to be closer to an axis?
      val zoomFactor = Math.pow(1.1, 1 * e.getPreciseWheelRotation).toFloat

      val axisIndex = getAxisForLocation(e.getX)
      val newRange = JScatterPlot.scrollNewRange(axesRange(axisIndex), zoomFactor, 1 - (e.getY.toFloat / getHeight))
      axesRange = axesRange.updated(axisIndex, newRange)

      validate()
      repaint()
    }
  })

  // TODO unify w/ ZoomDragScrollPanel
  private val dragListener = new MouseAdapter {
    var dragLastAxisPos: Option[(Int, Int)] = None  // axis, y-pos

    override def mousePressed(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragLastAxisPos = Some((getAxisForLocation(e.getX), e.getY))
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      dragLastAxisPos = None
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragLastAxisPos.foreach { case (axisIndex, pos) =>
          if (axisIndex >= axesRange.length) {
            return
          }
          val currentRange = axesRange(axisIndex)
          val dy = (pos - e.getY).toFloat * (currentRange._2 - currentRange._1) / getHeight
          axesRange = axesRange.updated(axisIndex, (currentRange._1 - dy, currentRange._2 - dy))

          this.dragLastAxisPos = Some((axisIndex, e.getY))

          validate()
          repaint()
        }
      }
    }
  }
  addMouseListener(dragListener) // this registers the press / release
  addMouseMotionListener(dragListener) // this registers the dragged

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
  def onClick(e: MouseEvent, data: Seq[Data]): Unit = {}

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(data: Seq[Data]): Unit = {}
}
