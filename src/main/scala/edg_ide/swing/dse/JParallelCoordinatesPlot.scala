package edg_ide.swing.dse

import edg_ide.swing.{ColorUtil, DrawAnchored}

import java.awt.event._
import java.awt.{BasicStroke, Color, Graphics, Graphics2D}
import javax.swing.{JComponent, SwingUtilities}


/** Parallel coordinates plot with arbitrary number of axes, with data structured as ((positions ...), data),
  * with arbitrary data of ValueType attached to each point
  */
class JParallelCoordinatesPlot[ValueType] extends JComponent {
  // Data point object
  class Data(val value: ValueType, val positions: IndexedSeq[Option[Float]],
             val zOrder: Int = 1, val color: Option[Color] = None, val tooltipText: Option[String] = None) {
  }

  // data state, note axes is considered the authoritative definition of the number of positions
  // arbitrarily initialize to one axis, because why would this have less than one axis?
  private var axes: IndexedSeq[PlotAxis.AxisType] = IndexedSeq(Some(Seq()))
  private var data: IndexedSeq[Data] = IndexedSeq()
  private var mouseOverIndices: Seq[Int] = Seq() // sorted by increasing index
  private var selectedIndices: Seq[Int] = Seq() // unsorted

  private var dragRange: Option[(Int, Float, Option[Float])] = None  // (axis, start, current) in data-space if in drag

  // UI state
  private var axesRange: Seq[(Float, Float)] = Seq((-1.0f, 1.0f))  // range for each axis

  // axis MUST be defined for each position, but can be None for a numeric axis
  def setData(data: IndexedSeq[Data], axes: IndexedSeq[PlotAxis.AxisType]): Unit = {
    this.data = data
    this.axes = axes
    mouseOverIndices = Seq() // clear
    selectedIndices = Seq() // clear
    dragRange = None

    axesRange = axes.zipWithIndex.map { case (axis, index) =>
      val values = data.flatMap { elt =>
        if (index >= elt.positions.length) {
          None
        } else {
          Some(elt.positions(index))
        }
      }
      JDsePlot.defaultValuesRange(values.flatten)
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
        case index if index >= 0 => Some(index)
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
        case _ => JDsePlot.getAxisTicks(range, getHeight)
      }
      ticks.foreach { case (tickPos, tickVal) =>
        val tickLabelX = if (index == 0) 0 else axisX
        val screenPos = ((range._2 - tickPos) * JDsePlot.dataScale(range, getHeight)).toInt
        paintGraphics.drawLine(axisX, screenPos, axisX + JDsePlot.kTickSizePx, screenPos)
        DrawAnchored.drawLabel(paintGraphics, tickVal,
          (tickLabelX + JDsePlot.kTickSizePx, screenPos), DrawAnchored.Left)
      }
    }
  }

  private def paintDataLine(paintGraphics: Graphics, value: Float, nextValue: Float,
                            axisIndex: Int): Unit = {
    val prevAxisPos = getPositionForAxis(axisIndex)
    val prevValuePos = getPositionForValue(axisIndex, value)
    val nextAxisPos = getPositionForAxis(axisIndex + 1)
    val nextValuePos = getPositionForValue(axisIndex + 1, nextValue)
    paintGraphics.drawLine(prevAxisPos, prevValuePos, nextAxisPos, nextValuePos)
  }

  private def paintDataPoint(paintGraphics: Graphics, value: Float, axisIndex: Int): Unit = {
    val axisPos = getPositionForAxis(axisIndex)
    val dataPos = getPositionForValue(axisIndex, value)

    // needed for the hover outline
    paintGraphics.drawOval(axisPos - JDsePlot.kPointSizePx / 2, dataPos - JDsePlot.kPointSizePx / 2,
      JDsePlot.kPointSizePx, JDsePlot.kPointSizePx)
    paintGraphics.fillOval(axisPos - JDsePlot.kPointSizePx / 2, dataPos - JDsePlot.kPointSizePx / 2,
      JDsePlot.kPointSizePx, JDsePlot.kPointSizePx)
  }

  private def paintData(paintGraphics: Graphics, data: IndexedSeq[Data], noColor: Boolean = false,
                        colorBlend: Float = 1.0f, alpha: Int = 255): Unit = {
    data.sortBy(_.zOrder).foreach { data =>
      val dataGraphics = if (noColor) {
        paintGraphics
      } else {
        val dataGraphics = paintGraphics.create()
        data.color.foreach { color => // if color is specified, set the color
          dataGraphics.setColor(color)
        }
        dataGraphics
      }
      dataGraphics.setColor(ColorUtil.withAlpha(
        ColorUtil.blendColor(getBackground, dataGraphics.getColor, colorBlend), alpha))

      data.positions.sliding(2).zipWithIndex.foreach {
        case (Seq(Some(value), Some(nextValue)), axisIndex) =>
          paintDataLine(dataGraphics, value, nextValue, axisIndex)
        case _ => // ignore None data for any lines
      }

      data.positions.zipWithIndex.foreach {
        case (Some(value), axisIndex) =>
          paintDataPoint(dataGraphics, value, axisIndex)
        case _ => // ignore None data for its specific axis
      }
    }
  }

  private def paintAllData(paintGraphics: Graphics): Unit = {
    val (mouseoverData, nonMouseoverData) = data.zipWithIndex.partition { case (data, dataIndex) =>
      mouseOverIndices.contains(dataIndex)
    }
    val (normalData, backgroundData) = nonMouseoverData.partition { case (data, dataIndex) =>
      // only have backgrounded data if there is an active selection
      selectedIndices.isEmpty || selectedIndices.contains(dataIndex)
    }

    paintData(paintGraphics, backgroundData.map(_._1), colorBlend = JDsePlot.kBackgroundBlend,
      alpha = JDsePlot.kBackgroundAlpha)

    paintGraphics.asInstanceOf[Graphics2D].setStroke(new BasicStroke(JDsePlot.kLinePx))
    paintData(paintGraphics, normalData.map(_._1), alpha = JDsePlot.kPointAlpha)

    val hoverGraphics = paintGraphics.create().asInstanceOf[Graphics2D]
    hoverGraphics.setColor(getBackground)
    hoverGraphics.setStroke(new BasicStroke(JDsePlot.kLineHoverBackgroundPx.toFloat))
    paintData(hoverGraphics, mouseoverData.map(_._1), noColor = true, alpha = 191)  // draw the background exclusion border
    hoverGraphics.setColor(ColorUtil.blendColor(getBackground, JDsePlot.kHoverOutlineColor, JDsePlot.kHoverOutlineBlend))
    hoverGraphics.setStroke(new BasicStroke(JDsePlot.kLineHoverOutlinePx.toFloat))
    paintData(hoverGraphics, mouseoverData.map(_._1), noColor = true)

    paintData(paintGraphics, mouseoverData.map(_._1))
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val axesGraphics = paintGraphics.create()
    axesGraphics.setColor(ColorUtil.blendColor(getBackground, paintGraphics.getColor, JDsePlot.kTickBrightness))
    paintAxes(axesGraphics)

    paintAllData(paintGraphics)

    dragRange.collect { case (axis, startVal, Some(currVal)) =>
      val selectionGraphics = paintGraphics.create()
      selectionGraphics.setColor(JDsePlot.kHoverOutlineColor)
      val (minY, maxY) = JDsePlot.orderedValues(getPositionForValue(axis, startVal), getPositionForValue(axis, currVal))
      val centerX = getPositionForAxis(axis)
      val minX = centerX - JDsePlot.kPointHoverOutlinePx / 2
      val maxX = centerX + JDsePlot.kPointHoverOutlinePx / 2

      selectionGraphics.drawRect(minX, minY, maxX - minX, maxY - minY)

      selectionGraphics.setColor(ColorUtil.withAlpha(selectionGraphics.getColor, JDsePlot.kDragSelectAlpha))
      selectionGraphics.fillRect(minX, minY, maxX - minX, maxY - minY)
    }
  }

  private def getAxisForLocation(x: Int): Int = {
    math.min(x * axes.length / getWidth, axes.length - 1)
  }

  // Given the current selection, returns the selectable points zipped with index
  private def selectablePointsWithIndex: Seq[(Data, Int)] = {
    if (selectedIndices.nonEmpty) { // if currently selected points, filter from that
      selectedIndices.map(index => (data(index), index))
    } else { // otherwise all points valid
      data.zipWithIndex
    }
  }

  // Returns the points with some specified distance (in screen coordinates, px) of the point.
  // Returns as (index of point, distance)
  private def getPointsForLocation(x: Int, y: Int, maxDistance: Int, selectableWithIndex: Seq[(Data, Int)]): Seq[(Int, Float)] = {
    val axisIndex = getAxisForLocation(x)
    val axisPosition = getPositionForAxis(axisIndex)
    if (math.abs(axisPosition - x) > maxDistance) {
      return Seq()  // if not close enough to the axis nothing else matters
    }

    selectableWithIndex.flatMap { case (data, index) =>
      data.positions(axisIndex) match {
        case Some(value) =>
          val xDist = axisPosition - x
          val yDist = getPositionForValue(axisIndex, value) - y
          val distance = math.sqrt(xDist * xDist + yDist * yDist).toFloat
          if (distance <= maxDistance) {
            Some(index, distance)
          } else {
            None
          }
        case _ => None  // ignore None
      }
    }
  }

  private def getPositionForAxis(axisIndex: Int): Int = {
    val axisSpacing = getWidth / math.max(axes.length, 1)
    axisSpacing * axisIndex + axisSpacing / 2
  }

  private def getPositionForValue(axisIndex: Int, value: Float): Int = {
    if (axisIndex >= axesRange.length) {
      return Int.MinValue
    }
    value match {
      case Float.PositiveInfinity => 1
      case Float.NegativeInfinity => getHeight - 2
      case _ =>
        val range = axesRange(axisIndex)
        ((range._2 - value) * JDsePlot.dataScale(range, getHeight)).toInt
    }
  }

  private def getValueForPosition(axisIndex: Int, position: Int): Float = {
    if (axisIndex >= axesRange.length) {
      return Float.NaN
    }
    val range = axesRange(axisIndex)
    -(position / JDsePlot.dataScale(range, getHeight) - range._2)
  }

  private def hoverUpdated(newIndices: Seq[Int]): Unit = {
    if (mouseOverIndices != newIndices) {
      mouseOverIndices = newIndices
      validate()
      repaint()

      onHoverChange(newIndices.map(data(_)))
    }
  }

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)
      if (dragRange.isEmpty) {  // only runs on non-drag
        val newPoints = getPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx, selectablePointsWithIndex)
        val sortedIndices = newPoints.sortBy(_._2).map(_._1)  // sort by distance
        hoverUpdated(sortedIndices)
      }
    }
  })

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      onClick(e, mouseOverIndices.map(data(_)))
    }
  })

  addMouseWheelListener(new MouseWheelListener {
    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      // TODO maybe require the mouse to be closer to an axis?
      val zoomFactor = Math.pow(1.1, 1 * e.getPreciseWheelRotation).toFloat

      val axisIndex = getAxisForLocation(e.getX)
      val newRange = JDsePlot.scrollNewRange(axesRange(axisIndex), zoomFactor, 1 - (e.getY.toFloat / getHeight))
      axesRange = axesRange.updated(axisIndex, newRange)

      validate()
      repaint()
    }
  })

  // TODO unify w/ ZoomDragScrollPanel
  private val dragPanListener = new MouseAdapter {
    var dragLastAxisPos: Option[(Int, Int)] = None  // axis, y-pos

    override def mousePressed(e: MouseEvent): Unit = {
      if (SwingUtilities.isMiddleMouseButton(e)) {
        dragLastAxisPos = Some((getAxisForLocation(e.getX), e.getY))
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      if (SwingUtilities.isMiddleMouseButton(e)) {
        dragLastAxisPos = None
      }
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      if (SwingUtilities.isMiddleMouseButton(e)) {
        dragLastAxisPos.foreach { case (axisIndex, pos) =>
          if (axisIndex >= axesRange.length || axesRange.isEmpty) {
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
  addMouseListener(dragPanListener) // this registers the press / release
  addMouseMotionListener(dragPanListener) // this registers the dragged

  private val dragSelectListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        val axisIndex = getAxisForLocation(e.getX)
        val axisValue = getValueForPosition(axisIndex, e.getY)
        dragRange = Some((axisIndex, axisValue, None))

        validate()
        repaint() // repaint the selection box regardless
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragRange.foreach { case (axisIndex, startY, end) =>
          if (end.nonEmpty) {
            if (mouseOverIndices.nonEmpty) {
              onClick(e, mouseOverIndices.map(data(_)))
            }
            mouseOverIndices = Seq()
          }
          dragRange = None

          validate()
          repaint() // repaint the selection box regardless
        }
      }
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragRange.foreach { case (axisIndex, startY, _) =>
          val currY = getValueForPosition(axisIndex, e.getY)
          dragRange = Some((axisIndex, startY, Some(currY)))

          val (minY, maxY) = JDsePlot.orderedValues(startY, currY)

          val newIndices = selectablePointsWithIndex.flatMap { case (data, index) =>
            data.positions(axisIndex) match {
              case Some(value) =>
                if (minY <= value && value <= maxY) {
                  Some(index)
                } else {
                  None
                }
              case None => None
            }
          }

          hoverUpdated(newIndices)

          validate()
          repaint() // repaint the selection box regardless
        }
      }
    }
  }
  addMouseListener(dragSelectListener) // this registers the press / release
  addMouseMotionListener(dragSelectListener) // this registers the dragged

  override def getToolTipText(e: MouseEvent): String = {
    getPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx, selectablePointsWithIndex).headOption match {
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
