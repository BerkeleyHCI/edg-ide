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
             val color: Option[Color] = None, val tooltipText: Option[String] = None) {
  }

  // data state, note axes is considered the authoritative definition of the number of positions
  // arbitrarily initialize to one axis, because why would this have less than one axis?
  private var axes: IndexedSeq[PlotAxis.AxisType] = IndexedSeq(Some(Seq()))
  private var data: IndexedSeq[Data] = IndexedSeq()
  private var mouseOverIndices: Seq[Int] = Seq() // sorted by increasing index
  private var selectedIndices: Seq[Int] = Seq() // unsorted

  // UI state
  private var axesRange: Seq[(Float, Float)] = Seq((-1.0f, 1.0f))  // range for each axis

  // axis MUST be defined for each position, but can be None for a numeric axis
  def setData(data: IndexedSeq[Data], axes: IndexedSeq[PlotAxis.AxisType]): Unit = {
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
        case _ => JDsePlot.getAxisTicks(range, getHeight)
      }
      ticks.foreach { case (tickPos, tickVal) =>
        val screenPos = ((range._2 - tickPos) * JDsePlot.dataScale(range, getHeight)).toInt
        paintGraphics.drawLine(axisX, screenPos, axisX + JDsePlot.kTickSizePx, screenPos)
        DrawAnchored.drawLabel(paintGraphics, tickVal,
          (axisX + JDsePlot.kTickSizePx, screenPos), DrawAnchored.Left)
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

    paintGraphics.drawOval(axisPos - JDsePlot.kPointSizePx / 2, dataPos - JDsePlot.kPointSizePx / 2,
      JDsePlot.kPointSizePx, JDsePlot.kPointSizePx)
    paintGraphics.fillOval(axisPos - JDsePlot.kPointSizePx / 2, dataPos - JDsePlot.kPointSizePx / 2,
      JDsePlot.kPointSizePx, JDsePlot.kPointSizePx)
  }

  private def paintData(paintGraphics: Graphics, data: IndexedSeq[Data], noColor: Boolean = false,
                        colorBlend: Float = 1.0f): Unit = {
    data.foreach { data =>
      val dataGraphics = if (noColor) {
        paintGraphics
      } else {
        val dataGraphics = paintGraphics.create()
        data.color.foreach { color => // if color is specified, set the color
          dataGraphics.setColor(color)
        }
        dataGraphics
      }
      dataGraphics.setColor(ColorUtil.blendColor(getBackground, dataGraphics.getColor, colorBlend))

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
    // paint order: (bottom) normal -> selected -> mouseover
    val normalData = data.zipWithIndex.filter { case (data, dataIndex) =>
      !mouseOverIndices.contains(dataIndex) && !selectedIndices.contains(dataIndex)
    }.map(_._1)
    val selectedData = data.zipWithIndex.filter { case (data, dataIndex) =>
      !mouseOverIndices.contains(dataIndex) && selectedIndices.contains(dataIndex)
    }.map(_._1)
    val mouseoverData = data.zipWithIndex.filter { case (data, dataIndex) =>
      mouseOverIndices.contains(dataIndex)
    }.map(_._1)

    val normalDataBlend = if (selectedIndices.nonEmpty) 0.33f else 1.0f  // dim others if there is a selection
    paintData(paintGraphics, normalData, colorBlend = normalDataBlend)

    val selectedGraphics = paintGraphics.create().asInstanceOf[Graphics2D]
    selectedGraphics.setStroke(new BasicStroke(JDsePlot.kLineSelectedSizePx.toFloat))
    paintData(selectedGraphics, selectedData)

    val hoverGraphics = paintGraphics.create().asInstanceOf[Graphics2D]
    hoverGraphics.setColor(ColorUtil.blendColor(getBackground, JDsePlot.kHoverOutlineColor, 0.5))
    hoverGraphics.setStroke(new BasicStroke(JDsePlot.kLineHoverOutlinePx.toFloat))
    paintData(hoverGraphics, mouseoverData, noColor = true)

    paintData(selectedGraphics, mouseoverData)  // TODO: separate out selected from mouseover? idk
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val axesGraphics = paintGraphics.create()
    axesGraphics.setColor(ColorUtil.blendColor(getBackground, paintGraphics.getColor, JDsePlot.kTickBrightness))
    paintAxes(axesGraphics)

    paintAllData(paintGraphics)
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
      return Seq()  // if not close enough to the axis nothing else matters
    }

    data.zipWithIndex.flatMap { case (data, index) =>
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

  def getSelectingPointsForLocation(x: Int, y: Int, maxDistance: Int): Seq[(Int, Float)] = {
    val allPoints = getPointsForLocation(x, y, maxDistance)
    if (selectedIndices.nonEmpty) { // if selection, subset from selection
      allPoints.filter { case (index, dist) =>
        selectedIndices.contains(index)
      }
    } else { // otherwise create fresh selection
      allPoints
    }
  }

  def getPositionForAxis(axisIndex: Int): Int = {
    val axisSpacing = getWidth / math.max(axes.length, 1)
    axisSpacing * axisIndex + axisSpacing / 2
  }

  def getPositionForValue(axisIndex: Int, value: Float): Int = {
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

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val newPoints = getSelectingPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx)
      onClick(e, newPoints.sortBy(_._2).map(pair => data(pair._1)))
    }
  })

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)
      val newPoints = getSelectingPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx)
      if (mouseOverIndices != newPoints.map(_._1)) {
        mouseOverIndices = newPoints.map(_._1)
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
      val newRange = JDsePlot.scrollNewRange(axesRange(axisIndex), zoomFactor, 1 - (e.getY.toFloat / getHeight))
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
  addMouseListener(dragListener) // this registers the press / release
  addMouseMotionListener(dragListener) // this registers the dragged

  override def getToolTipText(e: MouseEvent): String = {
    getSelectingPointsForLocation(e.getX, e.getY, JDsePlot.kSnapDistancePx).headOption match {
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
