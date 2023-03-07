package edg_ide.swing.dse

import com.intellij.ui.JBColor
import edg_ide.swing.{ColorUtil, DrawAnchored}

import java.awt.event._
import java.awt.{Color, Graphics, Point}
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
  private var axes: IndexedSeq[Option[JScatterPlot.AxisType]] = IndexedSeq()
  private var data: IndexedSeq[Data] = IndexedSeq()
  private var mouseOverIndices: Seq[Int] = Seq() // sorted by increasing index
  private var selectedIndices: Seq[Int] = Seq() // unsorted

  // UI state
  private var axesRange: Seq[(Float, Float)] = Seq()  // range for each axis

  // axis MUST be defined for each position, but can be None for a numeric axis
  def setData(data: IndexedSeq[Data], axes: IndexedSeq[Option[JScatterPlot.AxisType]]): Unit = {
    this.data = data
    this.axes = axes
    mouseOverIndices = Seq() // clear
    selectedIndices = Seq() // clear

    axesRange = axes.zipWithIndex.map { case (axis, index) =>
      val values = data.flatMap { elt =>
        if (index < elt.positions.length) {
          None
        } else {
          Some(elt.positions(index))
        }
      }
      JScatterPlot.expandedRange((values.min, values.max))
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


}
