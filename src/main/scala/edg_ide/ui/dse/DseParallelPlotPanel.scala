package edg_ide.ui.dse

import com.intellij.icons.AllIcons
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.ui.ComboBox
import edg_ide.dse._
import edg_ide.swing.SwingHtmlUtil
import edg_ide.swing.dse._
import edg_ide.ui.Gbc

import java.awt.event.{ItemEvent, ItemListener, MouseEvent}
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing.{JButton, JPanel}


class DseParallelPlotPanel() extends JPanel {
  // Data State
  private var combinedResults = new CombinedDseResultSet(Seq())  // reflects the data points
  private var displayAxisSelector: (Seq[DseConfigElement], Seq[DseObjective]) = (Seq(), Seq())  // reflects the widget display

  setLayout(new GridBagLayout)

  private val parallelPlot = new JParallelCoordinatesPlot[DseResult]() {
    override def onClick(e: MouseEvent, data: Seq[Data]): Unit = {
      DseParallelPlotPanel.this.onClick(e, data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DseParallelPlotPanel.this.onHoverChange(data.map(_.value))
    }
  }
  add(parallelPlot, Gbc(0, 0, GridBagConstraints.BOTH, 100))

  private val plotSwitchButton = new JButton()
  plotSwitchButton.setIcon(AllIcons.Toolwindows.ToolWindowMessages)
  plotSwitchButton.setToolTipText("Switch plot type")

  private val axisAddButton = new JButton()
  axisAddButton.setIcon(AllIcons.General.Add)
  axisAddButton.setToolTipText("Add plot axis")

  private val emptyAxis = new DummyAxis("Empty")
  private val deleteAxis = new DummyAxis("Delete")

  // default to one axis
  private val axisSelectors: Seq[ComboBox[PlotAxis]] = Seq(new ComboBox[PlotAxis]())
  axisSelectors.head.addItem(emptyAxis)
  axisSelectors.head.addItem(deleteAxis)

  private def updatePlot(): Unit = {
    val flatResults = combinedResults.groupedResults.flatten
    val (pointsByAxis, axisByAxis) = axisSelectors.zipWithIndex.map { case (axisSelector, index) =>
      val (points, axis) = axisSelector.getItem.resultsToValuesAxis(flatResults)
      require(flatResults.size == points.size, s"Axis ${index} points mismatch, got ${points.size} expected ${flatResults.size}")
      (points, axis)
    }.unzip

    val parallelPoints = flatResults.zipWithIndex.flatMap { case (result, index) =>


//      case (result, (Some(xVal), Some(yVal))) =>
//        val (idealErrors, otherErrors) = DseResultModel.partitionByIdeal(result.errors)
//        val color = (idealErrors.nonEmpty, otherErrors.nonEmpty) match {
//          case (_, true) => Some(DseResultModel.kColorOtherError)
//          case (true, false) => Some(DseResultModel.kColorIdealError)
//          case (false, false) => None
//        }
//        val tooltipText = DseConfigElement.configMapToString(result.config)
//        Some(new parallelPlot.Data(result, IndexedSeq(xVal, yVal), color,
//          Some(SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont))))
//      case _ => Seq()
    }

    ApplicationManager.getApplication.invokeLater(() => {
      parallelPlot.setData(parallelPoints, axisByAxis.toIndexedSeq)
    })
  }

  private def updateAxisSelectors(search: Seq[DseConfigElement], objectives: Seq[DseObjective]): Unit = {
    if ((search, objectives) == displayAxisSelector) {
      return  // nothing needs to be done
    }
    displayAxisSelector = (search, objectives)

    ApplicationManager.getApplication.invokeLater(() => {
      val items = search.flatMap(PlotAxis.fromSearchConfig) ++ objectives.flatMap(PlotAxis.fromObjective)
      axisSelectors.foreach { axisSelector =>
        val selected = axisSelector.getItem

        axisSelector.removeItemListener(axisSelectorListener)
        axisSelector.removeAllItems()
        axisSelector.addItem(emptyAxis)
        items.foreach { item =>
          axisSelector.addItem(item)
        }
        axisSelector.addItem(deleteAxis)
        axisSelector.addItemListener(axisSelectorListener)

        // restore prior selection by name matching
        items.find { item => item.toString == selected.toString }.foreach { item => axisSelector.setItem(item) }
      }
    })
  }

  private val axisSelectorListener = new ItemListener() {
    override def itemStateChanged(e: ItemEvent): Unit = {
      if (e.getStateChange == ItemEvent.SELECTED) {
        updatePlot()
      }
    }
  }

  def setResults(combinedResults: CombinedDseResultSet, search: Seq[DseConfigElement],
                 objectives: Seq[DseObjective]): Unit = {
    updateAxisSelectors(search, objectives)

    this.combinedResults = combinedResults
    updatePlot()
  }

  def setSelection(results: Seq[DseResult]): Unit = {
    parallelPlot.setSelected(results)
  }

  // User hooks - can be overridden
  //
  // called when this widget clicked, for all points within some hover radius of the cursor
  // sorted by distance from cursor (earlier = closer), and may be empty
  def onClick(e: MouseEvent, results: Seq[DseResult]): Unit = {}

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(results: Seq[DseResult]): Unit = {}
}
