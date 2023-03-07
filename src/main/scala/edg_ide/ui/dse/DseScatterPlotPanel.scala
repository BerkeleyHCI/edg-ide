package edg_ide.ui.dse

import com.intellij.icons.AllIcons
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.ui.ComboBox
import com.intellij.ui.components.JBLabel
import edg_ide.dse._
import edg_ide.swing.SwingHtmlUtil
import edg_ide.swing.dse._
import edg_ide.ui.Gbc

import java.awt.event.{ActionEvent, ItemEvent, ItemListener, MouseEvent}
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing.{JButton, JPanel}


abstract class DseBasePlot extends JPanel {
  // API functions
  //
  def setResults(combinedResults: CombinedDseResultSet, search: Seq[DseConfigElement],
                 objectives: Seq[DseObjective]): Unit
  def setSelection(results: Seq[DseResult]): Unit

  // User hooks - can be overridden
  //
  // called when this widget clicked, for all points within some hover radius of the cursor
  // sorted by distance from cursor (earlier = closer), and may be empty
  def onClick(e: MouseEvent, results: Seq[DseResult]): Unit = {}

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(results: Seq[DseResult]): Unit = {}

  // called when the switch plot button is clicked
  def onSwitchClick(): Unit = {}
}


class DseScatterPlotPanel() extends DseBasePlot {
  // Data State
  private var combinedResults = new CombinedDseResultSet(Seq())  // reflects the data points
  private var displayAxisSelector: (Seq[DseConfigElement], Seq[DseObjective]) = (Seq(), Seq())  // reflects the widget display

  setLayout(new GridBagLayout)

  private val scatterPlot = new JScatterPlot[DseResult]() {
    override def onClick(e: MouseEvent, data: Seq[Data]): Unit = {
      DseScatterPlotPanel.this.onClick(e, data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DseScatterPlotPanel.this.onHoverChange(data.map(_.value))
    }
  }
  add(scatterPlot, Gbc(0, 0, GridBagConstraints.BOTH, 5))

  private val plotSwitchButton = new JButton()
  plotSwitchButton.setIcon(AllIcons.Toolwindows.ToolWindowMessages)
  plotSwitchButton.setToolTipText("Switch plot type")
  plotSwitchButton.addActionListener((actionEvent: ActionEvent) => onSwitchClick())
  add(plotSwitchButton, Gbc(0, 1))

  private val emptyAxis = new DummyAxis("Empty")
  private val ySelector = new ComboBox[PlotAxis]()
  ySelector.addItem(emptyAxis)  // don't need listener yet, does nothing
  add(new JBLabel("Y ↑"), Gbc(1, 1))
  add(ySelector, Gbc(2, 1, GridBagConstraints.HORIZONTAL))

  private val xSelector = new ComboBox[PlotAxis]()
  xSelector.addItem(emptyAxis)  // don't need listener yet, does nothing
  add(xSelector, Gbc(3, 1, GridBagConstraints.HORIZONTAL))
  add(new JBLabel("X →"), Gbc(4, 1))

  private def updatePlot(): Unit = {
    val flatResults = combinedResults.groupedResults.flatten
    val (xPoints, xAxis) = xSelector.getItem.resultsToValuesAxis(flatResults)
    val (yPoints, yAxis) = ySelector.getItem.resultsToValuesAxis(flatResults)

    require(flatResults.size == xPoints.size, s"X axis points mismatch, got ${xPoints.size} expected ${flatResults.size}")
    require(flatResults.size == yPoints.size, s"Y axis points mismatch, got ${xPoints.size} expected ${flatResults.size}")

    val scatterPoints = flatResults.zip(xPoints.zip(yPoints)).toIndexedSeq.flatMap {
      case (result, (Some(xVal), Some(yVal))) =>
        val (idealErrors, otherErrors) = DseResultModel.partitionByIdeal(result.errors)
        val color = (idealErrors.nonEmpty, otherErrors.nonEmpty) match {
          case (_, true) => Some(DseResultModel.kColorOtherError)
          case (true, false) => Some(DseResultModel.kColorIdealError)
          case (false, false) => None
        }
        val tooltipText = DseConfigElement.configMapToString(result.config)
        Some(new scatterPlot.Data(result, xVal, yVal, color,
          Some(SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont))))
      case _ => Seq()
    }

    ApplicationManager.getApplication.invokeLater(() => {
      scatterPlot.setData(scatterPoints, xAxis, yAxis)
    })
  }

  private def updateAxisSelectors(search: Seq[DseConfigElement], objectives: Seq[DseObjective]): Unit = {
    if ((search, objectives) == displayAxisSelector) {
      return // nothing needs to be done
    }
    displayAxisSelector = (search, objectives)

    ApplicationManager.getApplication.invokeLater(() => {
      val items = search.flatMap(PlotAxis.fromSearchConfig) ++ objectives.flatMap(PlotAxis.fromObjective)
      Seq(xSelector, ySelector).foreach { axisSelector =>
        val selected = axisSelector.getItem

        axisSelector.removeItemListener(axisSelectorListener)
        axisSelector.removeAllItems()
        axisSelector.addItem(emptyAxis)
        items.foreach { item =>
          axisSelector.addItem(item)
        }
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

  override def setResults(combinedResults: CombinedDseResultSet, search: Seq[DseConfigElement],
                 objectives: Seq[DseObjective]): Unit = {
    updateAxisSelectors(search, objectives)

    this.combinedResults = combinedResults
    updatePlot()
  }

  override def setSelection(results: Seq[DseResult]): Unit = {
    scatterPlot.setSelected(results)
  }
}
