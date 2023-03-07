package edg_ide.ui.dse

import com.intellij.icons.AllIcons
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.ui.ComboBox
import edg_ide.dse._
import edg_ide.swing.SwingHtmlUtil
import edg_ide.swing.dse._
import edg_ide.ui.Gbc

import java.awt.event._
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing.{JButton, JPanel}


class DseParallelPlotPanel() extends JPanel {
  // Data State
  private var combinedResults = new CombinedDseResultSet(Seq())  // reflects the data points
  private var displayAxisSelector: (Seq[DseConfigElement], Seq[DseObjective]) = (Seq(), Seq())  // reflects the widget display

  val thisLayout = new GridBagLayout
  setLayout(thisLayout)

  private val parallelPlot = new JParallelCoordinatesPlot[DseResult]() {
    override def onClick(e: MouseEvent, data: Seq[Data]): Unit = {
      DseParallelPlotPanel.this.onClick(e, data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DseParallelPlotPanel.this.onHoverChange(data.map(_.value))
    }
  }
  add(parallelPlot, Gbc(0, 0, GridBagConstraints.BOTH, 3))
  parallelPlot.setData(IndexedSeq(), IndexedSeq(None))  // reflect Empty status

  private val plotSwitchButton = new JButton()
  plotSwitchButton.setIcon(AllIcons.Toolwindows.ToolWindowMessages)
  plotSwitchButton.setToolTipText("Switch plot type")
  add(plotSwitchButton, Gbc(0, 1))

  // default to one axis
  private val emptyAxis = new DummyAxis("Empty")
  private val deleteAxis = new DummyAxis("Delete")

  private var axisSelectors: Seq[ComboBox[PlotAxis]] = Seq(new ComboBox[PlotAxis]())
  axisSelectors.head.addItem(emptyAxis)
  axisSelectors.head.addItemListener(axisSelectorListener)
  add(axisSelectors.head, Gbc(1, 1, GridBagConstraints.HORIZONTAL))

  private val axisAddButton = new JButton()
  axisAddButton.setIcon(AllIcons.General.Add)
  axisAddButton.setToolTipText("Add plot axis")
  axisAddButton.addActionListener(new ActionListener {
    override def actionPerformed(actionEvent: ActionEvent): Unit = {
      ApplicationManager.getApplication.invokeLater(() => {
        // add the new axis at the end, updating layout constraints as needed
        val newAxis = new ComboBox[PlotAxis]()
        newAxis.addItem(emptyAxis)
        axisSelectors = axisSelectors :+ newAxis
        add(newAxis, Gbc(axisSelectors.size, 1, GridBagConstraints.HORIZONTAL))
        thisLayout.setConstraints(axisAddButton, Gbc(axisSelectors.size + 1, 1))
        thisLayout.setConstraints(parallelPlot, Gbc(0, 0, GridBagConstraints.BOTH, axisSelectors.size + 2))

        // update all the axes to populate the menu items
        val (search, objectives) = displayAxisSelector
        updateAxisSelectors(search, objectives, true)  // TODO doesn't need to be rewrapped in invokeLater
        updatePlot()  // TODO doesn't need to be rewrapped in invokeLater
      })
    }
  })
  add(axisAddButton, Gbc(2, 1))

  private def updatePlot(): Unit = {
    val flatResults = combinedResults.groupedResults.flatten
    val (pointsByAxis, axisByAxis) = axisSelectors.zipWithIndex.map { case (axisSelector, index) =>
      val (points, axis) = axisSelector.getItem.resultsToValuesAxis(flatResults)
      require(flatResults.size == points.size, s"Axis ${index} points mismatch, got ${points.size} expected ${flatResults.size}")
      (points, axis)
    }.unzip

    val parallelPoints = flatResults.toIndexedSeq.zipWithIndex.map { case (result, dataIndex) =>
      val (idealErrors, otherErrors) = DseResultModel.partitionByIdeal(result.errors)
      val color = (idealErrors.nonEmpty, otherErrors.nonEmpty) match {
        case (_, true) => Some(DseResultModel.kColorOtherError)
        case (true, false) => Some(DseResultModel.kColorIdealError)
        case (false, false) => None
      }
      val tooltipText = DseConfigElement.configMapToString(result.config)
      val axisValues = axisSelectors.indices.map { axisIndex =>
        pointsByAxis(axisIndex)(dataIndex)
      }.toIndexedSeq

      new parallelPlot.Data(result, axisValues, color,
        Some(SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont)))
    }

    ApplicationManager.getApplication.invokeLater(() => {
      parallelPlot.setData(parallelPoints, axisByAxis.toIndexedSeq)
    })
  }

  private def updateAxisSelectors(search: Seq[DseConfigElement], objectives: Seq[DseObjective],
                                  forced: Boolean = false): Unit = {
    if ((search, objectives) == displayAxisSelector && !forced) {
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
        if (axisSelectors.length > 1) {  // don't allow only axis to be deleted
          axisSelector.addItem(deleteAxis)
        }
        axisSelector.addItemListener(axisSelectorListener)

        // restore prior selection by name matching
        items.find { item => item.toString == selected.toString }.foreach { item => axisSelector.setItem(item) }
      }
      revalidate()
    })
  }

  private val axisSelectorListener = new ItemListener() {
    override def itemStateChanged(e: ItemEvent): Unit = {
      if (e.getStateChange == ItemEvent.SELECTED) {
        ApplicationManager.getApplication.invokeLater(() => {
          val (filteredAxisSelectors, removedAxisSelectors) = axisSelectors.partition { axisSelector =>
            axisSelector.getItem != deleteAxis
          }
          if (removedAxisSelectors.nonEmpty) {
            axisSelectors = filteredAxisSelectors
            removedAxisSelectors.foreach { axisSelector =>
              remove(axisSelector)
            }
            filteredAxisSelectors.zipWithIndex.foreach { case (axisSelector, axisIndex) =>
              thisLayout.setConstraints(axisSelector, Gbc(axisIndex + 1, 1, GridBagConstraints.HORIZONTAL))
              axisSelector
            }
            thisLayout.setConstraints(axisAddButton, Gbc(axisSelectors.size + 1, 1))
            thisLayout.setConstraints(parallelPlot, Gbc(0, 0, GridBagConstraints.BOTH, axisSelectors.size + 2))
          }
          revalidate()

          updatePlot()  // TODO doesn't need to be wrapped in invokeLater
        })
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
