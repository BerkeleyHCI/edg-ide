package edg_ide.ui.dse

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.ui.ComboBox
import com.intellij.ui.components.JBLabel
import edg.compiler._
import edg_ide.dse._
import edg_ide.swing.SwingHtmlUtil
import edg_ide.swing.dse.{DseConfigOrdinalAxis, DseConfigParamAxis, DseObjectiveAxis, DseObjectiveParamAxis, DseObjectiveParamOrdinalAxis, DseResultModel, DummyAxis, JParallelCoordinatesPlot, JScatterPlot, PlotAxis}
import edg_ide.ui.Gbc

import java.awt.event.{ItemEvent, ItemListener, MouseEvent}
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing.JPanel


class DsePlotPanel() extends JPanel {
  // Data State
  private var combinedResults = new CombinedDseResultSet(Seq())  // reflects the data points
  private var displayAxisSelector: (Seq[DseConfigElement], Seq[DseObjective]) = (Seq(), Seq())  // reflects the widget display

  setLayout(new GridBagLayout)

  private val scatterPlot = new JScatterPlot[DseResult]() {
    override def onClick(e: MouseEvent, data: Seq[Data]): Unit = {
      DsePlotPanel.this.onClick(e, data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onHoverChange(data.map(_.value))
    }
  }

  private val parallelPlot = new JParallelCoordinatesPlot[DseResult]() {
    override def onClick(e: MouseEvent, data: Seq[Data]): Unit = {
      DsePlotPanel.this.onClick(e, data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onHoverChange(data.map(_.value))
    }
  }

//  add(scatterPlot, Gbc(0, 0, GridBagConstraints.BOTH, 4))
  add(parallelPlot, Gbc(0, 0, GridBagConstraints.BOTH, 4))

  parallelPlot.setData(IndexedSeq(), IndexedSeq(None, None, None))

  private val emptyAxis = new DummyAxis("Empty")
  private val ySelector = new ComboBox[PlotAxis]()
  ySelector.addItem(emptyAxis)
  add(new JBLabel("Y ↑"), Gbc(0, 1))
  add(ySelector, Gbc(1, 1, GridBagConstraints.HORIZONTAL))

  private val xSelector = new ComboBox[PlotAxis]()
  xSelector.addItem(emptyAxis)
  add(xSelector, Gbc(2, 1, GridBagConstraints.HORIZONTAL))
  add(new JBLabel("X →"), Gbc(3, 1))

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

    // TODO separate out
    val parallelPoints = flatResults.zip(xPoints.zip(yPoints)).toIndexedSeq.flatMap {
      case (result, (Some(xVal), Some(yVal))) =>
        val (idealErrors, otherErrors) = DseResultModel.partitionByIdeal(result.errors)
        val color = (idealErrors.nonEmpty, otherErrors.nonEmpty) match {
          case (_, true) => Some(DseResultModel.kColorOtherError)
          case (true, false) => Some(DseResultModel.kColorIdealError)
          case (false, false) => None
        }
        val tooltipText = DseConfigElement.configMapToString(result.config)
        Some(new parallelPlot.Data(result, IndexedSeq(xVal, yVal), color,
          Some(SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont))))
      case _ => Seq()
    }

    ApplicationManager.getApplication.invokeLater(() => {
      scatterPlot.setData(scatterPoints, xAxis, yAxis)

      // TODO separate out
      parallelPlot.setData(parallelPoints, IndexedSeq(xAxis, yAxis))
    })
  }

  private def updateAxisSelectors(search: Seq[DseConfigElement], objectives: Seq[DseObjective]): Unit = {
    if ((search, objectives) == displayAxisSelector) {
      return  // nothing needs to be done
    }

    val selectedX = xSelector.getItem
    val selectedY = ySelector.getItem

    val items = search.flatMap {
      case config: DseParameterSearch if config.values.forall(_.isInstanceOf[FloatValue]) =>
        Seq(new DseConfigParamAxis(config, "", expr => Some(expr.asInstanceOf[FloatValue].value)))
      case config: DseParameterSearch if config.values.forall(_.isInstanceOf[IntValue]) =>
        Seq(new DseConfigParamAxis(config, "", expr => Some(expr.asInstanceOf[IntValue].toFloat)))
      case config: DseParameterSearch if config.values.forall(_.isInstanceOf[RangeType]) => Seq(
        new DseConfigParamAxis(config, " (min)", {
          case RangeValue(lower, upper) => Some(lower)
          case _ => None
        }),
        new DseConfigParamAxis(config, " (mid)", {
          case RangeValue(lower, upper) => Some((lower + upper) / 2)
          case _ => None
        }),
        new DseConfigParamAxis(config, " (max)", {
          case RangeValue(lower, upper) => Some(upper)
          case _ => None
        })
      )
      case config => Seq(new DseConfigOrdinalAxis(config))
    } ++ objectives.flatMap {
      case objective: DseObjectiveFootprintArea => Seq(new DseObjectiveAxis(objective))
      case objective: DseObjectiveFootprintCount => Seq(new DseObjectiveAxis(objective))
      case objective: DseObjectiveParameter if objective.exprType == classOf[FloatValue] =>
        Seq(new DseObjectiveParamAxis(objective, "", param => Some(param.asInstanceOf[FloatValue].value)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[IntValue] =>
        Seq(new DseObjectiveParamAxis(objective, "", param => Some(param.asInstanceOf[IntValue].toFloat)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[RangeType] => Seq(
        new DseObjectiveParamAxis(objective, " (min)", {
          case RangeValue(lower, upper) => Some(lower)
          case _ => None
        }),
        new DseObjectiveParamAxis(objective, " (mid)", {
          case RangeValue(lower, upper) => Some((lower + upper) / 2)
          case _ => None
        }),
        new DseObjectiveParamAxis(objective, " (max)", {
          case RangeValue(lower, upper) => Some(upper)
          case _ => None
        })
      )
      case objective: DseObjectiveParameter =>
        Seq(new DseObjectiveParamOrdinalAxis(objective))
      case objective => Seq(new DummyAxis(f"unknown ${objective.objectiveToString}"))
    }
    displayAxisSelector = (search, objectives)

    ApplicationManager.getApplication.invokeLater(() => {
      xSelector.removeItemListener(axisSelectorListener)
      ySelector.removeItemListener(axisSelectorListener)

      xSelector.removeAllItems()
      ySelector.removeAllItems()

      xSelector.addItem(emptyAxis)
      ySelector.addItem(emptyAxis)
      items.foreach { item =>
        xSelector.addItem(item)
        ySelector.addItem(item)
      }

      xSelector.addItemListener(axisSelectorListener)
      ySelector.addItemListener(axisSelectorListener)
      // restore prior selection by name matching
      items.find { item => item.toString == selectedX.toString }.foreach { item => xSelector.setItem(item) }
      items.find { item => item.toString == selectedY.toString }.foreach { item => ySelector.setItem(item) }
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
    scatterPlot.setSelected(results)
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
