package edg_ide.ui

import com.intellij.openapi.ui.ComboBox
import edg.compiler.{ExprValue, FloatValue, IntValue, RangeType, RangeValue}
import edg_ide.dse.{CombinedDseResultSet, DseConfigElement, DseObjective, DseObjectiveFootprintArea, DseObjectiveFootprintCount, DseObjectiveParameter, DseResult}
import edg_ide.swing.SwingHtmlUtil
import edg_ide.swing.dse.JScatterPlot

import java.awt.{GridBagConstraints, GridBagLayout}
import java.awt.event.{ItemEvent, ItemListener}
import javax.swing.JPanel


sealed trait PlotAxis {
  def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType)
}

class DummyAxis(val name: String) extends PlotAxis {
  override def toString = name

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    (results.map(_ => Some(0)), Some(Seq())) // zero everything so other axes can display
  }
}

class DseObjectiveAxis(objective: DseObjective) extends PlotAxis {
  override def toString = objective.objectiveToString

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      objective.calculate(result.compiled, result.compiler) match {
        case x: Float => Some(x)
        case x: Int => Some(x.toFloat)
        case _ => None
      }
    }
    (values, None)
  }
}

class DseObjectiveMappedAxis(objective: DseObjectiveParameter, postfix: String,
                             map: ExprValue => Option[Float]) extends PlotAxis {
  override def toString = objective.objectiveToString + postfix

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      objective.calculate(result.compiled, result.compiler).flatMap(map)
    }
    (values, None)
  }
}

class DseObjectiveOrdinalAxis(objective: DseObjectiveParameter) extends PlotAxis {
  override def toString = objective.objectiveToString

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      objective.calculate(result.compiled, result.compiler).map(_.toStringValue)
    }
    val stringToPos = values.flatten.distinct.sorted.zipWithIndex.map { case (str, index) => (str, index.toFloat) }
    val axis = stringToPos.map { case (str, index) => (index, str) }

    val stringToPosMap = stringToPos.toMap
    val positionalValues = values.map { value =>
      value.flatMap { value =>
        stringToPosMap.get(value)
      }
    }
    (positionalValues, Some(axis))
  }
}


class DsePlotPanel() extends JPanel {
  // Data State
  private var combinedResults = new CombinedDseResultSet(Seq())  // reflects the data points
  private var displayAxisSelectorObjectives: Seq[DseObjective] = Seq()  // reflects the widget display

  setLayout(new GridBagLayout)

  private val plot = new JScatterPlot[DseResult]() {
    override def onClick(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onClick(data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onHoverChange(data.map(_.value))
    }
  }
  add(plot, Gbc(0, 0, GridBagConstraints.BOTH, 2))

  private val xSelector = new ComboBox[PlotAxis]()
  private val xAxisHeader = new DummyAxis("X Axis")
  xSelector.addItem(xAxisHeader)
  add(xSelector, Gbc(0, 1, GridBagConstraints.HORIZONTAL))
  private val ySelector = new ComboBox[PlotAxis]()
  private val yAxisHeader = new DummyAxis("Y Axis")
  ySelector.addItem(yAxisHeader)
  add(ySelector, Gbc(1, 1, GridBagConstraints.HORIZONTAL))

  private def updatePlot(): Unit = {
    val examples = combinedResults.groupedResults.map(_.head)
    val (xPoints, xAxis) = xSelector.getItem.resultsToValuesAxis(examples)
    val (yPoints, yAxis) = ySelector.getItem.resultsToValuesAxis(examples)

    val points = combinedResults.groupedResults.zip(xPoints.zip(yPoints)).toIndexedSeq.flatMap {
      case (resultSet, (Some(xVal), Some(yVal))) => resultSet.flatMap { result =>
        val color = if (result.errors.nonEmpty) {
          Some(com.intellij.ui.JBColor.RED)
        } else {
          None
        }
        val tooltipText = DseConfigElement.configMapToString(result.config)
        Some(new plot.Data(result, xVal, yVal, color,
          Some(SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont))))
      }
      case _ => Seq()
    }
    plot.setData(points, xAxis, yAxis)
  }

  private def updateAxisSelectors(objectives: Seq[DseObjective]): Unit = {
    if (objectives == displayAxisSelectorObjectives) {
      return  // nothing needs to be done
    }

    val selectedX = xSelector.getItem
    val selectedY = ySelector.getItem

    val items = objectives flatMap {
      case objective: DseObjectiveFootprintArea => Seq(new DseObjectiveAxis(objective))
      case objective: DseObjectiveFootprintCount => Seq(new DseObjectiveAxis(objective))
      case objective: DseObjectiveParameter if objective.exprType == classOf[FloatValue] =>
        Seq(new DseObjectiveMappedAxis(objective, "", param => Some(param.asInstanceOf[FloatValue].value)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[IntValue] =>
        Seq(new DseObjectiveMappedAxis(objective, "", param => Some(param.asInstanceOf[IntValue].toFloat)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[RangeType] => Seq(
        new DseObjectiveMappedAxis(objective, " (min)", {
          case RangeValue(lower, upper) => Some(lower)
          case _ => None
        }),
        new DseObjectiveMappedAxis(objective, " (max)", {
          case RangeValue(lower, upper) => Some(upper)
          case _ => None
        })
      )
      case objective: DseObjectiveParameter =>
        Seq(new DseObjectiveOrdinalAxis(objective))
      case objective => Seq(new DummyAxis(f"unknown ${objective.objectiveToString}"))
    }

    xSelector.removeItemListener(axisSelectorListener)
    ySelector.removeItemListener(axisSelectorListener)

    xSelector.removeAllItems()
    ySelector.removeAllItems()

    xSelector.addItem(xAxisHeader)
    ySelector.addItem(yAxisHeader)
    items.foreach { item =>
      xSelector.addItem(item)
      ySelector.addItem(item)
    }

    xSelector.addItemListener(axisSelectorListener)
    ySelector.addItemListener(axisSelectorListener)
    displayAxisSelectorObjectives = objectives

    // restore prior selection by name matching
    items.find { item => item.toString == selectedX.toString }.foreach { item => xSelector.setItem(item) }
    items.find { item => item.toString == selectedY.toString }.foreach { item => ySelector.setItem(item) }
  }

  private val axisSelectorListener = new ItemListener() {
    override def itemStateChanged(e: ItemEvent): Unit = {
      if (e.getStateChange == ItemEvent.SELECTED) {
        updatePlot()
      }
    }
  }

  def setResults(combinedResults: CombinedDseResultSet, objectives: Seq[DseObjective]): Unit = {
    updateAxisSelectors(objectives)

    this.combinedResults = combinedResults
    updatePlot()
  }

  def setSelection(results: Seq[DseResult]): Unit = {
    plot.setSelected(results)
  }

  // User hooks - can be overridden
  //
  // called when this widget clicked, for all points within some hover radius of the cursor
  // sorted by distance from cursor (earlier = closer), and may be empty
  def onClick(results: Seq[DseResult]): Unit = {}

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(results: Seq[DseResult]): Unit = {}
}