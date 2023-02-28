package edg_ide.ui

import com.intellij.openapi.ui.ComboBox
import edg.compiler.{ExprValue, FloatValue, IntValue, RangeType, RangeValue}
import edg_ide.dse.{CombinedDseResultSet, DseConfigElement, DseObjective, DseObjectiveFootprintArea, DseObjectiveFootprintCount, DseObjectiveParameter, DseResult}
import edg_ide.swing.SwingHtmlUtil
import edg_ide.swing.dse.JScatterPlot

import java.awt.{GridBagConstraints, GridBagLayout}
import java.awt.event.{ItemEvent, ItemListener}
import javax.swing.JPanel


sealed trait AxisItem {
  def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType)
}

class DummyAxisItem(val name: String) extends AxisItem {
  override def toString = name

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    (results.map(_ => Some(0)), Some(Seq())) // zero everything so other axes can display
  }
}

class DseObjectiveItem(objective: DseObjective) extends AxisItem {
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

class DseObjectiveParameterItem(objective: DseObjectiveParameter, postfix: String,
                                map: ExprValue => Option[Float]) extends AxisItem {
  override def toString = objective.objectiveToString + postfix

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      objective.calculate(result.compiled, result.compiler).flatMap(map)
    }
    (values, None)
  }
}

class DseObjectiveParameterStringItem(objective: DseObjectiveParameter) extends AxisItem {
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

  private val plot = new JScatterPlot[Seq[DseResult]]() {
    override def onClick(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onClick(data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onHoverChange(data.map(_.value))
    }
  }
  add(plot, Gbc(0, 0, GridBagConstraints.BOTH, 2))

  private val xSelector = new ComboBox[AxisItem]()
  private val xAxisHeader = new DummyAxisItem("X Axis")
  xSelector.addItem(xAxisHeader)
  add(xSelector, Gbc(0, 1, GridBagConstraints.HORIZONTAL))
  private val ySelector = new ComboBox[AxisItem]()
  private val yAxisHeader = new DummyAxisItem("Y Axis")
  ySelector.addItem(yAxisHeader)
  add(ySelector, Gbc(1, 1, GridBagConstraints.HORIZONTAL))

  private def updatePlot(): Unit = {
    val examples = combinedResults.groupedResults.map(_.head)
    val (xPoints, xAxis) = xSelector.getItem.resultsToValuesAxis(examples)
    val (yPoints, yAxis) = ySelector.getItem.resultsToValuesAxis(examples)

    val points = combinedResults.groupedResults.zip(xPoints.zip(yPoints)).toIndexedSeq.flatMap {
      case (resultSet, (Some(xVal), Some(yVal))) =>
        val example = resultSet.head
        val color = if (example.errors.nonEmpty) {
          Some(com.intellij.ui.JBColor.RED)
        } else {
          None
        }

        val objectivesStr = example.objectives.map { case (objective, value) =>
          f"$objective=${DseConfigElement.valueToString(value)}"
        }.mkString("\n")
        val resultsStr = resultSet.map { result =>
          DseConfigElement.configMapToString(result.config)
        }.mkString("\n")
        val tooltipText = objectivesStr + f"\n\n${resultSet.size} results:\n" + resultsStr

        Some(new plot.Data(resultSet, xVal, yVal, color,
          Some(SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont))))
      case _ => None
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
      case objective: DseObjectiveFootprintArea => Seq(new DseObjectiveItem(objective))
      case objective: DseObjectiveFootprintCount => Seq(new DseObjectiveItem(objective))
      case objective: DseObjectiveParameter if objective.exprType == classOf[FloatValue] =>
        Seq(new DseObjectiveParameterItem(objective, "", param => Some(param.asInstanceOf[FloatValue].value)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[IntValue] =>
        Seq(new DseObjectiveParameterItem(objective, "", param => Some(param.asInstanceOf[IntValue].toFloat)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[RangeType] => Seq(
        new DseObjectiveParameterItem(objective, " (min)", {
          case RangeValue(lower, upper) => Some(lower)
          case _ => None
        }),
        new DseObjectiveParameterItem(objective, " (max)", {
          case RangeValue(lower, upper) => Some(upper)
          case _ => None
        })
      )
      case objective: DseObjectiveParameter =>
        Seq(new DseObjectiveParameterStringItem(objective))
      case objective => Seq(new DummyAxisItem(f"unknown ${objective.objectiveToString}"))
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

  def setSelection(resultSets: Seq[Seq[DseResult]]): Unit = {
    plot.setSelected(resultSets)
  }

  // User hooks - can be overridden
  //
  // called when this widget clicked, for all points within some hover radius of the cursor
  // sorted by distance from cursor (earlier = closer), and may be empty
  def onClick(data: Seq[Seq[DseResult]]): Unit = {}

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(data: Seq[Seq[DseResult]]): Unit = {}

}