package edg_ide.swing.dse

import edg.compiler.ExprValue
import edg_ide.dse.{DseConfigElement, DseObjective, DseObjectiveParameter, DseResult}


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
      result.objectives.get(objective).flatMap {
        case x: Float => Some(x)
        case x: Int => Some(x.toFloat)
        case _ => None
      }
    }
    (values, None)
  }
}

class DseObjectiveParamAxis(objective: DseObjectiveParameter, postfix: String,
                            mapFn: ExprValue => Option[Float]) extends PlotAxis {
  override def toString = objective.objectiveToString + postfix

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      result.objectives.get(objective).flatMap(value =>
        value.asInstanceOf[Option[ExprValue]].flatMap(value =>
          mapFn(value)))
    }
    (values, None)
  }
}

class DseObjectiveParamOrdinalAxis(objective: DseObjectiveParameter) extends PlotAxis {
  override def toString = objective.objectiveToString

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      result.objectives.get(objective).flatMap(value =>
        value.asInstanceOf[Option[ExprValue]].map(_.toStringValue))
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


class DseConfigParamAxis(config: DseConfigElement, postfix: String,
                         map: ExprValue => Option[Float]) extends PlotAxis {
  override def toString = config.configToString + postfix

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      result.config.get(config).flatMap(value => map(value.asInstanceOf[ExprValue]))
    }
    (values, None)
  }
}


class DseConfigOrdinalAxis(config: DseConfigElement) extends PlotAxis {
  override def toString = config.configToString

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
    val values = results.map { result =>
      result.config.get(config).map(config.valueToString)
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
