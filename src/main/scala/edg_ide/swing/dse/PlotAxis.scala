package edg_ide.swing.dse

import edg.compiler.{ExprValue, FloatValue, IntValue, RangeType, RangeValue}
import edg_ide.dse.{DseConfigElement, DseNumericObjective, DseObjective, DseObjectiveFootprintArea, DseObjectiveFootprintCount, DseObjectiveParameter, DseParameterSearch, DseRefinementElement, DseResult}


object PlotAxis {
  type AxisType = Option[Seq[(Float, String)]]

  // creates plot axes for a search config
  def fromSearchConfig(config: DseConfigElement): Seq[PlotAxis] = config match {
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
  }

  // creates plot axes for an objective function
  def fromObjective(objective: DseObjective): Seq[PlotAxis] = objective match {
    case objective: DseNumericObjective => Seq(new DseObjectiveAxis(objective))
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
}


sealed trait PlotAxis {
  def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], PlotAxis.AxisType)
}

class DummyAxis(val name: String) extends PlotAxis {
  override def toString = name

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], PlotAxis.AxisType) = {
    (results.map(_ => Some(0)), Some(Seq())) // zero everything so other axes can display
  }
}

class DseObjectiveAxis(objective: DseObjective) extends PlotAxis {
  override def toString = objective.objectiveToString

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], PlotAxis.AxisType) = {
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

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], PlotAxis.AxisType) = {
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

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], PlotAxis.AxisType) = {
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

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], PlotAxis.AxisType) = {
    val values = results.map { result =>
      result.config.get(config).flatMap(value => map(value.asInstanceOf[ExprValue]))
    }
    (values, None)
  }
}


class DseConfigOrdinalAxis(config: DseConfigElement) extends PlotAxis {
  override def toString = config.configToString

  override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], PlotAxis.AxisType) = {
    val values = results.map { result =>
      result.config.get(config).map(config.valueToString)
    }

    val distinctValues = values.flatten.distinct
    val presort = (config match {  // sorted by input space
      case config: DseRefinementElement[Any] => config.getValues.map { case (value, refinements) =>
        config.valueToString(value)
      }
      case _ => Seq()
    }).filter(distinctValues.contains(_))
    val postsort = distinctValues.filter(!presort.contains(_)).sorted  // anything else in actual results

    val stringToPos = (presort ++ postsort).zipWithIndex.map { case (str, index) => (str, index.toFloat) }
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
