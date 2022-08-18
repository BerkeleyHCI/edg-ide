package edg_ide.dse
import edg.compiler.{ExprValue, FloatValue, IntValue, RangeValue}
import edg.wir.IndirectDesignPath
import edgir.schema.schema
import edgir.schema.schema.Design


// Abstract base class for all design space objectives - some function of the design
// that produces a metric that is useful to the user, eg cost or some design parameter
sealed trait DseObjective {
  // TODO: also needs libraries and external sources?
  def calculate(design: schema.Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float]
}

case class DseObjectiveParameter(path: IndirectDesignPath) extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = {
    values.get(path) match {
      case Some(FloatValue(value)) => Some(value)
      case Some(IntValue(value)) => Some(value.toFloat)
      case Some(RangeValue(lower, upper)) => Some((lower + upper) / 2)  // TODO support more data types
      case _ => None
    }
  }
}

case class DseObjectiveSummation() extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = ???
}

case class DseObjectiveFootprintArea() extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = ???
}

case class DseObjectiveFootprintCount() extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = ???
}

case class DseObjectiveCost() extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = ???
}
