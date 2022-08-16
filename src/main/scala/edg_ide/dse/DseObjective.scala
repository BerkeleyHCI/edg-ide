package edg_ide.dse
import edgir.schema.schema
import edgir.schema.schema.Design


// Abstract base class for all design space objectives - some function of the design
// that produces a metric that is useful to the user, eg cost or some design parameter
sealed trait DseObjective {
  // TODO: also needs libraries and external sources?
  def calculate(design: schema.Design): Float
}

case class DseObjectiveParameter() extends DseObjective {
  override def calculate(design: Design): Float = ???
}

case class DseObjectiveSummation() extends DseObjective {
  override def calculate(design: Design): Float = ???
}

case class DseObjectiveFootprintArea() extends DseObjective {
  override def calculate(design: Design): Float = ???
}
