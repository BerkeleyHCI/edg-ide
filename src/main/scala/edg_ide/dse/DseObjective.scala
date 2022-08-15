package edg_ide.dse
import edgir.schema.schema


// Abstract base class for all design space objectives - some function of the design
// that produces a metric that is useful to the user, eg cost or some design parameter
sealed trait DseObjective {
  // TODO: also needs libraries and external sources?
  def calculate(design: schema.Design): Float
}

case class DseObjectiveParameter() extends DseObjective {

}

case class DseObjectiveSummation() extends DseObjective {

}

case class DseObjectiveFootprintArea() extends DseObjective {

}
