package edg_ide.dse

import edg.compiler.ExprValue
import edg.wir.{DesignPath, Refinements}


// Abstract base class for a design space search configuration element - some independent
// parameter to scan through, eg "all refinements of superclass" or "try these parameter values"
sealed trait DseConfigElement {
}

// DSE element that generates into a set of refinements with no dynamic dependencies
sealed trait DseRefinementElement extends DseConfigElement {
  def getRefinements: Seq[Refinements]
}

// Tries all values for some parameter
case class DseParameterSearch(path: DesignPath, values: Seq[ExprValue]) extends DseRefinementElement {
  override def getRefinements: Seq[Refinements] = values.map { value =>
    Refinements(instanceValues=Map(path -> value))
  }
}
