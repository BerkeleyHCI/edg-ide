package edg_ide.dse

import edg.compiler.ExprValue
import edg.wir.DesignPath


// Abstract base class for a design space search configuration element - some independent
// parameter to scan through, eg "all refinements of superclass" or "try these parameter values"
sealed trait DseConfigElement

// Tries all values for some parameter
case class DseParameterSearch(path: DesignPath, values: Seq[ExprValue])
