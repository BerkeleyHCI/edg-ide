package edg_ide.dse

import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{ExprValue, PartialCompile}
import edgir.ref.ref
import edg.wir.{DesignPath, Refinements}


// Abstract base class for a design space search configuration element - some independent
// parameter to scan through, eg "all refinements of superclass" or "try these parameter values
//
// Must be serializable so configs can be saved and persist across IDE restarts
sealed trait DseConfigElement { self: Serializable =>
  def getPartialCompile: PartialCompile
}


// DSE element that generates into a set of refinements with no dynamic dependencies
sealed trait DseRefinementElement extends DseConfigElement { self: Serializable =>
  def getRefinements: Seq[Refinements]
}


// Tries all values for some parameter
case class DseParameterSearch(path: DesignPath, values: Seq[ExprValue])
    extends DseRefinementElement with Serializable {
  override def toString = f"${this.getClass.getSimpleName}($path, ${values.map(_.toStringValue).mkString(", ")})"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(params=Seq(path))
  }

  override def getRefinements: Seq[Refinements] = values.map { value =>
    Refinements(instanceValues=Map(path -> value))
  }
}


// Tries all subclasses for some block
case class DseSubclassSearch(path: DesignPath, subclasses: Seq[ref.LibraryPath])
    extends DseRefinementElement with Serializable {
  override def toString = f"${this.getClass.getSimpleName}($path, ${subclasses.map(_.toSimpleString).mkString(", ")})"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(blocks = Seq(path))
  }

  override def getRefinements: Seq[Refinements] = subclasses.map { value =>
    Refinements(instanceRefinements=Map(path -> value))
  }
}
