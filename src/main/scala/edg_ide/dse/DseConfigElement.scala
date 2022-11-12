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
sealed trait DseRefinementElement[+ValueType] extends DseConfigElement { self: Serializable =>
  // Returns a list of possibilities, as both a raw value and a refinement
  def getValues: Seq[(ValueType, Refinements)]
}


// DSE element that is associated with a single path
sealed trait DseInstanceRefinementElement[+ValueType] extends DseRefinementElement[ValueType] { self: Serializable =>
  val path: DesignPath
}


// Tries all values for some parameter
case class DseParameterSearch(path: DesignPath, values: Seq[ExprValue])
    extends DseInstanceRefinementElement[ExprValue] with Serializable {
  override def toString = f"${this.getClass.getSimpleName}($path, ${values.map(_.toStringValue).mkString(", ")})"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(params=Seq(path))
  }

  override def getValues: Seq[(ExprValue, Refinements)] = values.map { value =>
    (value, Refinements(instanceValues=Map(path -> value)))
  }
}


// Tries all subclasses for some block
case class DseSubclassSearch(path: DesignPath, subclasses: Seq[ref.LibraryPath])
    extends DseInstanceRefinementElement[ref.LibraryPath] with Serializable {
  override def toString = f"${this.getClass.getSimpleName}($path, ${subclasses.map(_.toSimpleString).mkString(", ")})"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(blocks = Seq(path))
  }

  override def getValues: Seq[(ref.LibraryPath, Refinements)] = subclasses.map { value =>
    (value, Refinements(instanceRefinements=Map(path -> value)))
  }
}
