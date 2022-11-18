package edg_ide.dse

import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{ExprValue, FloatValue, PartialCompile}
import edg.util.Errorable
import edgir.ref.ref
import edg.wir.{DesignPath, Refinements}
import edg_ide.util.ExceptionNotifyImplicits.ExceptOption
import edg_ide.util.IterableExtensions.IterableExtension
import edg_ide.util.{exceptable, requireExcept}

import scala.collection.SeqMap


object DseConfigElement {
  def valueToString(value: Any): String = value match {
    case value: ref.LibraryPath => value.toSimpleString
    case value: ExprValue => value.toStringValue
    case Some(value) => valueToString(value) // drop the "Some" for simplicity
    case value => value.toString
  }

  def configMapToString(configMap: SeqMap[DseConfigElement, Any]): String = {
    configMap.map { case (config, value) =>
      f"${config.configToString} -> ${valueToString(value)}"
    }.mkString(", ")
  }
}


// Abstract base class for a design space search configuration element - some independent
// parameter to scan through, eg "all refinements of superclass" or "try these parameter values
//
// Must be serializable so configs can be saved and persist across IDE restarts
sealed trait DseConfigElement { self: Serializable =>
  def getPartialCompile: PartialCompile
  def configToString: String
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
  override def configToString: String = f"Param($path)"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(params=Seq(path))
  }

  override def getValues: Seq[(ExprValue, Refinements)] = values.map { value =>
    (value, Refinements(instanceValues=Map(path -> value)))
  }

  // Returns the values as a string, that will parse back with valuesStringToConfig.
  def valuesToString(): String = {
    values.map(_.toStringValue).mkString(", ")
  }

  // Parses a string specification of values into a new DseParameterSearch (of the same path and type).
  // The existing object is required to determine the path and value type.
  // May fail with an error message that can be propagated back to the user.
  // TODO handle quoting (strings) and range parentheses
  def valuesStringToConfig(str: String): Errorable[DseParameterSearch] = exceptable {
    val valueClass = values.map(_.getClass).allSameValue.exceptNone("internal error, inconsistent values")
    val splitString = str.split(',').map(_.strip())
    requireExcept(splitString.nonEmpty, "no values specified")
    val newValues = (valueClass match {
      case v if v == classOf[FloatValue] =>
        Some(splitString.zipWithIndex.map { case (str, index) =>
          FloatValue(str.toFloatOption
              .exceptNone(f"invalid value ${index + 1} '$str': not a float"))
        })
      case _ =>
        None
    }).exceptNone("")
    DseParameterSearch(path, newValues)
  }
}


// Tries all subclasses for some block
case class DseSubclassSearch(path: DesignPath, subclasses: Seq[ref.LibraryPath])
    extends DseInstanceRefinementElement[ref.LibraryPath] with Serializable {
  override def toString = f"${this.getClass.getSimpleName}($path, ${subclasses.map(_.toSimpleString).mkString(", ")})"
  override def configToString: String = f"Subclass($path)"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(blocks = Seq(path))
  }

  override def getValues: Seq[(ref.LibraryPath, Refinements)] = subclasses.map { value =>
    (value, Refinements(instanceRefinements=Map(path -> value)))
  }
}
