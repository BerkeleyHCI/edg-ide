package edg_ide.dse

import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{BooleanValue, ExprValue, FloatValue, IntValue, PartialCompile, RangeValue, TextValue}
import edg.util.Errorable
import edgir.ref.ref
import edg.wir.{DesignPath, Refinements}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptOption, ExceptSeq}
import edg_ide.util.IterableExtensions.IterableExtension
import edg_ide.util.{ExceptionNotifyException, exceptable, requireExcept}

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


object DseParameterSearch {
  // parsing regex from https://stackoverflow.com/a/18893443/5875811, which includes a breakdown explanation!
  protected lazy val rangeSplitRegex = ",(?=(?:[^(]*\\([^)]*\\))*[^()]*$)".r
  protected lazy val rangeParseRegex = raw"^\s*\(\s*([\d.])+\s*,\s*([\d.])+\s*\)\s*$$".r
}


// Tries all values for some parameter
case class DseParameterSearch(path: DesignPath, values: Seq[ExprValue])
    extends DseInstanceRefinementElement[ExprValue] with Serializable {
  override def toString = f"${this.getClass.getSimpleName}($path, ${values.map(_.toStringValue).mkString(",")})"
  override def configToString: String = f"Param($path)"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(params=Seq(path))
  }

  override def getValues: Seq[(ExprValue, Refinements)] = values.map { value =>
    (value, Refinements(instanceValues=Map(path -> value)))
  }

  // Returns the values as a string, that will parse back with valuesStringToConfig.
  def valuesToString(): String = {
    values.map(_.toStringValue).mkString(",")
  }

  // Parses a string specification of values into a new DseParameterSearch (of the same path and type).
  // The existing object is required to determine the path and value type.
  // May fail with an error message that can be propagated back to the user.
  // TODO handle quoting (strings) and range parentheses
  def valuesStringToConfig(str: String): Errorable[DseParameterSearch] = exceptable {
    val valueClass = values.map(_.getClass).allSameValue.exceptNone("internal error, inconsistent values")

    val newValues = valueClass match {
      case v if v == classOf[BooleanValue] =>
        str.split(',').zipWithIndex.map { case (str, index) =>
          BooleanValue(str.strip().toBooleanOption.exceptNone(f"invalid value ${index + 1} '$str': not an bool"))
        }
      case v if v == classOf[IntValue] =>
        str.split(',').zipWithIndex.map { case (str, index) =>
          IntValue(str.strip().toIntOption.exceptNone(f"invalid value ${index + 1} '$str': not an int"))
        }
      case v if v == classOf[FloatValue] =>
        str.split(',').zipWithIndex.map { case (str, index) =>
          FloatValue(str.strip().toFloatOption.exceptNone(f"invalid value ${index + 1} '$str': not a float"))
        }
      case v if v == classOf[TextValue] =>
        str.split(',').map {  // TODO support escaping spaces - the above regex doesn't delete the quotes
          TextValue
        }
      case v if v == classOf[RangeValue] =>
        DseParameterSearch.rangeSplitRegex.split(str).zipWithIndex.map { case (str, index) =>
          val patMatch = DseParameterSearch.rangeParseRegex.findAllMatchIn(str).toSeq
              .onlyExcept(f"invalid value ${index + 1} '$str': not a range")
          requireExcept(patMatch.groupCount == 2, f"invalid value ${index + 1} '$str': not a range")
          val min = patMatch.group(0).toFloatOption.exceptNone(f"invalid value ${index + 1} '$str': not a range")
          val max = patMatch.group(1).toFloatOption.exceptNone(f"invalid value ${index + 1} '$str': not a range")
          RangeValue(min, max)
        }
      case v =>
        throw ExceptionNotifyException(f"unknown type of value $v")
    }
    requireExcept(newValues.nonEmpty, "no values specified")
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
