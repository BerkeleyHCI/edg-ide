package edg_ide.dse

import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, CompilerError, ExprValue}
import edg.wir.Refinements
import edgir.schema.schema.Design
import edgir.ref.ref

import scala.collection.SeqMap


// result entry for an evaluated point in the design space
case class DseResult(
  values: SeqMap[DseConfigElement, Any],
  refinement: Refinements,
  compiler: Compiler,
  compiled: Design,
  errors: Seq[CompilerError],
  objectives: SeqMap[String, Any],
  compileTime: Long
) {
  def valueToString(value: Any): String = value match {
    case value: ref.LibraryPath => value.toSimpleString
    case value: ExprValue => value.toStringValue
    case Some(value) => valueToString(value)  // drop the "Some" for simplicity
    case value => value.toString
  }

  def configToString: String = {
    values.map { case (config, value) =>
      val configStr = config match {
        case config: DseInstanceRefinementElement[Any] => config.path
        case config => config.toString
      }
      f"$configStr -> ${valueToString(value)}"
    }.mkString(", ")
  }

  def objectiveToString: String = {
    objectives.map { case (name, value) =>
      f"$name -> ${valueToString(value)}"
    }.mkString(", ")
  }
}
