package edg_ide.dse

import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, CompilerError, ExprValue}
import edg.wir.Refinements
import edgir.schema.schema.Design
import edgir.ref.ref

import scala.collection.SeqMap


// result entry for an evaluated point in the design space
case class DseResult(
                        config: SeqMap[DseConfigElement, Any],
                        configRefinement: Refinements,
                        compiler: Compiler,
                        compiled: Design,
                        errors: Seq[CompilerError],
                        objectives: SeqMap[String, Any],
                        compileTime: Long
) {
  def objectiveToString: String = {
    objectives.map { case (name, value) =>
      f"$name -> ${DseConfigElement.valueToString(value)}"
    }.mkString(", ")
  }
}
