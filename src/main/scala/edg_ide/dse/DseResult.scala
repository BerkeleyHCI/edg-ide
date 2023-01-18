package edg_ide.dse

import edg.compiler.{Compiler, CompilerError}
import edg.wir.Refinements
import edgir.schema.schema.Design

import scala.collection.SeqMap


// result entry for an evaluated point in the design space
case class DseResult(
                        index: Int,
                        config: SeqMap[DseConfigElement, Any],
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
