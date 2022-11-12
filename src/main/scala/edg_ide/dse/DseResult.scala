package edg_ide.dse

import edg.compiler.{Compiler, CompilerError}
import edg.wir.Refinements
import edgir.schema.schema.Design

import scala.collection.SeqMap


// result entry for an evaluated point in the design space
case class DseResult(
  refinement: Refinements,
  compiler: Compiler,
  compiled: Design,
  errors: Seq[CompilerError],
  objectives: SeqMap[String, Any],
  compileTime: Long
)
