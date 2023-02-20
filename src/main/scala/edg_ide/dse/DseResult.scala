package edg_ide.dse

import edg.compiler.{Compiler, CompilerError}
import edgir.schema.schema.Design

import scala.collection.{SeqMap, mutable}


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


// DSE result set that combines similar entries while preserving order
class CombinedDseResultSet(results: Seq[DseResult]) {
  // groups similar results while preserving the order of groups (based on first-seen order of results)
  private def combineSimilarResults(results: Seq[DseResult]): Seq[Seq[DseResult]] = {
    type GroupingKey = (Boolean, SeqMap[String, Any])  // group only by objective values and if there were errors
    val groupedMap = mutable.SeqMap[GroupingKey, mutable.ArrayBuffer[DseResult]]()
    results.foreach { result =>
      val key = (result.errors.nonEmpty, result.objectives)
      groupedMap.getOrElseUpdate(key, mutable.ArrayBuffer[DseResult]()).append(result)
    }
    groupedMap.map { case (key, vals) =>
      vals.toSeq
    }.toSeq
  }

  val groupedResults: Seq[Seq[DseResult]] = combineSimilarResults(results)
}
