package edg_ide.dse

import edg.compiler.{Compiler, CompilerError}
import edg.wir.Refinements
import edg_ide.swing.dse.DseResultModel
import edgir.schema.schema.Design

import scala.collection.{SeqMap, mutable}

// result entry for an evaluated point in the design space
case class DseResult(
    index: Int,
    config: SeqMap[DseConfigElement, Any],
    searchRefinements: Refinements,
    compiler: Compiler,
    compiled: Design,
    errors: Seq[CompilerError],
    objectives: SeqMap[DseObjective, Any],
    compileTime: Long
) {
  def objectiveToString: String = {
    objectives
      .map { case (objective, value) =>
        f"${objective.objectiveToString} -> ${DseConfigElement.valueToString(value)}"
      }
      .mkString(", ")
  }
}

// DSE result set that combines similar entries while preserving order
class CombinedDseResultSet(results: Seq[DseResult]) {
  // groups similar results while preserving the order of groups (based on first-seen order of results)
  private def combineSimilarResults(results: Seq[DseResult]): Seq[Seq[DseResult]] = {
    // group only by ideal errors only, non-ideal errors, objective values
    type GroupingKey = (Boolean, Boolean, SeqMap[DseObjective, Any])
    val groupedMap = mutable.SeqMap[GroupingKey, mutable.ArrayBuffer[DseResult]]()
    results.foreach { result =>
      val (idealErrors, otherErrors) = DseResultModel.partitionByIdeal(result.errors)
      val key = (idealErrors.nonEmpty && otherErrors.isEmpty, otherErrors.nonEmpty, result.objectives)
      groupedMap.getOrElseUpdate(key, mutable.ArrayBuffer[DseResult]()).append(result)
    }
    groupedMap.map { case (key, vals) =>
      vals.toSeq
    }.toSeq
  }

  val groupedResults: Seq[Seq[DseResult]] = combineSimilarResults(results)
}
