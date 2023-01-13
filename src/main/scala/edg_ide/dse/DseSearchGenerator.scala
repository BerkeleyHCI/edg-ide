package edg_ide.dse

import edgir.schema.schema
import edg.compiler.{Compiler, PartialCompile}
import edg.wir.Refinements

import scala.collection.{SeqMap, mutable}


/** This class generates the search space for design space exploration.
  * This supports dynamic / derived design spaces, where additional points in the design space are revealed
  * based on the value of evaluated points.
  *
  * This is its own class so the design space behavior is unit-testable.
  */
class DseSearchGenerator(configs: Seq[DseConfigElement]) {
  val (staticConfigs, derivedConfigs) = configs.partitionMap {
    case config: DseRefinementElement[Any] => Left(config)
    case config: DseDerivedConfig => Right(config)
  }

  // stack of partial compiles up to the next point under evaluation
  // staticStack elements correspond in ordering to staticConfigs, ditto for dynamicStack
  //
  // the first element of each element is the next point to be evaluated
  // if the stack is incomplete (stack.length != config.length), it means a partial compilation is requested
  // for example, for a config of [[1, 2], [10, 20]]
  // a stack of [[2], [10, 20]], points (1, 10), (1, 20) have been evaluated and (2, 10) is next
  // a stack of [] is the initial state, no points have been searched
  // a stack of [[1, 2]] means the initial partial compile is done, and a partial compile of 1 for the first element
  //   and holding back the second element is requested
  // a stack of None means all points have been searched
  // inner list values must not be empty
  private val staticStack: Option[mutable.ListBuffer[mutable.ListBuffer[(Any, Refinements)]]] = Some(mutable.ListBuffer())
  private val derivedStack: Option[mutable.ListBuffer[mutable.ListBuffer[(Any, Refinements)]]] = Some(mutable.ListBuffer())

  // partial compiles, element correlates to the maximal partial compilation that the corresponding config builds
  // on top of, so the first element would be holding back everything
  private val staticCompilerStack = mutable.ListBuffer[Compiler]()
  private val derivedCompilerStack = mutable.ListBuffer[Compiler]()

  // Returns the next point to search in the design space. Returns new points and the prior one is evaluated.
  // If a design point has an empty PartialCompile, it can be used in the output.
  // This only changes after addEvaluatedPoint is called, when the point is marked as evaluated
  // and derived points are added.
  def nextPoint(): Option[(Option[Compiler], PartialCompile, SeqMap[DseConfigElement, Any], Refinements)] = {
    require(derivedConfigs.isEmpty && derivedStack.isEmpty || derivedStack.get.isEmpty && derivedCompilerStack.isEmpty)
    // initial point: add partial compile root, with all config holdbacks
    // for each static config: do a partial compile while holding back the rest
    // when all static configs have an assignment:
    //   if no derived configs: the last one has no partial configs and is a design point
    //   if derived configs: derived configs are still held back
    //     do an additional generating compile with derived configs no held back
    //     feed that back into the design space for derived configs, and repeat stack behavior with derived configs
    staticStack.map { staticStack =>
      require(staticStack.length == staticCompilerStack.length)
      val partialCompileRule = staticConfigs.drop(staticStack.length).map(_.getPartialCompile).reduce(_ ++ _)
      val baseCompiler = staticCompilerStack.lastOption  // initial is None
      val (searchValues, refinements) = (staticConfigs zip staticStack).map { case (staticConfig, staticValues) =>
        val (thisValue, thisRefinement) = staticValues.head
        (staticConfig.asInstanceOf[DseConfigElement] -> thisValue, thisRefinement)
      }.unzip
      val combinedSearchValueMap = searchValues.to(SeqMap)
      val combinedRefinement = refinements.fold(Refinements())(_ ++ _)
      (baseCompiler, partialCompileRule, combinedSearchValueMap, combinedRefinement)
    }
  }

  // Call with the result of getNextPoint() to mark that point as searched and update the next nextPoint
  def addEvaluatedPoint(compiler: Compiler): Unit = {
    require(staticStack.nonEmpty)
    staticStack.foreach { staticStack =>
      if (staticStack.size != staticConfigs.length) {  // just evaluated an intermediate point, add down the stack
        staticStack.append(staticConfigs(staticStack.length).getValues.to(mutable.ListBuffer))
        staticCompilerStack.append(compiler)
      } else {  // just evaluated a concrete design point, pop up the stack

      }
    }

  }
}
