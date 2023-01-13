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
  // a stack of None is the initial state, no points have been searched
  // a stack of [[1, 2]] means the initial partial compile is done, and a partial compile of 1 for the first element
  //   and holding back the second element is requested
  // a stack of [] means all points have been searched
  private val staticStack = ...
  private val dynamicStack = ...

  // partial compiles, element correlates to the maximal partial compilation that the corresponding config builds
  // on top of, so the first element would be holding back everything
  private val staticCompilerStack = mutable.ListBuffer[Compiler]()
  private val dynamicCompilerStack = mutable.ListBuffer[Compiler]()

  // Returns the currently known design space. May return new values as points are evaluated.
  // If a design point has an empty PartialCompile, it can be used in the output.
  // This only changes after addEvaluatedPoint is called, when the point is marked as evaluated
  // and derived points are added.
  def getNextPoint(): (PartialCompile, SeqMap[DseConfigElement, Any], Refinements) = {
    // initial point: add partial compile root
    // in derived case: add derived config holdbacks to all partial compile
    // for all static configs:
    //   for each one, do a partial compile while holding back the rest
    //
    // subsequent, in non-derived case:
    //   for each derived point add a partial compile
    // subsequent, in derived case:
    //
  }

  // Call with the result of getNextPoint() to mark that point as searched, and either
  def addEvaluatedPoint(compiler: Compiler): Unit = {

  }
}
