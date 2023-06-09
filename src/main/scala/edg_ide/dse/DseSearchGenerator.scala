package edg_ide.dse

import edg.compiler.{Compiler, PartialCompile}
import edg.wir.Refinements

import scala.collection.{SeqMap, mutable}

/** This class generates the search space for design space exploration. This supports dynamic / derived design
  * spaces, where additional points in the design space are revealed based on the value of evaluated points.
  *
  * This is its own class so the design space behavior is unit-testable.
  */
class DseSearchGenerator(configs: Seq[DseConfigElement]) {
  val (staticConfigs, derivedConfigs) = configs.partitionMap {
    case config: DseRefinementElement[Any] => Left(config)
    case config: DseDerivedConfig          => Right(config)
  }
  private val allConfigs = staticConfigs ++ derivedConfigs

  private val staticSpace = staticConfigs.to(IndexedSeq).map(_.getValues)

  // stack of partial compiles up to the next point under evaluation
  // the first elements (up to staticConfigs.length) correspond to the static config,
  // the ones after (up to derivedConfigs.length) correspond to those.
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
  private var searchStack: Option[mutable.ListBuffer[mutable.ListBuffer[(Any, Refinements)]]] =
    Some(mutable.ListBuffer())
  // the total derived space for the current staticConfig part of searchStack
  // if searchStack does not have a fully defined staticConfig, this must be None
  private var derivedSpace: Option[IndexedSeq[Seq[(Any, Refinements)]]] = None

  // partial compiles, element correlates to the maximal partial compilation that the corresponding config builds
  // on top of, so the first element would be holding back everything
  private val compilerStack = mutable.ListBuffer[Compiler]()

  // returns the points remaining in the search and the total points, given the current stack and space
  // (where space is defined as points for each variable)
  private def getRemainingSearchSpace(stackCount: Seq[Int], space: Seq[Int]): (Int, Int) = {
    // the search space for each element of each level, assuming we increment the last element
    // index 1 corresponds to the search space for the first element, and index 0 is the total search space size
    val cumulativeSpaceSize = space.reverse.scan(1)(_ * _).reverse

    // the implicit count is the number of elements from the implicit tail past searchStack
    val implicitStaticCount = if (stackCount.size >= space.length) 1 else cumulativeSpaceSize(stackCount.size)
    // this pushes back the accounting for the last element of each config (the one currently under evaluation)
    // to the next config, and eventually onto the 1 in implicitStaticCount
    val remainingStaticCount = stackCount
      .zip(cumulativeSpaceSize.drop(1))
      .map { case (remainingCount, searchSpace) =>
        (remainingCount - 1) * searchSpace
      }
      .sum + implicitStaticCount

    (remainingStaticCount, cumulativeSpaceSize.head)
  }

  // Returns the next point to search in the design space. Returns new points as the prior one is evaluated.
  // Points are returned as: prior compiler (empty on first one), partial compilation rule, search value map,
  //   search refinements, incremental refinements (over prior compiler), fraction complete
  // If a design point has an empty PartialCompile, it can be used in the output.
  // This only changes after addEvaluatedPoint is called, when the point is marked as evaluated
  // and derived points are added.
  def nextPoint(): Option[
    (Option[Compiler], PartialCompile, SeqMap[DseConfigElement, Any], Refinements, Refinements, Float)
  ] = {
    // initial point: add partial compile root, with all config holdbacks
    // for each static config: do a partial compile while holding back the rest
    // when all static configs have an assignment:
    //   if no derived configs: the last one has no partial configs and is a design point
    //   if derived configs: derived configs are still held back
    //     do an additional generating compile with derived configs no held back
    //     feed that back into the design space for derived configs, and repeat stack behavior with derived configs
    searchStack.map { searchStack =>
      // check invariants
      if (searchStack.length < staticConfigs.length) {
        require(derivedSpace.isEmpty)
      }
      require(searchStack.length == compilerStack.length)

      val staticPartialCompile = staticConfigs
        .drop(searchStack.length)
        .map(_.getPartialCompile)
        .fold(PartialCompile())(_ ++ _)
      val derivedPartialCompile =
        if (derivedSpace.isEmpty && searchStack.length == staticConfigs.length) { // do generating compile
          PartialCompile()
        } else { // for all other cases, it's a normal part of backtracking search
          derivedConfigs
            .drop(math.max(0, searchStack.length - staticConfigs.length))
            .map(_.getPartialCompile)
            .fold(PartialCompile())(_ ++ _)
        }
      val baseCompiler = compilerStack.lastOption // initial is None
      val searchValues = allConfigs.zip(searchStack).map { case (staticConfig, staticValues) =>
        val (thisValue, thisRefinement) = staticValues.head
        staticConfig -> thisValue
      }
      val combinedSearchValueMap = searchValues.to(SeqMap)
      val searchRefinements = searchStack.map(_.head._2).foldLeft(Refinements())(_ ++ _)
      val incrRefinement = searchStack.lastOption.map(_.head._2).getOrElse(Refinements())

      val (staticSpaceRemain, staticSpaceTotal) = getRemainingSearchSpace(
        searchStack.map(_.length).toSeq,
        staticSpace.map(_.length)
      )

      val derivedCompletedFraction =
        derivedSpace match { // this counts against the last element of staticSpaceRemain
          case Some(derivedSpace) =>
            val (derivedSpaceRemain, derivedSpaceTotal) = getRemainingSearchSpace(
              searchStack.drop(staticSpace.length).map(_.length).toSeq,
              derivedSpace.map(_.length)
            )
            (derivedSpaceTotal - derivedSpaceRemain.toFloat) / derivedSpaceTotal
          case None => 0
        }
      val completedFraction =
        (staticSpaceTotal - (staticSpaceRemain.toFloat - derivedCompletedFraction)) / staticSpaceTotal

      (
        baseCompiler,
        staticPartialCompile ++ derivedPartialCompile,
        combinedSearchValueMap,
        searchRefinements,
        incrRefinement,
        completedFraction
      )
    }
  }

  // Call with the result of getNextPoint() to mark that point as searched and update the next nextPoint
  def addEvaluatedPoint(compiler: Compiler): Unit = {
    require(searchStack.nonEmpty)
    searchStack.foreach { searchStack =>
      if (derivedConfigs.nonEmpty && derivedSpace.isEmpty && searchStack.length == staticConfigs.length) { // is generating compile
        derivedSpace = Some(derivedConfigs.to(IndexedSeq).map { derivedConfig =>
          derivedConfig.configFromDesign(compiler).get.getValues
        })
      } else if (searchStack.size < allConfigs.length) { // just evaluated an intermediate point, add down the stack
        if (searchStack.size < staticConfigs.length) { // static config case
          searchStack.append(staticSpace(searchStack.length).to(mutable.ListBuffer))
        } else { // dynamic config case
          searchStack.append(
            derivedSpace.get(searchStack.length - staticConfigs.length).to(mutable.ListBuffer)
          )
        }
        compilerStack.append(compiler)
      } else { // just evaluated a concrete design point, pop up the stack
        if (searchStack.nonEmpty) { // searchStack may be empty for an empty design space
          searchStack.last.remove(0) // remove the first (just evaluated) point
        }
        while (searchStack.nonEmpty && searchStack.last.isEmpty) { // backtrack as needed
          compilerStack.remove(searchStack.length - 1)
          searchStack.remove(searchStack.length - 1)
          if (searchStack.length == staticConfigs.length) { // if backtracking past a derived space, clear the prior
            require(derivedSpace.nonEmpty)
            derivedSpace = None
          }
          if (searchStack.nonEmpty) { // make sure we don't go past the beginning of the stack
            searchStack.last.remove(0) // remove the first (just evaluated) point
          }
        }
      }

      if (searchStack.isEmpty && derivedSpace.isEmpty) {
        this.searchStack = None
      }
    }
  }
}
