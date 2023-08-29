package edg_ide.util

import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiManager}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.SlowOperations
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi._
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edgir.ref.ref
import edgir.schema.schema
import edg.wir.DesignPath
import edg.util.Errorable
import edg.ElemBuilder.LibraryPath
import edg_ide.EdgirUtils
import edg_ide.psi_edits.InsertConnectAction
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}

import scala.jdk.CollectionConverters.{CollectionHasAsScala, ListHasAsScala}
import scala.collection.mutable

object DesignAnalysisUtils {
  // Returns whether subclass is a subclass of superclass, using the PSI.
  // If anything can't be found, returns false.
  def isSubclassOfPsi(subclass: ref.LibraryPath, superclass: ref.LibraryPath, project: Project): Boolean = {
    (pyClassOf(subclass, project).toOption, pyClassOf(superclass, project).toOption) match {
      case (Some(subclass), Some(superclass)) =>
        subclass.isSubclass(superclass, TypeEvalContext.codeAnalysis(project, null))
      case _ => false
    }
  }

  /** Returns the PyClass of a LibraryPath
    */
  def pyClassOf(path: ref.LibraryPath, project: Project): Errorable[PyClass] = {
    pyClassOf(path.getTarget.getName, project)
  }

  def pyClassOf(className: String, project: Project): Errorable[PyClass] = {
    val pyPsi = PyPsiFacade.getInstance(project)
    val anchor = PsiManager.getInstance(project).findFile(project.getProjectFile)
    SlowOperations.allowSlowOperations(() => {
      // this is often used to build responsive UI elements, so hopefully is also fast enough to run in EDT
      Errorable(pyPsi.createClassByQName(className, anchor), s"no class $className")
    })
  }

  def typeOf(pyClass: PyClass): ref.LibraryPath = {
    LibraryPath(pyClass.getQualifiedName)
  }

  def isPyClassAbstract(pyClass: PyClass): Boolean = {
    val hasAbstractDecorator = Option(pyClass.getDecoratorList).map { decoratorList =>
      decoratorList.getDecorators.exists(_.getName == "abstract_block") ||
      decoratorList.getDecorators.exists(_.getName == "abstract_block_default")
    }
    hasAbstractDecorator.getOrElse(false)
  }

  // For a statement list, eg in a function, returns the super().__init__ call if it exists and is the first statement
  def firstInitOption(stmts: Seq[PyStatement]): Option[PyCallExpression] = stmts match {
    case Seq() => None
    case Seq(headExpr: PyExpressionStatement, tail @ _*) =>
      headExpr.getExpression match {
        case pyCall: PyCallExpression if pyCall.getCallee.textMatches("super().__init__") => Some(pyCall)
        case _: PyStringLiteralExpression => firstInitOption(tail) // skip comments
        case expr => None // unknown, ignored
      }
    case Seq(head, tail @ _*) => None
  }

  /** For a PyClass, traverses down the init MRO chain, and returns all the arguments accepted by the init accounting
    * for **kwargs propagation.
    *
    * Returns (positional-capable args, keyword-only args)
    */
  def initParamsOf(
      pyClass: PyClass,
      project: Project
  ): Errorable[(Seq[PyNamedParameter], Seq[PyNamedParameter])] =
    exceptable {
      val argsList = mutable.ListBuffer[PyNamedParameter]() // parameters that can be positional args
      val kwargsList = mutable.ListBuffer[PyNamedParameter]() // parameters that can only be kwargs

      def processInitOfClass(
          remainingClasses: Seq[PyClass],
          ignoredNames: Set[String],
          argsDiscard: Int,
          argsAllowed: Boolean,
          kwargsAllowed: Boolean
      ): Unit = remainingClasses match {
        case Seq() => // reached the bottom of the call chain, nothing else to be done here
        case Seq(thisClass, tail @ _*) =>
          Option(thisClass.findInitOrNew(false, null)) match {
            case None => // no init function, traverse to next class in MRO
              processInitOfClass(tail, ignoredNames, argsDiscard, argsAllowed, kwargsAllowed)
            case Some(initFn) =>
              var canBePositional: Boolean = argsAllowed
              var argsDiscardRemain: Int = argsDiscard
              var argsName: Option[String] = None
              var kwargsName: Option[String] = None
              initFn.getParameterList.getParameters.foreach { param =>
                if (param.isSelf) {
                  // discard
                } else if (param.getText.startsWith("**")) {
                  requireExcept(kwargsName.isEmpty, s"duplicate kwargs in ${thisClass.getName}")
                  kwargsName = Some(param.getName)
                } else if (param.getText.startsWith("*")) {
                  requireExcept(argsName.isEmpty, s"duplicate args in ${thisClass.getName}")
                  argsName = Some(param.getName)
                } else if (param.isInstanceOf[PySingleStarParameter]) {
                  canBePositional = false
                } else { // normal argument
                  if (canBePositional) {
                    if (argsDiscardRemain == 0) {
                      argsList += param.instanceOfExcept[PyNamedParameter](
                        "" +
                          s"non-named parameter ${param.getName} in ${thisClass.getName}"
                      )
                    } else {
                      argsDiscardRemain -= 1
                    }
                  } else if (kwargsAllowed && !ignoredNames.contains(param.getName)) {
                    kwargsList += param.instanceOfExcept[PyNamedParameter](
                      "" +
                        s"non-named parameter ${param.getName} in ${thisClass.getName}"
                    )
                  } else {
                    // ignored - nothing can be passed into that param
                  }
                }
              }

              // recurse through superclasses in the init chain as needed
              firstInitOption(initFn.getStatementList.getStatements.toSeq) match {
                case Some(pyCall) =>
                  var positionalArgsLegal: Boolean = true
                  var numPositionalArgs: Int = 0
                  val keywordArgsUsed = mutable.Set[String]()
                  var takesArgs: Boolean = false
                  var takesKwargs: Boolean = false
                  pyCall.getArgumentList.getArguments.foreach {
                    case pyStar: PyStarArgument => // note is a type of PyExpression
                      if (argsName.isDefined && pyStar.textMatches(s"*${argsName.get}")) {
                        positionalArgsLegal = false
                        takesArgs = true
                      } else if (kwargsName.isDefined && pyStar.textMatches(s"**${kwargsName.get}")) {
                        takesKwargs = true
                      } else {
                        // ignored TODO should this error?
                      }
                    case pyKeyword: PyKeywordArgument => // note is a type of PyExpression
                      positionalArgsLegal = false
                      keywordArgsUsed += pyKeyword.getKeyword
                    case pyExpr: PyExpression =>
                      requireExcept(positionalArgsLegal, s"invalid superclass call in ${thisClass.getName}")
                      numPositionalArgs += 1 // assumes well-formatted expr
                    case _ =>
                  }
                  if (takesArgs || takesKwargs) { // if it doesn't take either, this don't do anything
                    processInitOfClass(tail, keywordArgsUsed.toSet, numPositionalArgs, takesArgs, takesKwargs)
                  }
                case None =>
              }
          }
      }

      val ancestors = pyClass.getAncestorClasses(TypeEvalContext.codeCompletion(project, null))
      processInitOfClass(pyClass +: ancestors.asScala.toSeq, Set(), 0, true, true)
      (argsList.toSeq, kwargsList.toSeq)
    }

  /** Returns whether an element is after another element accounting for EDG function call semantics. If within the same
    * function, does a simple after analysis without accounting for runtime behavior.
    *
    * TODO: could use better naming - more formally, the intent is beforeElement is visible immediately after
    * afterElement
    *
    * It is assumed both are in the same class.
    */
  def elementAfterEdg(
      beforeElement: PsiElement,
      afterElement: PsiElement,
      project: Project
  ): Option[Boolean] = {
    val beforeElementFunction = PsiTreeUtil.getParentOfType(beforeElement, classOf[PyFunction])
    val afterElementFunction = PsiTreeUtil.getParentOfType(afterElement, classOf[PyFunction])
    if (beforeElementFunction == null || afterElementFunction == null) { // this generally shouldn't happen
      return None
    }
    val beforeElementClass = PsiTreeUtil.getParentOfType(beforeElementFunction, classOf[PyClass])
    val afterElementClass = PsiTreeUtil.getParentOfType(afterElementFunction, classOf[PyClass])
    if (beforeElementClass == null || afterElementClass == null) { // this generally shouldn't happen
      return None
    }

    if (beforeElementClass != afterElementClass) { // compare class relationships
      Some(afterElementClass.isSubclass(beforeElementClass, TypeEvalContext.codeCompletion(project, null)))
    } else if (beforeElementFunction != afterElementFunction) { // compare function positions
      // TODO authoritative data structure for function name constants
      if (afterElementFunction.getName == "__init__") {
        Some(false) // note that the equal case is handled above
      } else if (afterElementFunction.getName == "contents") {
        Some(beforeElementFunction.getName == "__init__")
      } else {
        Some(true)
      }
    } else {
      // compare positions within a function
      // the length is used so after includes the contents of the entire sub-tree
      Some(
        beforeElement.getTextOffset + beforeElement.getTextLength <=
          afterElement.getTextOffset + afterElement.getTextLength
      )
    }
  }

  /** Returns all assigns to some path, by searching its parent classes for assign statements
    */
  def allAssignsTo(
      path: DesignPath,
      topDesign: schema.Design,
      project: Project
  ): Errorable[Seq[PyAssignmentStatement]] = exceptable {
    requireExcept(path.steps.nonEmpty, "node at top")
    val (parentPath, blockName) = path.split
    val parentBlock = EdgirUtils
      .resolveExactBlock(parentPath, topDesign)
      .exceptNone(s"no block at parent path $parentPath")
    val parentPyClass = pyClassOf(parentBlock.getSelfClass, project).exceptError
    val assigns = findAssignmentsTo(parentPyClass, blockName, project)
      .filter(_.canNavigateToSource)
      .exceptEmpty(s"no assigns to $blockName found in ${parentPyClass.getName}")
    assigns
  }

  /** Returns all connects to some path, by searching its parent class (for exports) and that parent's parent (for
    * connects). TODO clarify semantics around chain and implicits!
    */
  def allConnectsTo(
      path: DesignPath,
      topDesign: schema.Design,
      project: Project
  ): Errorable[Seq[PyExpression]] =
    exceptable {
      requireExcept(path.steps.nonEmpty, "path at top")
      val (parentPath, parentBlock) = EdgirUtils.resolveDeepestBlock(path, topDesign)
      requireExcept(path.steps.length > parentPath.steps.length, "path resolves to block")
      val portName = path.steps(parentPath.steps.length)

      val parentPyClass = pyClassOf(parentBlock.getSelfClass, project).exceptError
      val parentConnects = findGeneralConnectsTo(parentPyClass, ("", portName), project) match {
        case Errorable.Success(connects) => connects
        case _ => Seq()
      }

      val (containingPath, parentName) = parentPath.split
      val containingBlock = EdgirUtils
        .resolveExactBlock(containingPath, topDesign)
        .exceptNone(s"no block at containing path $containingPath")
      val containingPyClass = pyClassOf(containingBlock.getSelfClass, project).exceptError
      val containingConnects =
        findGeneralConnectsTo(containingPyClass, (parentName, portName), project) match {
          case Errorable.Success(connects) => connects
          case _ => Seq()
        }

      (containingConnects ++ parentConnects)
        .filter(_.canNavigateToSource)
        .exceptEmpty(s"no connects to $parentName.$portName")
    }

  /** Returns all connections involving a port, specified relative from the container as a pair. TODO: dedup w/
    * InsertConnectAction? But this is more general, and finds (some!) chains TODO needs to be aware of implicit port
    * semantics, including chain TODO needs to find exports
    */
  protected def findGeneralConnectsTo(
      container: PyClass,
      pair: (String, String),
      project: Project
  ): Errorable[Seq[PyExpression]] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    container.getMethods.toSeq
      .map { psiFunction =>
        exceptable {
          val selfName = psiFunction.getParameterList.getParameters.toSeq.headOption
            .exceptNone(s"function ${psiFunction.getName} has no self")
            .getName
          val connectReference =
            psiElementGenerator.createExpressionFromText(
              LanguageLevel.forElement(container),
              s"$selfName.connect"
            )
          val chainReference =
            psiElementGenerator.createExpressionFromText(
              LanguageLevel.forElement(container),
              s"$selfName.chain"
            )
          val portReference = psiElementGenerator.createExpressionFromText(
            LanguageLevel.forElement(container),
            InsertConnectAction.elementPairToText(selfName, pair)
          )

          // Traverse w/ recursive visitor to find all port references inside a self.connect
          val references = mutable.ListBuffer[PyExpression]()
          container.accept(new PyRecursiveElementVisitor() {
            override def visitPyCallExpression(node: PyCallExpression): Unit = {
              if (node.getCallee.textMatches(connectReference) || node.getCallee.textMatches(chainReference)) {
                // an optimization to not traverse down other functions
                super.visitPyCallExpression(node)
              }
            }
            override def visitPyReferenceExpression(node: PyReferenceExpression): Unit = {
              if (node.textMatches(portReference)) {
                references += node
              }
              // don't recurse any further, don't look at sub-references
            }
          })

          references.toSeq
            .flatMap { reference => // from reference to call expression
              Option(PsiTreeUtil.getParentOfType(reference, classOf[PyCallExpression]))
            }
            .collect {
              case call if call.getCallee.textMatches(connectReference) => call
              case call if call.getCallee.textMatches(chainReference) => call
            }
        }
      }
      .collect { case Errorable.Success(x) =>
        x
      }
      .flatten
      .distinct
      .exceptEmpty(s"class ${container.getName} contains no prior connects")
  }

  /** Returns all assignment statements targeting some targetName
    */
  def findAssignmentsTo(
      container: PyClass,
      targetName: String,
      project: Project
  ): Seq[PyAssignmentStatement] = {
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    val assigns = container.getMethods.toSeq.collect { method =>
      val parameters = method.getParameterList.getParameters
      if (parameters.nonEmpty) {
        val selfName = parameters(0).getName
        val targetReference =
          psiElementGenerator.createExpressionFromText(
            LanguageLevel.forElement(method),
            s"$selfName.$targetName"
          )

        // TODO support ElementDict and array ops
        // TODO search superclasses
        val methodAssigns = mutable.ListBuffer[PyAssignmentStatement]()
        method.accept(new PyRecursiveElementVisitor() {
          override def visitPyAssignmentStatement(node: PyAssignmentStatement): Unit = {
            if (node.getTargets.exists(expr => expr.textMatches(targetReference))) {
              methodAssigns += node
            }
          }
        })
        methodAssigns.toSeq
      } else {
        Seq()
      }
    }.flatten

    if (assigns.isEmpty) { // search up the superclass chain if needed
      container
        .getSuperClasses(TypeEvalContext.userInitiated(project, null))
        .toSeq
        .flatMap(findAssignmentsTo(_, targetName, project))
        .distinct // TODO also prevent duplicate work in case of multiple inheritance?
    } else {
      assigns
    }
  }

  /** Resolves a PyReferenceExpression to PyClasses. May return empty if the reference does not resolve to a class.
    */
  private def referenceToClass(ref: PyReferenceExpression): Seq[PyClass] = {
    ref.getReference
      .multiResolve(false)
      .toSeq
      .map(_.getElement)
      .collect { case expr: PyClass => expr }
  }

  /** Like PyClassInheritorsSearch.search, but returns subclasses depth-first order (roughly grouping similar results).
    * At each level, the default refinement (if provided) occurs first, with the rest sorted alphabetically by name If a
    * subclass occurs multiple times, only the first is kept. non_library elements are not includes, but their
    * subclasses are included.
    */
  def findOrderedSubclassesOf(superclass: PyClass): Seq[PyClass] = {
    val directSubclasses = PyClassInheritorsSearch
      .search(superclass, false)
      .findAll()
      .asScala
      .toSeq
      .sortBy(_.getName)
    val defaultRefinements = Option(superclass.getDecoratorList).toSeq
      .flatMap(_.getDecorators.toSeq)
      .filter(_.getName == "abstract_block_default")
      .map(_.getExpression)
      .collect { case expr: PyCallExpression => Option(expr.getArgument(0, classOf[PyLambdaExpression])) }
      .flatten
      .map(_.getBody)
      .flatMap { case expr: PyReferenceExpression => referenceToClass(expr) }

    val (defaults, others) = directSubclasses
      .flatMap { directSubclass =>
        val directIfLibrary =
          if (
            Option(directSubclass.getDecoratorList).toSeq
              .flatMap(_.getDecorators.toSeq)
              .exists(_.getName == "non_library")
          ) { // don't include non_library subclasses
            Seq()
          } else {
            Seq(directSubclass)
          }
        directIfLibrary ++ findOrderedSubclassesOf(directSubclass)
      }
      .distinct
      .partition(elt => defaultRefinements.contains(elt))
    defaults ++ others
  }
}
