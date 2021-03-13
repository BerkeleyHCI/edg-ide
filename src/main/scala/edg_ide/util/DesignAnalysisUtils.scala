package edg_ide.util

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi._
import edg.ref.ref
import edg.schema.schema
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.EdgirUtils
import edg_ide.actions.InsertConnectAction
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.mutable


object DesignAnalysisUtils {
  /** Returns the PyClass of a LibraryPath
    */
  def pyClassOf(path: ref.LibraryPath, project: Project): Errorable[PyClass] = {
    val pyPsi = PyPsiFacade.getInstance(project)
    Errorable(pyPsi.findClass(path.getTarget.getName), "no class")
  }

  /** For a PyClass, traverses down the init MRO chain, and returns all the arguments
    * accepted by the init accounting for **kwargs propagation.
    *
    * Returns (positional-capable args, keyword-only args)
    */
  def initParamsOf(pyClass: PyClass, project: Project):
      Errorable[(Seq[PyNamedParameter], Seq[PyNamedParameter])] = exceptable {
    val argsList = mutable.ListBuffer[PyNamedParameter]()  // parameters that can be positional args
    val kwargsList = mutable.ListBuffer[PyNamedParameter]()  // parameters that can only be kwargs

    def processInitOfClass(remainingClasses: Seq[PyClass], ignoredNames: Set[String],
                           argsDiscard: Int,
                           argsAllowed: Boolean, kwargsAllowed: Boolean): Unit = remainingClasses match {
      case Seq() =>  // reached the bottom of the call chain, nothing else to be done here
      case Seq(thisClass, tail @ _*) => Option(thisClass.findInitOrNew(false, null)) match {
        case None =>  // no init function, traverse to next class in MRO
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
            } else {  // normal argument
              if (canBePositional) {
                if (argsDiscardRemain == 0) {
                  argsList += param.instanceOfExcept[PyNamedParameter]("" +
                      s"non-named parameter ${param.getName} in ${thisClass.getName}")
                } else {
                  argsDiscardRemain -= 1
                }
              } else if (kwargsAllowed) {
                kwargsList += param.instanceOfExcept[PyNamedParameter]("" +
                    s"non-named parameter ${param.getName} in ${thisClass.getName}")
              } else {
                // ignored - nothing can be passed into that param
              }
            }
          }

          initFn.getStatementList.getStatements.headOption match {
            case Some(pyExpr: PyExpressionStatement) => pyExpr.getExpression match {
              case pyCall: PyCallExpression if pyCall.getCallee.textMatches("super().__init__") =>
                var positionalArgsLegal: Boolean = true
                var numPositionalArgs: Int = 0
                val keywordArgsUsed = mutable.Set[String]()
                var takesArgs: Boolean = false
                var takesKwargs: Boolean = false
                pyCall.getArgumentList.getArguments.foreach {
                  case pyStar: PyStarArgument =>  // note is a type of PyExpression
                    if (argsName.isDefined && pyStar.textMatches(s"*${argsName.get}")) {
                      positionalArgsLegal = false
                      takesArgs = true
                    } else if (kwargsName.isDefined && pyStar.textMatches(s"**${kwargsName.get}")) {
                      takesKwargs = true
                    } else {
                      // ignored TODO should this error?
                    }
                  case pyKeyword: PyKeywordArgument =>  // note is a type of PyExpression
                    positionalArgsLegal = false
                    keywordArgsUsed += pyKeyword.getKeyword
                  case pyExpr: PyExpression =>
                    requireExcept(positionalArgsLegal, s"invalid superclass call in ${thisClass.getName}")
                    numPositionalArgs += 1  // assumes well-formatted expr
                  case _ =>
                }
                if (takesArgs || takesKwargs) {  // if it doesn't take either, this don't do anything
                  processInitOfClass(tail, keywordArgsUsed.toSet, numPositionalArgs, takesArgs, takesKwargs)
                }
              case _ =>  // ignored
            }
            case _ =>  // ignored, nothing else to do if first isn't an init call
          }
      }
    }

    val ancestors = pyClass.getAncestorClasses(TypeEvalContext.codeCompletion(project, null))
    processInitOfClass(pyClass +: ancestors.toSeq, Set(), 0, true, true)
    (argsList.toSeq, kwargsList.toSeq)
  }

  /** Returns whether an element is after another element accounting for EDG function call semantics.
    * If within the same function, does a simple after analysis without accounting for runtime
    * behavior.
    *
    * TODO: could use better naming - more formally, the intent is beforeElement is visible
    * immediately after afterElement
    *
    * It is assumed both are in the same class.
    */
  def elementAfterEdg(beforeElement: PsiElement, afterElement: PsiElement, project: Project): Option[Boolean] = {
    val beforeElementFunction = PsiTreeUtil.getParentOfType(beforeElement, classOf[PyFunction])
    val afterElementFunction = PsiTreeUtil.getParentOfType(afterElement, classOf[PyFunction])
    if (beforeElementFunction == null || afterElementFunction == null) {  // this generally shouldn't happen
      return None
    }
    val beforeElementClass = PsiTreeUtil.getParentOfType(beforeElementFunction, classOf[PyClass])
    val afterElementClass = PsiTreeUtil.getParentOfType(afterElementFunction, classOf[PyClass])
    if (beforeElementClass == null || afterElementClass == null) {  // this generally shouldn't happen
      return None
    }

    if (beforeElementClass != afterElementClass) {  // compare class relationships
      Some(afterElementClass.isSubclass(beforeElementClass, TypeEvalContext.codeCompletion(project, null)))
    } else if (beforeElementFunction != afterElementFunction) {  // compare function positions
      // TODO authoritative data structure for function name constants
      if (afterElementFunction.getName == "__init__") {
        Some(false)  // note that the equal case is handled above
      } else if (afterElementFunction.getName == "contents") {
        Some(beforeElementFunction.getName == "__init__")
      } else {
        Some(true)
      }
    } else {
      // compare positions within a function
      // the length is used so after includes the contents of the entire sub-tree
      Some(beforeElement.getTextOffset + beforeElement.getTextLength <=
          afterElement.getTextOffset + afterElement.getTextLength)
    }
  }

  /** Returns all assigns to some path, by searching its parent classes for assign statements
    */
  def allAssignsTo(path: DesignPath, topDesign: schema.Design,
                   project: Project): Errorable[Seq[PyAssignmentStatement]] = exceptable {
    requireExcept(path.steps.nonEmpty, "node at top")
    val (parentPath, blockName) = path.split
    val parentBlock = EdgirUtils.resolveExactBlock(parentPath, topDesign)
        .exceptNone(s"no block at parent path $parentPath")
    requireExcept(parentBlock.superclasses.length == 1,
      s"invalid parent class ${EdgirUtils.SimpleSuperclass(parentBlock.superclasses)}")
    val parentPyClass = pyClassOf(parentBlock.superclasses.head, project).exceptError
    val assigns = findAssignmentsTo(parentPyClass, blockName, project).filter(_.canNavigateToSource)
        .exceptEmpty(s"no assigns to $blockName found in ${parentPyClass.getName}")
    assigns
  }

  /** Returns all connects to some path, by searching its parent class (for exports) and that parent's parent
    * (for connects).
    * TODO clarify semantics around chain and implicits!
    */
  def allConnectsTo(path: DesignPath, topDesign: schema.Design,
                   project: Project): Errorable[Seq[PyExpression]] = exceptable {
    requireExcept(path.steps.nonEmpty, "path at top")
    val (parentPath, parentBlock) = EdgirUtils.resolveDeepestBlock(path, topDesign)
    requireExcept(path.steps.length > parentPath.steps.length, "path resolves to block")
    val portName = path.steps(parentPath.steps.length)

    requireExcept(parentBlock.superclasses.length == 1,
      s"invalid parent class ${EdgirUtils.SimpleSuperclass(parentBlock.superclasses)}")
    val parentPyClass = pyClassOf(parentBlock.superclasses.head, project).exceptError
    val parentConnects = findGeneralConnectsTo(parentPyClass, ("", portName), project) match {
      case Errorable.Success(connects) => connects
      case _ => Seq()
    }

    val (containingPath, parentName) = parentPath.split
    val containingBlock = EdgirUtils.resolveExactBlock(containingPath, topDesign)
        .exceptNone(s"no block at containing path $containingPath")
    requireExcept(containingBlock.superclasses.length == 1,
      s"invalid containing class ${EdgirUtils.SimpleSuperclass(parentBlock.superclasses)}")
    val containingPyClass = pyClassOf(containingBlock.superclasses.head, project).exceptError
    val containingConnects = findGeneralConnectsTo(containingPyClass, ("", portName), project) match {
      case Errorable.Success(connects) => connects
      case _ => Seq()
    }

    (containingConnects ++ parentConnects)
        .filter(_.canNavigateToSource)
        .exceptEmpty(s"no connects to $parentName.$portName")
  }

  /** Returns all connections involving a port, specified relative from the container as a pair.
    * TODO: dedup w/ InsertConnectAction? But this is more general, and finds (some!) chains
    * TODO needs to be aware of implicit port semantics, including chain, implicit blocks, and export
    */
  def findGeneralConnectsTo(container: PyClass, pair: (String, String),
                     project: Project): Errorable[Seq[PyExpression]] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    container.getMethods.toSeq.map { psiFunction => exceptable {  //
      val selfName = psiFunction.getParameterList.getParameters.toSeq
          .exceptEmpty(s"function ${psiFunction.getName} has no self")
          .head.getName
      val connectReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(container),
        s"$selfName.connect")
      val chainReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(container),
        s"$selfName.chain")
      val portReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(container),
        InsertConnectAction.elementPairToText(selfName, pair))

      // Traverse w/ recursive visitor to find all port references inside a self.connect
      val references = mutable.ListBuffer[PyExpression]()
      container.accept(new PyRecursiveElementVisitor() {
        override def visitPyCallExpression(node: PyCallExpression): Unit = {
          if (node.getCallee.textMatches(connectReference) || node.getCallee.textMatches(chainReference)) {
            // an optimization to not traverse down other functions
            super.visitPyCallExpression(node)
          }
        }
        override def visitPyExpression(node: PyExpression): Unit = {
          if (node.textMatches(portReference)) {
            references += node
          }
        }
      })

      references.toSeq.map { reference => // from reference to call expression
        PsiTreeUtil.getParentOfType(reference, classOf[PyCallExpression])
      }.filter { call =>
        if (call == null) {
          false
        } else {
          call.getCallee.textMatches(connectReference) || call.getCallee.textMatches(chainReference)
        }
      }
    }}.collect {
      case Errorable.Success(x) => x
    }.flatten
        .distinct
        .exceptEmpty(s"class ${container.getName} contains no prior connects")
  }

  /** Returns all assignment statements targeting some targetName
    */
  def findAssignmentsTo(container: PyClass, targetName: String,
                        project: Project): Seq[PyAssignmentStatement] = {
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    val assigns = container.getMethods.toSeq.collect { method =>
      val parameters = method.getParameterList.getParameters
      if (parameters.nonEmpty) {
        val selfName = parameters(0).getName
        val targetReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(method),
          s"$selfName.$targetName"
        )

        // TODO support ElementDict and array ops
        // TODO search superclasses
        val methodAssigns = mutable.ListBuffer[PyAssignmentStatement]()
        method.accept(new PyRecursiveElementVisitor() {
          override def visitPyAssignmentStatement(node: PyAssignmentStatement): Unit = {
            if (node.getTargets.exists(expr => expr.textMatches(targetReference))) {
              methodAssigns += (node)
            }
          }
        })
        methodAssigns.toSeq
      } else {
        Seq()
      }
    }.flatten

    if (assigns.isEmpty) {  // search up the superclass chain if needed
      container.getSuperClasses(TypeEvalContext.userInitiated(project, null))
          .flatMap(findAssignmentsTo(_, targetName, project))
          .distinct  // TODO also prevent duplicate work in case of multiple inheritance?
    } else {
      assigns
    }
  }
}
