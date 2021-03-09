package edg_ide.actions

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{LanguageLevel, PyCallExpression, PyClass, PyElementGenerator, PyExpression, PyFunction, PyRecursiveElementVisitor, PyReferenceExpression, PyStatement, PyStatementList}
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}

import scala.collection.mutable


object InsertConnectAction {
  def findConnectsTo(container: PyClass, pair: (String, String),
                     project: Project): Errorable[Seq[PyExpression]] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    container.getMethods.toSeq.map { psiFunction => exceptable {  //
      val selfName = psiFunction.getParameterList.getParameters.toSeq
          .exceptEmpty(s"function ${psiFunction.getName} has no self")
          .head.getName
      val connectReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(container),
        s"$selfName.connect")
      val portReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(container),
        elementPairToText(selfName, pair))

      // Traverse w/ recursive visitor to find all port references inside a self.connect
      val references = mutable.ListBuffer[PyReferenceExpression]()
      container.accept(new PyRecursiveElementVisitor() {
        override def visitPyCallExpression(node: PyCallExpression): Unit = {
          if (node.getCallee.textMatches(connectReference)) {  // an optimization to not traverse down other functions
            super.visitPyCallExpression(node)
          }
        }
        override def visitPyReferenceExpression(node: PyReferenceExpression): Unit = {
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
          call.getCallee.textMatches(connectReference)
        }
      }
    }}.collect {
      case Errorable.Success(x) => x
    }.flatten
        .distinct
        .exceptEmpty(s"class ${container.getName} contains no prior connects")
  }

  def elementPairToText(selfName: String, pair: (String, String)): String = pair match {
    case ("", portName) => s"$selfName.$portName"
    case (blockName, portName) => s"$selfName.$blockName.$portName"
  }

  private def pairAttributesAfter(after: PsiElement, portPairs: Seq[(String, String)],
                                  containingPsiClass: PyClass, project: Project): Seq[String] = {
    portPairs.map {  // to attribute name
      case ("", portName) => portName
      case (blockName, portName) => blockName  // assume portName is in blockName
    }.map { attributeName =>  // to ([assigns], attribute name)
      (DesignAnalysisUtils.findAssignmentsTo(containingPsiClass, attributeName, project),
          attributeName)
    }.flatMap { case (assigns, attributeName) =>  // flatten assign statements to attribute names if any are after
      val containsAssignAfter = assigns.map { assign =>
        DesignAnalysisUtils.elementAfterEdg(assign, after, project)
      }.contains(Some(false))
      if (containsAssignAfter) {
        Some(attributeName)
      } else {
        None
      }
    }
  }

  /** Creates an action to insert a new connect statement containing (interior block, block's port).
    * If interior_block is empty, the port is a port of the parent.
    * Location is checked to ensure all the ports are visible.
    *
    * TODO should elements be more structured to allow more analysis checking?
    * This generally assumes that elements are sane and in the class of after.
    */
  def createInsertConnectFlow(after: PsiElement, requiresName: Boolean, portPairs: Seq[(String, String)],
                              actionName: String, project: Project,
                              continuation: (String, PsiElement) => Unit): Errorable[() => Unit] = exceptable {
    val containingPsiList = after.getParent
        .instanceOfExcept[PyStatementList](s"invalid position for insertion")
    val containingPsiFunction = containingPsiList.getParent
        .instanceOfExcept[PyFunction]("not in a function")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyClass])
        .exceptNull("not in a class")

    // Check that referenced attributes (eg, port or block names) are not defined after the current position
    val attributesAfter = pairAttributesAfter(after, portPairs, containingPsiClass, project)
    requireExcept(attributesAfter.isEmpty, s"${attributesAfter.mkString(", ")} defined after insertion position")

    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
        .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
        .head.getName

    def insertConnectAction(name: String): Unit = {
      val elementsText = portPairs map { elementPairToText(selfName, _) }
      val connectStatementText = s"$selfName.connect(${elementsText.mkString(", ")})"
      val statementText = if (name.isEmpty) {
        connectStatementText
      } else {
        s"$selfName.$name = $connectStatementText"
      }

      val newStatement = psiElementGenerator.createFromText(LanguageLevel.forElement(after),
        classOf[PyStatement], statementText)

      val added = writeCommandAction(project).withName(actionName).compute(() => {
        containingPsiList.addAfter(newStatement, after)
      })
      continuation(name, added)
    }

    def insertConnectFlow(): Unit = {
      if (requiresName) {
        InsertAction.createNameEntryPopup("Connect Name (optional)", containingPsiClass, project,
          allowEmpty=true) { name => exceptable { insertConnectAction(name) } }
      } else {
        insertConnectAction("")
      }
    }
    () => insertConnectFlow()
  }

  def createAppendConnectFlow(within: PsiElement,
                              allPortPairs: Seq[(String, String)], portPairs: Seq[(String, String)],
                              actionName: String, project: Project,
                              continuation: (String, PsiElement) => Unit): Errorable[() => Unit] = exceptable {
    val containingPsiCall = within match {
      case within: PyCallExpression => within
      case within => PsiTreeUtil.getParentOfType(within, classOf[PyCallExpression])
          .exceptNull(s"not in an call")
    }
    val containingPsiFunction = PsiTreeUtil.getParentOfType(within, classOf[PyFunction])
        .exceptNull("not in a function")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
        .exceptNull("not in a class")

    // Check that referenced attributes (eg, port or block names) are not defined after the current position
    val attributesAfter = pairAttributesAfter(within, portPairs, containingPsiClass, project)
    requireExcept(attributesAfter.isEmpty, s"${attributesAfter.mkString(", ")} defined after insertion position")

    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
        .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
        .head.getName
    val connectReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(within),
      s"$selfName.connect")
    requireExcept(containingPsiCall.getCallee.textMatches(connectReference), "call not to connect")

    val portRefElements = portPairs.map { elementPairToText(selfName, _) }
        .map { refText =>
          psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(within),
            refText)
        }
    val allPortRefElements = allPortPairs.map { elementPairToText(selfName, _) }
        .map { refText =>
          psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(within),
            refText)
        }

    requireExcept(containingPsiCall.getArguments.forall { arg =>
      allPortRefElements.exists { allPortRef =>
          arg.textMatches(allPortRef)
        }
    }, s"connect doesn't contain previously connected element")

    val newPortRefElements = portRefElements.filter { portRef =>  // don't re-generate existing connects
      !containingPsiCall.getArguments.exists { arg =>
        arg.textMatches(portRef)
      }
    }

    () => {
      writeCommandAction(project).withName(actionName).compute(() => {
        newPortRefElements.map { portRefElement =>
          containingPsiCall.getArgumentList.addArgument(portRefElement)
        }
      })
      continuation("", containingPsiCall.getArgumentList)  // TODO name doesn't make sense here?
    }
  }
}
