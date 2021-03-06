package edg_ide.actions

import collection.mutable
import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.pom.Navigatable
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi.{LanguageLevel, PyAssignmentStatement, PyCallExpression, PyClass, PyElementGenerator, PyExpression, PyFunction, PyRecursiveElementVisitor, PyReferenceExpression, PyStatement, PyStatementList}
import edg.util.Errorable
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}
import edg_ide.util.ExceptionNotifyImplicits._

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`


object InsertAction {
  def getPyClassOfContext(project: Project): Errorable[PyClass] = exceptable {
    val (contextPath, contextBlock) = BlockVisualizerService(project)
        .getContextBlock.exceptNone("no context block")
    requireExcept(contextBlock.superclasses.length == 1, "invalid class for context block")
    DesignAnalysisUtils.pyClassOf(contextBlock.superclasses.head, project).exceptError
  }

  def getCaretAtFile(file: PsiFile, expectedClass: PyClass, project: Project): Errorable[PsiElement] = exceptable {
    val editors = FileEditorManager.getInstance(project)
        .getAllEditors(file.getVirtualFile).toSeq
        .exceptEmpty("file not open")
    requireExcept(editors.length == 1, "multiple editors open")
    val contextEditor = editors.head.instanceOfExcept[TextEditor]("not a text editor")
    val contextOffset = contextEditor.getEditor.getCaretModel.getOffset
    val element = file.findElementAt(contextOffset).exceptNull("invalid caret position")

    val containingPsiClass = PsiTreeUtil.getParentOfType(element, classOf[PyClass])
        .exceptNull("not in a class")
    requireExcept(containingPsiClass == expectedClass, s"not in expected class ${expectedClass.getName}")

    element
  }

  def findInsertionPoints(container: PyClass, project: Project): Errorable[Seq[PyFunction]] = exceptable {
    val methods = container.getMethods.toSeq.collect {
      case method if InsertBlockAction.VALID_FUNCTION_NAMES.contains(method.getName) => method
    }.exceptEmpty(s"class ${container.getName} contains no insertion methods")

    methods
  }

  def createNameEntryPopup(title: String, containingPsiClass: PyClass,
                           project: Project,
                           allowEmpty: Boolean = false)(accept: String => Errorable[Unit]): Unit = exceptable {
    val contextAttributeNames = containingPsiClass.getInstanceAttributes.toSeq.map(_.getName)

    PopupUtils.createStringEntryPopup(title, project) { name => exceptable {
      if (!(allowEmpty && name.isEmpty)) {
        LanguageNamesValidation.isIdentifier(PythonLanguage.getInstance(), name)
            .exceptFalse("not an identifier")
      }
      contextAttributeNames.contains(name)
          .exceptTrue(s"attribute already exists in ${containingPsiClass.getName}")

      accept(name).exceptError
    }}
  }

  /** If the element is navigatable, navigates to it.
    * Used as a continuation to the createInsertBlockFlow.
    */
  def navigateElementFn(element: PsiElement): Unit = element match {
    case navigatable: Navigatable => navigatable.navigate(true)
  }
}


object InsertBlockAction {
  val VALID_FUNCTION_NAMES = Set("__init__", "contents")  // TODO support generators
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"

  /** Creates an action to insert a block of type libClass after some PSI element after.
    * Validation is performed before the action is generated, though the action itself may also return an error.
    */
  def createInsertBlockFlow(after: PsiElement, libClass: PyClass, actionName: String,
                            project: Project, continuation: PsiElement => Unit): Errorable[() => Unit] = exceptable {
    val containingPsiList = after.getParent
        .instanceOfExcept[PyStatementList](s"invalid position for insertion")
    val containingPsiFunction = containingPsiList.getParent
        .instanceOfExcept[PyFunction]("not in a function")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyClass])
        .exceptNull("not in a class")

    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
        .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
        .head.getName

    def insertBlockFlow: Unit = {
      InsertAction.createNameEntryPopup("Block Name", containingPsiClass, project) { name => exceptable {
        val newAssign = psiElementGenerator.createFromText(LanguageLevel.forElement(after),
          classOf[PyAssignmentStatement], s"$selfName.$name = $selfName.Block(${libClass.getName}())")

        writeCommandAction(project).withName(actionName).run(() => {
          val added = containingPsiList.addAfter(newAssign, after)
          continuation(added)
        })
      }}
    }
    () => insertBlockFlow
  }
}


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

  private def elementPairToText(selfName: String, pair: (String, String)): String = pair match {
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
  def createInsertConnectFlow(after: PsiElement, portPairs: Seq[(String, String)], actionName: String,
                              project: Project, continuation: PsiElement => Unit): Errorable[() => Unit] = exceptable {
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

    def insertConnectFlow: Unit = {
      InsertAction.createNameEntryPopup("Connect Name (optional)", containingPsiClass, project,
          allowEmpty=true) { name => exceptable {
        val elementsText = portPairs map { elementPairToText(selfName, _) }
        val connectStatementText = s"$selfName.connect(${elementsText.mkString(", ")})"
        val statementText = if (name.isEmpty) {
          connectStatementText
        } else {
          s"$selfName.$name = $connectStatementText"
        }

        val newStatement = psiElementGenerator.createFromText(LanguageLevel.forElement(after),
          classOf[PyStatement], statementText)

        writeCommandAction(project).withName(actionName).run(() => {
          val added = containingPsiList.addAfter(newStatement, after)
          continuation(added)
        })
      }}
    }
    () => insertConnectFlow
  }

  def createAppendConnectFlow(within: PsiElement, portPairs: Seq[(String, String)], actionName: String,
                              project: Project, continuation: PsiElement => Unit): Errorable[() => Unit] = exceptable {
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
    requireExcept(containingPsiCall.getArguments.exists { arg =>
      arg.textMatches(portRefElements.head)
    }, s"connect doesn't contain ${portPairs.head}")

    () => {
      writeCommandAction(project).withName(actionName).run(() => {
        val added = portRefElements.drop(1).map { portRefElement =>
          containingPsiCall.getArgumentList.addArgument(portRefElement)
        }
        continuation(containingPsiCall.getArgumentList)
      })
    }
  }
}
