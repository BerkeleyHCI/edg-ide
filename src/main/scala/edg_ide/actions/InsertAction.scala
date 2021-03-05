package edg_ide.actions

import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.pom.Navigatable
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi.{LanguageLevel, PyAssignmentStatement, PyClass, PyElementGenerator, PyFunction, PyStatementList}
import edg.util.Errorable
import edg_ide.ui.PopupUtils
import edg_ide.util.{exceptable, requireExcept}
import edg_ide.util.ExceptionNotifyImplicits._

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`


object InsertAction {

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
    val psiFile = container.getContainingFile.exceptNull("no containing file")
    requireExcept(container.isSubclass(InsertBlockAction.VALID_SUPERCLASS,
      TypeEvalContext.codeCompletion(project, psiFile)),
      s"class ${container.getName} is not a subclass of ${InsertBlockAction.VALID_SUPERCLASS}")

    val methods = container.getMethods.toSeq.collect {
      case method if InsertBlockAction.VALID_FUNCTION_NAMES.contains(method.getName) => method
    }.exceptEmpty(s"class ${container.getName} contains no insertion methods")

    methods
  }

  def createNameEntryPopup(title: String, containingPsiClass: PyClass,
                           project: Project)(accept: String => Errorable[Unit]): Unit = exceptable {
    val contextAttributeNames = containingPsiClass.getInstanceAttributes.toSeq.map(_.getName)

    PopupUtils.createStringEntryPopup(title, project) { name => exceptable {
      LanguageNamesValidation.isIdentifier(PythonLanguage.getInstance(), name)
          .exceptFalse("not an identifier")
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
  val VALID_FUNCTION_NAMES = Set("__init__", "contents")
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"

  val notificationGroup = "edg_ide.actions.InsertBlockAction"

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
          classOf[PyAssignmentStatement],
          s"$selfName.$name = $selfName.Block(${libClass.getName}())"
        )

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
  /** Creates an action to insert a new connect statement containing (interior block, block's port).
    * If interior_block is empty, the port is a port of the parent.
    * Location is checked to ensure all the ports are visible.
    */
  def createInsertConnectFlow(after: PsiElement, elements: Seq[(String, String)], actionName: String,
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

    // TODO check containing class
          // TODO check reachability

    def insertConnectFlow: Unit = {

      InsertAction.createNameEntryPopup("Connect Name (optional)", containingPsiClass, project) { name => exceptable {
        val elementsText = elements map {
          case ("", portName) => portName
          case (blockName, portName) => s"$blockName.$portName"
        }
        val connectStatementText = s"$selfName.connect(${elementsText.mkString(", ")})"
        val statementText = if (name.isEmpty) {
          connectStatementText
        } else {
          s"$selfName.$name = $connectStatementText"
        }

        val newAssign = psiElementGenerator.createFromText(LanguageLevel.forElement(after),
          classOf[PyAssignmentStatement],
          statementText
        )

        writeCommandAction(project).withName(actionName).run(() => {
          val added = containingPsiList.addAfter(newAssign, after)
          continuation(added)
        })
      }}
    }
    () => insertConnectFlow
  }
}
