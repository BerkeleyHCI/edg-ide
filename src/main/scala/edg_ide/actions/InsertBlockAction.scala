package edg_ide.actions

import com.intellij.lang.LanguageNamesValidation
import com.intellij.notification.NotificationGroup
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.pom.Navigatable
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi.{LanguageLevel, PyAssignmentStatement, PyClass, PyElementGenerator, PyFunction, PyStatementList}
import edg.util.Errorable
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionNotify, requireExcept}
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
}


object InsertBlockAction {
  val VALID_FUNCTION_NAMES = Set("__init__", "contents")
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"

  val notificationGroup = "edg_ide.actions.InsertBlockAction"

  def navigateElementFn(element: PsiElement): Unit = element match {
    case navigatable: Navigatable => navigatable.navigate(true)
  }

  /** Creates an action to insert a block of type libClass after some PSI element after.
    * Validation is performed before the action is generated, though the action itself may also return an error.
    */
  def createInsertBlockFlow(after: PsiElement, libClass: PyClass, actionName: String,
                            project: Project, continuation: (PsiElement) => Unit): Errorable[() => Unit] = exceptable {
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
    val contextAttributeNames = containingPsiClass.getInstanceAttributes.toSeq.map(_.getName)

    def insertBlockFlow: Unit = {
      PopupUtils.createStringEntryPopup("Block Name", project) { targetName => exceptable {
        LanguageNamesValidation.isIdentifier(PythonLanguage.getInstance(), targetName)
            .exceptFalse("not an identifier")
        contextAttributeNames.contains(targetName)
            .exceptTrue(s"attribute already exists in ${containingPsiClass.getName}")

        val newAssign = psiElementGenerator.createFromText(LanguageLevel.forElement(after),
          classOf[PyAssignmentStatement],
          s"$selfName.$targetName = $selfName.Block(${libClass.getName}())"
        )

        writeCommandAction(project).withName(actionName).run(() => {
          val added = containingPsiList.addAfter(newAssign, after)
          continuation(added)
          added match {
            case added: Navigatable => added.navigate(true)
            case _ => // ignored
          }
        })
      }}
    }

    () => insertBlockFlow
  }
}


class InsertBlockAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    exceptionNotify("edg_ide.actions.InsertBlockAction", event.getProject) {
      val visualizer = BlockVisualizerService.apply(event.getProject).visualizerPanelOption
          .exceptNull("No visualizer panel")

      val editor = event.getData(CommonDataKeys.EDITOR).exceptNull("No editor")
      val offset = editor.getCaretModel.getOffset
      val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("No PSI file")
      val psiElement = psiFile.findElementAt(offset).exceptNull("No PSI element")

      val psiClass = PsiTreeUtil.getParentOfType(psiElement, classOf[PyClass])
          .exceptNull("No containing PSI class")
      requireExcept(psiClass.isSubclass(InsertBlockAction.VALID_SUPERCLASS, TypeEvalContext.codeCompletion(event.getProject, psiFile)),
        s"Containing class ${psiClass.getName} is not a subclass of ${InsertBlockAction.VALID_FUNCTION_NAMES}")

      val psiContainingList = psiElement.getParent.instanceOfExcept[PyStatementList](s"Invalid location to insert block")
      val psiContainingFunction = psiContainingList.getParent.instanceOfExcept[PyFunction]("Not in a function")
      requireExcept(InsertBlockAction.VALID_FUNCTION_NAMES.contains(psiContainingFunction.getName),
        s"Containing function ${psiContainingFunction.getName} not valid for block insertion")

      val selfName = psiContainingFunction.getParameterList.getParameters()(0).getName

      val psiElementGenerator = PyElementGenerator.getInstance(event.getProject)
      val newAssign = psiElementGenerator.createFromText(LanguageLevel.forElement(psiElement),
        classOf[PyAssignmentStatement],
        s"$selfName.target_name = $selfName.Block(TargetClass())"
      )

      writeCommandAction(event.getProject).withName("Insert Block").run(() => {
        psiContainingList.addAfter(newAssign, psiElement)
      })
    }
  }
}
