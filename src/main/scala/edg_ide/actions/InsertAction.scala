package edg_ide.actions

import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.pom.Navigatable
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile}
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi.{PyClass, PyFunction}
import edg.util.Errorable
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits._
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`


object InsertAction {
  def getPyClassOfContext(project: Project): Errorable[PyClass] = exceptable {
    val (contextPath, contextBlock) = BlockVisualizerService(project)
        .getContextBlock.exceptNone("no context block")
    requireExcept(contextBlock.superclasses.length == 1, "invalid class for context block")
    DesignAnalysisUtils.pyClassOf(contextBlock.superclasses.head, project).exceptError
  }

  def getCaretAtFile(file: PsiFile, expectedClass: PyClass, project: Project): Errorable[PsiElement] = exceptable {
    val editors = FileEditorManager.getInstance(project).getSelectedEditors
        .filter { editor => editor.getFile == file.getVirtualFile }
    requireExcept(editors.length > 0, s"no editors for ${file.getName} open")
    requireExcept(editors.length == 1, s"multiple editors for ${file.getName} open")
    val contextEditor = editors.head.instanceOfExcept[TextEditor]("not a text editor")
    val contextOffset = contextEditor.getEditor.getCaretModel.getOffset
    val element = file.findElementAt(contextOffset).exceptNull(s"invalid caret position in ${file.getName}")

    val containingPsiClass = PsiTreeUtil.getParentOfType(element, classOf[PyClass])
        .exceptNull(s"not in a class in ${file.getName}")
    requireExcept(containingPsiClass == expectedClass, s"not in expected class ${expectedClass.getName} in ${file.getName}")

    element
  }

  def findInsertionPoints(container: PyClass, validFunctions: Seq[String]): Errorable[Seq[PyFunction]] = exceptable {
    val methods = container.getMethods.toSeq.collect {
      case method if validFunctions.contains(method.getName) => method
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
  def navigateElementFn(name: String, element: PsiElement): Unit = element match {
    case navigatable: Navigatable => navigatable.navigate(true)
  }
}
