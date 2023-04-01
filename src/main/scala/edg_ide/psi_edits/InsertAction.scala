package edg_ide.psi_edits

import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.editor.CaretState
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile}
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi.{PyClass, PyFunction, PyStatementList}
import edg.util.Errorable
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits._
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}

import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import scala.reflect.ClassTag


object InsertAction {
  def getPyClassOfContext(project: Project): Errorable[PyClass] = exceptable {
    val (contextPath, contextBlock) = BlockVisualizerService(project)
        .getContextBlock.exceptNone("no context block")
    DesignAnalysisUtils.pyClassOf(contextBlock.getSelfClass, project).exceptError
  }

  // Returns the PSI element at (or immediately before) the caret at level within the containerPsiType.
  // This traverses up the PSI tree to get the element directly above containerPsiType,
  // or fails if there is no containing containerPsiType.
  def getCaretAtFileOfType[T <: PsiElement](file: PsiFile, containerPsiType: Class[T], project: Project)
                                           (implicit tag: ClassTag[T]): Errorable[PsiElement] = exceptable {
    val editors = FileEditorManager.getInstance(project).getSelectedEditors
        .filter { editor => editor.getFile == file.getVirtualFile }.toSeq
    val editor = editors.onlyExcept(s"not exactly one editor open for ${file.getName}")
        .instanceOfExcept[TextEditor]("not a text editor")
    val caretOffset = editor.getEditor.getCaretModel.getOffset
    val element = file.findElementAt(caretOffset).exceptNull(s"invalid caret position in ${file.getName}")

    // given the leaf element at the caret, returns the rootmost element right before the caret
    def prevElementOf(element: PsiElement): PsiElement = {
      if (element.getTextRange.getStartOffset == caretOffset) {  // caret at beginning of element, so take the previous
        val prev = PsiTreeUtil.prevLeaf(element)
        prevElementOf(prev.exceptNull("no element before caret"))
      } else {
        if (containerPsiType.isAssignableFrom(element.getParent.getClass)) {
          element
        } else {
          prevElementOf(element.getParent.exceptNull(s"not in a ${containerPsiType.getSimpleName}"))
        }
      }
    }
    prevElementOf(element)
  }

  /** Returns the PSI element immediately before the cursor of a given file.
    *
    * TODO: this is PyStatementList-aware (generates an insertion location). Needs a more specific name!
    */
  def getCaretForNewClassStatement(expectedClass: PyClass, project: Project): Errorable[PsiElement] = exceptable {
    val file = expectedClass.getContainingFile.exceptNull("no file")
    val prevElement = getCaretAtFileOfType(file, classOf[PyStatementList], project).exceptError

    val containingPsiClass = PsiTreeUtil.getParentOfType(prevElement, classOf[PyClass])
        .exceptNull(s"not in a class in ${file.getName}")
    requireExcept(containingPsiClass == expectedClass, s"not in expected class ${expectedClass.getName} in ${file.getName}")

    prevElement
  }

  def navigateToEnd(element: PsiElement): Unit = {
    new OpenFileDescriptor(element.getProject, element.getContainingFile.getVirtualFile,
      element.getTextRange.getEndOffset)
        .navigate(true)
  }

  def selectAndNavigate(elements: Seq[PsiElement]): Unit = {
    // TODO a proof of concept, should be cleaned up and use live templates instead of a caret selection
    if (elements.isEmpty) {
      return
    }
    val exampleElement = elements.head
    val project = exampleElement.getProject
    val fileEditor = FileEditorManager.getInstance(project).getSelectedEditor(exampleElement.getContainingFile.getVirtualFile)
    val editor = fileEditor match {
      case editor: TextEditor => editor.getEditor
      case _ => return
    }
    val carets = elements.map { element =>
      val startPos = editor.offsetToLogicalPosition(element.getTextRange.getStartOffset)
      val endPos = editor.offsetToLogicalPosition(element.getTextRange.getEndOffset)
      new CaretState(endPos, startPos, endPos)
    }.toList
    editor.getCaretModel.setCaretsAndSelections(carets.asJava)
  }

  def findInsertionPoints(container: PyClass, validFunctions: Seq[String]): Errorable[Seq[PyFunction]] = exceptable {
    val methods = container.getMethods.toSeq.collect {
      case method if validFunctions.contains(method.getName) => method
    }.exceptEmpty(s"class ${container.getName} contains no insertion methods")

    methods
  }

  /** Name entry popup that checks for name legality */
  def createNameEntryPopup(title: String,
                           project: Project)(accept: String => Errorable[Unit]): Unit = exceptable {
    PopupUtils.createStringEntryPopup(title, project) { name => exceptable {
      LanguageNamesValidation.isIdentifier(PythonLanguage.getInstance(), name)
          .exceptFalse("not an identifier")

      accept(name).exceptError
    }}
  }

  /** Name entry popup that checks for name legality and collisions with other class members */
  def createClassMemberNameEntryPopup(title: String, containingPsiClass: PyClass,
                                      project: Project,
                                      allowEmpty: Boolean = false)(accept: String => Errorable[Unit]): Unit = exceptable {
    val contextAttributeNames = containingPsiClass.getInstanceAttributes.asScala.map(_.getName)

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
}
