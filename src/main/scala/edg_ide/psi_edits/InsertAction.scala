package edg_ide.psi_edits

import com.intellij.openapi.editor.CaretState
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile, PsiWhiteSpace}
import com.jetbrains.python.psi.PyClass
import edg.util.Errorable
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.ExceptionNotifyImplicits._
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.reflect.ClassTag

object InsertAction {
  // valid functions to insert block definition statements, sorted by preference
  val kValidFunctionNames = Seq("contents", "__init__") // TODO support generators

  def getPyClassOfContext(project: Project): Errorable[PyClass] = exceptable {
    val (contextPath, contextBlock) =
      BlockVisualizerService(project).getContextBlock.exceptNone("no context block")
    DesignAnalysisUtils.pyClassOf(contextBlock.getSelfClass, project).exceptError
  }

  // Returns the PSI element at (or immediately before) the caret at level within the containerPsiType.
  // This traverses up the PSI tree to get the element directly above containerPsiType,
  // or fails if there is no containing containerPsiType.
  // TODO this does the wrong thing if at the beginning of containerPsiType, eg right at first line of a statement-list
  // TODO this needs a return type that supports the first element
  def getCaretAtFileOfType[T <: PsiElement](file: PsiFile, containerPsiType: Class[T], project: Project)(
      implicit tag: ClassTag[T]
  ): Errorable[PsiElement] = exceptable {
    val editors = FileEditorManager
      .getInstance(project)
      .getSelectedEditors
      .filter { editor => editor.getFile == file.getVirtualFile }
      .toSeq
    val editor = editors
      .onlyExcept(s"not exactly one editor open for ${file.getName}")
      .instanceOfExcept[TextEditor]("not a text editor")
    val caretOffset = editor.getEditor.getCaretModel.getOffset
    val element = file.findElementAt(caretOffset).exceptNull(s"invalid caret position in ${file.getName}")

    // given the leaf element at the caret, returns the rootmost element right before the caret
    def prevElementOf(element: PsiElement): PsiElement = {
      requireExcept(element.getTextRange != null, "element with null range") // if traversing beyond file level
      if (element.getTextRange.getStartOffset == caretOffset) { // caret at beginning of element, so take the previous
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

  // given a PSI element, returns the closest element (self or towards root) that is an immediate child of
  // a containerPsiType; or None if containerPsiType is not in its parents
  @tailrec
  def snapToContainerChild(element: PsiElement, containerPsiType: Class[_ <: PsiElement]): Option[PsiElement] = {
    if (element.getParent == null) {
      return None
    }
    if (containerPsiType.isAssignableFrom(element.getParent.getClass)) {
      Some(element)
    } else {
      snapToContainerChild(element.getParent, containerPsiType)
    }
  }

  // Given a PSI element, returns the insertion point element of type PsiType.
  // Snaps to previous if in whitespace, otherwise returns a parent of type PsiType
  def snapInsertionEltOfType[PsiType <: PsiElement](
      elt: PsiElement
  )(implicit tag: ClassTag[PsiType]): Errorable[PsiType] = {
    elt match {
      case elt: PsiWhiteSpace => snapInsertionEltOfType[PsiType](elt.getPrevSibling)
      case elt: PsiType => Errorable.Success(elt)
      case null => Errorable.Error(s"element not in a ${tag.getClass.getName}")
      case elt => snapInsertionEltOfType[PsiType](elt.getParent)
    }
  }

  def navigateToEnd(element: PsiElement): Unit = {
    new OpenFileDescriptor(
      element.getProject,
      element.getContainingFile.getVirtualFile,
      element.getTextRange.getEndOffset
    )
      .navigate(true)
  }

  def selectAndNavigate(elements: Seq[PsiElement]): Unit = {
    // TODO a proof of concept, should be cleaned up and use live templates instead of a caret selection
    if (elements.isEmpty) {
      return
    }
    val exampleElement = elements.head
    val project = exampleElement.getProject
    val fileEditor =
      FileEditorManager
        .getInstance(project)
        .getSelectedEditor(exampleElement.getContainingFile.getVirtualFile)
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

  // Returns valid insertion points for a HDL line - the last element of the specified functions within some class.
  // Returns the same format as getCaretAtFileOfType
  def findInsertionElements(container: PyClass, validFunctions: Seq[String]): Seq[PsiElement] = {
    container.getMethods.toSeq.collect {
      case method if validFunctions.contains(method.getName) =>
        method.getStatementList.getStatements.lastOption
    }.flatten
  }
}
