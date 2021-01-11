package edg_ide

import com.intellij.openapi.Disposable
import com.intellij.openapi.editor._
import com.intellij.openapi.editor.event._
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.fileEditor.impl.text.PsiAwareTextEditorProvider
import com.intellij.openapi.fileTypes.FileTypes
import com.intellij.openapi.project.DumbAware
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.testFramework.LightVirtualFile
import com.intellij.util.Alarm
import org.jdom.Attribute
import org.jdom.Element


object SplitFileEditorProviderUtils {
  def getBuilderFromEditorProvider(provider: FileEditorProvider, project: Project, file: VirtualFile) = provider match {
    case provider: AsyncFileEditorProvider => provider.createEditorAsync(project, file)
    case _ => new AsyncFileEditorProvider.Builder() {
      def build: FileEditor = provider.createEditor(project, file)
    }
  }
}


class SplitFileEditorProvider extends AsyncFileEditorProvider with DumbAware {
  val textProvider = new PsiAwareTextEditorProvider
  val editorTypeId = "pycharm-edg"

  override def accept(project: Project, file: VirtualFile): Boolean =
    textProvider.accept(project, file) && "py" == file.getExtension

  override def createEditor(project: Project, file: VirtualFile): FileEditor =
    createEditorAsync(project, file).build

  override def getEditorTypeId: String = editorTypeId

  override def getPolicy = FileEditorPolicy.HIDE_DEFAULT_EDITOR

  override def createEditorAsync(project: Project, file: VirtualFile): AsyncFileEditorProvider.Builder = {
    val textBuilder = SplitFileEditorProviderUtils.getBuilderFromEditorProvider(textProvider, project, file)

    new AsyncFileEditorProvider.Builder() {
      def build: FileEditor = {
        new SplitFileEditor(textBuilder.build().asInstanceOf[TextEditor], file)
      }
    }
  }

  // Editor state save / restore
  val ELEMENT_TEXT_EDITOR = "TEXT_EDITOR"
  val ELEMENT_EDG_FILE = "EDG_FILE"
  val ELEMENT_LIBRARY_FILE = "LIBRARY_FILE"

  override def readState(sourceElement: Element, project: Project, file: VirtualFile): SplitFileEditorState = {
    val textState = sourceElement.getChild(ELEMENT_TEXT_EDITOR) match {
      case null => null
      case textStateChild => textProvider.readState(textStateChild, project, file)
    }
    val edgFileAbsPath = Option(sourceElement.getAttribute(ELEMENT_EDG_FILE)).map { _.getValue }
    val edgLibraryAbsPath = Option(sourceElement.getAttribute(ELEMENT_LIBRARY_FILE)).map { _.getValue }

    new SplitFileEditorState(edgFileAbsPath, edgLibraryAbsPath, textState)
  }

  override def writeState(state: FileEditorState, project: Project, targetElement: Element): Unit = state match {
    case state: SplitFileEditorState =>
      Option(state.textState).map { textState =>
          val textChild = new Element(ELEMENT_TEXT_EDITOR)
          textProvider.writeState(textState, project, textChild)
          targetElement.addContent(textChild)
      }
      state.edgFileAbsPath.map { absPath =>
        targetElement.setAttribute(ELEMENT_EDG_FILE, absPath)
      }
      state.edgLibraryAbsPath.map { absPath =>
        targetElement.setAttribute(ELEMENT_LIBRARY_FILE, absPath)
      }
    case _ =>  // discard
  }
}
