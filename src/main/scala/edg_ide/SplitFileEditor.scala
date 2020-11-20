package edg_ide

import com.intellij.codeHighlighting.BackgroundEditorHighlighter
import com.intellij.ide.structureView.StructureViewBuilder
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.colors.EditorFontType
import com.intellij.openapi.editor.impl.EditorImpl
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.pom.Navigatable
import com.intellij.ui.JBColor
import com.intellij.ui.JBSplitter
import com.intellij.util.ui.ImageUtil
import org.jetbrains.annotations.NotNull
import org.jetbrains.annotations.Nullable

import javax.imageio.ImageIO
import javax.swing._
import java.awt._
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import java.io._
import java.util._
import java.util


class SplitFileEditor(private val textEditor: FileEditor, private val file: VirtualFile)
    extends UserDataHolderBase with TextEditor {
  // Build GUI components
  val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)
  splitter.setFirstComponent(textEditor.getComponent())

  val visualization = new JPanel(new BorderLayout())
  splitter.setSecondComponent(visualization)

  val component = new JPanel(new BorderLayout())
  component.add(splitter, BorderLayout.CENTER)

  textEditor.getComponent.setVisible(true)

  // Abstract methods that need to be implemented
  override def getName() = "PyCharm with EDG Live Visualization"

  override def getComponent(): JComponent = component
  override def getPreferredFocusedComponent() = textEditor.getPreferredFocusedComponent()

  override def getState(level: FileEditorStateLevel) = new SplitFileEditorState(
    "",
    textEditor.getState(level)
  )
  override def setState(state: FileEditorState): Unit = state match {
    case state: SplitFileEditorState =>
      textEditor.setState(state.textState)
    case _ =>  // discard state type
  }

  override def isModified() = textEditor.isModified()
  override def isValid() = textEditor.isValid()
  override def selectNotify(): Unit = textEditor.selectNotify()
  override def deselectNotify(): Unit = textEditor.deselectNotify()

  override def addPropertyChangeListener(listener: PropertyChangeListener): Unit =
    textEditor.addPropertyChangeListener(listener)
  override def removePropertyChangeListener(listener: PropertyChangeListener): Unit =
    textEditor.removePropertyChangeListener(listener)

  override def getBackgroundHighlighter: BackgroundEditorHighlighter = textEditor.getBackgroundHighlighter()
  override def getCurrentLocation(): FileEditorLocation = textEditor.getCurrentLocation()
  override def getStructureViewBuilder: StructureViewBuilder = textEditor.getStructureViewBuilder()

  override def dispose(): Unit = Disposer.dispose(textEditor)

  override def getEditor: Editor = textEditor.asInstanceOf[TextEditor].getEditor()

  override def canNavigateTo(navigatable: Navigatable): Boolean =
    textEditor.asInstanceOf[TextEditor].canNavigateTo(navigatable)
  override def navigateTo(navigatable: Navigatable): Unit =
    textEditor.asInstanceOf[TextEditor].navigateTo(navigatable)
}


// Container state around TextEditor
class SplitFileEditorState(val splitLayout: String, val textState: FileEditorState)
    extends FileEditorState {
  override def canBeMergedWith(otherState: FileEditorState, level: FileEditorStateLevel): Boolean =
    otherState match {
      case otherState: SplitFileEditorState => textState.canBeMergedWith(otherState.textState, level)
      case _ => false
    }
}
