package edg_ide

import com.intellij.codeHighlighting.BackgroundEditorHighlighter
import com.intellij.ide.structureView.StructureViewBuilder
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor._
import com.intellij.openapi.editor.colors.EditorFontType
import com.intellij.openapi.editor.impl.EditorImpl
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.fileChooser._
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vfs._
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

import com.intellij.openapi.ui.{TextFieldWithBrowseButton, TextBrowseFolderListener}


class SplitFileEditor(private val textEditor: FileEditor, private val file: VirtualFile)
    extends UserDataHolderBase with TextEditor {
  // Build GUI components
  textEditor.getComponent.setVisible(true)

  val mainSplitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)
  mainSplitter.setFirstComponent(textEditor.getComponent())

  val ideSplitter = new JBSplitter(true, 0.5f)
  ideSplitter.setHonorComponentsMinimumSize(true)
  ideSplitter.setShowDividerControls(true)
  ideSplitter.setBorder(BorderFactory.createLoweredBevelBorder())
  mainSplitter.setSecondComponent(ideSplitter)

  def makeGbc(gridx: Int, gridy: Int, fill: Int, xsize: Int = 1, ysize: Int = 1): GridBagConstraints = {
    val gbc = new GridBagConstraints()
    gbc.gridx = gridx
    gbc.gridy = gridy
    gbc.gridwidth = xsize
    gbc.gridheight = ysize
    gbc.fill = fill
    gbc
  }

  //
  //  Visualization Panel
  //
  val visualizationPanel = new JPanel(new GridBagLayout())
  ideSplitter.setFirstComponent(visualizationPanel)


  val fileBrowser = new TextFieldWithBrowseButton()
  visualizationPanel.add(fileBrowser, makeGbc(0, 0, GridBagConstraints.HORIZONTAL))
  val fileLabel = new JLabel("empty")
  visualizationPanel.add(fileLabel, makeGbc(0, 1, GridBagConstraints.HORIZONTAL))

  val descriptor = FileChooserDescriptorFactory.createSingleFileDescriptor()
  fileBrowser.addBrowseFolderListener(new TextBrowseFolderListener(descriptor, null) {
    override def onFileChosen(chosenFile: VirtualFile) {
      val absolutePath = VfsUtilCore.virtualToIoFile(chosenFile).getAbsolutePath()
      fileLabel.setText(absolutePath)
    }
  })

  //
  // Tree Panel
  //
  val treePanel = new JPanel(new BorderLayout())
  ideSplitter.setSecondComponent(treePanel)

  //
  // Implementation for abstract TextEditor
  //
  override def getName() = "PyCharm with EDG Live Visualization"

  override def getComponent(): JComponent = mainSplitter
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
