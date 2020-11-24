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
  // State
  var edgFileAbsPath: Option[String] = None

  // Build GUI components
  textEditor.getComponent.setVisible(true)

  val mainSplitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)
  mainSplitter.setFirstComponent(textEditor.getComponent())

  val ideSplitter = new JBSplitter(true, 0.5f)
  mainSplitter.setSecondComponent(ideSplitter)

  def makeGbc(gridx: Int, gridy: Int, fill: Int = GridBagConstraints.NONE,
              xsize: Int = 1, ysize: Int = 1,
              xweight: Float = 0.0f, yweight: Float = 0.0f): GridBagConstraints = {
    val gbc = new GridBagConstraints()
    gbc.gridx = gridx
    gbc.gridy = gridy
    gbc.fill = fill
    if (xweight == 0 && (fill == GridBagConstraints.HORIZONTAL || fill == GridBagConstraints.BOTH)) {
      gbc.weightx = 1  // default fill weight
    } else {
      gbc.weightx = xweight
    }
    if (yweight == 0 && (fill == GridBagConstraints.VERTICAL || fill == GridBagConstraints.BOTH)) {
      gbc.weighty = 1
    } else {
      gbc.weighty = yweight
    }
    gbc.gridwidth = xsize
    gbc.gridheight = ysize
    gbc
  }

  //
  //  Visualization Panel
  //
  val visualizationPanel = new JPanel(new GridBagLayout())
  visualizationPanel.setBorder(BorderFactory.createEtchedBorder())
  ideSplitter.setFirstComponent(visualizationPanel)

  val fileBrowser = new TextFieldWithBrowseButton()
  visualizationPanel.add(fileBrowser, makeGbc(0, 0, GridBagConstraints.HORIZONTAL))
  val fileLabel = new JLabel("No file")
  visualizationPanel.add(fileLabel, makeGbc(0, 1, GridBagConstraints.HORIZONTAL))

  val visualization = new JTextArea("(empty)")
  visualizationPanel.add(visualization, makeGbc(0, 2, GridBagConstraints.BOTH))

  val descriptor = FileChooserDescriptorFactory.createSingleFileDescriptor()
  fileBrowser.addBrowseFolderListener(new TextBrowseFolderListener(descriptor, null) {
    override def onFileChosen(chosenFile: VirtualFile) {
      val file = VfsUtilCore.virtualToIoFile(chosenFile)
      openEdgFile(file)
    }
  })

  def openEdgFile(file: File): Unit = {
    val absolutePath = file.getAbsolutePath
    fileBrowser.setText(absolutePath)

    val fileInputStream = new FileInputStream(file)

    import edg.schema.schema.Design
    import edg.elem.elem.HierarchyBlock

    val design = Design.parseFrom(fileInputStream)
    design.contents match {
      case Some(block) =>
        edgFileAbsPath = Some(absolutePath)
        fileLabel.setText(s"${block.getClass.toString}")
        visualization.setText(s":${block.toString}")
      case None =>
        edgFileAbsPath = None
        fileLabel.setText(s"Invalid file format: $absolutePath")
    }
    fileInputStream.close()
  }

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

  override def getState(level: FileEditorStateLevel) =
    new SplitFileEditorState(
      edgFileAbsPath,
      textEditor.getState(level)
    )
  override def setState(state: FileEditorState): Unit = state match {
    case state: SplitFileEditorState =>
      state.edgFileAbsPath.map { absPath: String => openEdgFile(new File(absPath)) }
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
class SplitFileEditorState(val edgFileAbsPath: Option[String], val textState: FileEditorState)
    extends FileEditorState {
  override def canBeMergedWith(otherState: FileEditorState, level: FileEditorStateLevel): Boolean =
    otherState match {
      case otherState: SplitFileEditorState => textState.canBeMergedWith(otherState.textState, level)
      case _ => false
    }
}
