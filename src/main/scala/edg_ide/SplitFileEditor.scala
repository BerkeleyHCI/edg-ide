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
import com.intellij.openapi.ui.{TextFieldWithBrowseButton, TextBrowseFolderListener}
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vfs._
import com.intellij.pom.Navigatable
import com.intellij.ui.JBColor
import com.intellij.ui.JBSplitter
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
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

import edg.elem.elem.HierarchyBlock
import edg.schema.schema.{Design, Library}


class SplitFileEditor(private val textEditor: FileEditor, private val file: VirtualFile)
    extends UserDataHolderBase with TextEditor {
  // State
  var edgFileAbsPath: Option[String] = None
  var edgLibraryAbsPath: Option[String] = None
  var library: Option[EdgirLibrary] = None

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
  val libraryBrowser = new TextFieldWithBrowseButton()
  visualizationPanel.add(libraryBrowser, makeGbc(0, 2, GridBagConstraints.HORIZONTAL))
  val libraryLabel = new JLabel("No library")
  visualizationPanel.add(libraryLabel, makeGbc(0, 3, GridBagConstraints.HORIZONTAL))

  val visualization = new JTextArea("(empty)")
  visualization.setLineWrap(true)
  visualization.setWrapStyleWord(true)
  val visualizationScrollPane = new JBScrollPane(visualization)
  visualizationPanel.add(visualizationScrollPane, makeGbc(0, 4, GridBagConstraints.BOTH))

  val fileDescriptor = FileChooserDescriptorFactory.createSingleFileDescriptor()
  fileBrowser.addBrowseFolderListener(new TextBrowseFolderListener(fileDescriptor, null) {
    override def onFileChosen(chosenFile: VirtualFile) {
      val file = VfsUtilCore.virtualToIoFile(chosenFile)
      openEdgFile(file)
    }
  })
  libraryBrowser.addBrowseFolderListener(new TextBrowseFolderListener(fileDescriptor, null) {
    override def onFileChosen(chosenFile: VirtualFile) {
      val file = VfsUtilCore.virtualToIoFile(chosenFile)
      openEdgLibrary(file)
    }
  })

  //
  // Tree Panel
  //
  val treePanel = new JPanel(new GridBagLayout())
  ideSplitter.setSecondComponent(treePanel)

  val designTree = new TreeTable(new EdgTreeTableModel(edg.elem.elem.HierarchyBlock()))
  designTree.setShowColumns(true)
  val designTreeScrollPane = new JBScrollPane(designTree)
  treePanel.add(designTreeScrollPane, makeGbc(0, 0, GridBagConstraints.BOTH))

  val libraryTree = new TreeTable(new EdgirLibraryTreeTableModel(new EdgirLibrary(edg.schema.schema.Library())))
  libraryTree.setShowColumns(true)
  val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  treePanel.add(libraryTreeScrollPane, makeGbc(1, 0, GridBagConstraints.BOTH))

  //
  // Interaction Implementations
  //
  def openEdgFile(file: File): Unit = {
    val absolutePath = file.getAbsolutePath
    fileBrowser.setText(absolutePath)

    val fileInputStream = new FileInputStream(file)
    val design: Design = Design.parseFrom(fileInputStream)
    design.contents match {
      case Some(block) =>
        edgFileAbsPath = Some(absolutePath)
        fileLabel.setText(s"${block.getClass.toString}")
        visualization.setText(s":${block.toString}")
        designTree.setModel(new EdgTreeTableModel(block))
        designTree.setRootVisible(false)  // this seems to get overridden when the model is updated
      case None =>
        edgFileAbsPath = None
        fileLabel.setText(s"Invalid file format: $absolutePath")
    }
    fileInputStream.close()
  }

  def openEdgLibrary(file: File): Unit = {
    val absolutePath = file.getAbsolutePath
    libraryBrowser.setText(absolutePath)

    val fileInputStream = new FileInputStream(file)
    val libraryProto: Library = Library.parseFrom(fileInputStream)
    libraryProto.root match {
      case Some(namespace) =>
        edgLibraryAbsPath = Some(absolutePath)
        library = Some(new EdgirLibrary(libraryProto))
        libraryLabel.setText(s"Library with ${namespace.members.keys.size} elements")
        libraryTree.setModel(new EdgirLibraryTreeTableModel(library.get))
        libraryTree.setRootVisible(false)  // this seems to get overridden when the model is updated
        // TODO: actual loading here
      case None =>
        edgLibraryAbsPath = None
        libraryLabel.setText(s"Invalid or empty library: $absolutePath")
    }
    fileInputStream.close()
  }

  //
  // Implementation for abstract TextEditor
  //
  override def getName() = "PyCharm with EDG Live Visualization"

  override def getComponent(): JComponent = mainSplitter
  override def getPreferredFocusedComponent() = textEditor.getPreferredFocusedComponent()

  override def getState(level: FileEditorStateLevel) =
    new SplitFileEditorState(
      edgFileAbsPath,
      edgLibraryAbsPath,
      textEditor.getState(level)
    )
  override def setState(state: FileEditorState): Unit = state match {
    case state: SplitFileEditorState =>
      state.edgFileAbsPath.map { absPath: String => openEdgFile(new File(absPath)) }
      state.edgLibraryAbsPath.map { absPath: String => openEdgLibrary(new File(absPath)) }
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
class SplitFileEditorState(val edgFileAbsPath: Option[String], val edgLibraryAbsPath: Option[String],
    val textState: FileEditorState)
    extends FileEditorState {
  override def canBeMergedWith(otherState: FileEditorState, level: FileEditorStateLevel): Boolean =
    otherState match {
      case otherState: SplitFileEditorState => textState.canBeMergedWith(otherState.textState, level)
      case _ => false
    }
}
