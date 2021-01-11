package edg_ide

import com.intellij.codeHighlighting.BackgroundEditorHighlighter
import com.intellij.ide.structureView.StructureViewBuilder
import com.intellij.openapi.editor._
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.fileChooser._
import com.intellij.openapi.ui.{TextBrowseFolderListener, TextFieldWithBrowseButton}
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vfs._
import com.intellij.pom.Navigatable
import com.intellij.ui.JBSplitter
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import javax.swing._
import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import java.beans.PropertyChangeListener
import java.io._

import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.util.FileContentUtil
import edg.elem.elem
import edg.schema.schema
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, HierarchyGraphElk, InferEdgeDirectionTransform, PruneDepthTransform, SimplifyPortTransform}

import scala.sys.process._


class SplitFileEditor(private val textEditor: FileEditor, private val file: VirtualFile)
    extends UserDataHolderBase with TextEditor {
  // State
  var edgFileAbsPath: Option[String] = None
  var edgLibraryAbsPath: Option[String] = None
  var library = new EdgirLibrary(schema.Library())

  // Build GUI components
  textEditor.getComponent.setVisible(true)
  val mainSplitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)
  mainSplitter.setFirstComponent(textEditor.getComponent)

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

  val blockTextField = new JTextField()
  visualizationPanel.add(blockTextField, makeGbc(0, 0, GridBagConstraints.HORIZONTAL))

  val button = new JButton("Visualize")
  visualizationPanel.add(button, makeGbc(0, 1, GridBagConstraints.HORIZONTAL))
  button.addActionListener(new ActionListener() {

    override def actionPerformed(e: ActionEvent) {
      FileContentUtil.reparseOpenedFiles()
      compileAndLoad(blockTextField.getText())
      //TODO: Move the compiled directory to the IDE
      var path = textEditor.getFile.getParent().getCanonicalPath() + "/compiled/"
      openEdgFile(new File(path + "/" + blockTextField.getText() + "/design.edg"))
      openEdgLibrary(new File(path + "/" + blockTextField.getText() + "/lib.lib"))
    }
  })

  val graph = new JElkGraph(HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(elem.HierarchyBlock(), "empty", library)))
  val graphScrollPane = new JBScrollPane(graph) with ZoomingScrollPane
  visualizationPanel.add(graphScrollPane, makeGbc(0, 4, GridBagConstraints.BOTH))

  def compileAndLoad(block: String) = {
    println(VfsUtilCore.virtualToIoFile(textEditor.getFile).getAbsolutePath())
    clearCompiled()
    var cmd = "python3 compile.py --module " + textEditor.getFile.getPath() + " --block " + block + " --output_dir " + textEditor.getFile.getParent().getCanonicalPath() + "/compiled/" + block
    Process(cmd, new File(textEditor.getFile.getParent().getCanonicalPath())).!!
  }

  def clearCompiled() = {
    var children = textEditor.getFile.getParent.findChild("compiled").getChildren
    for (c <- children) c.delete(this)
  }


  val fileDescriptor: FileChooserDescriptor = FileChooserDescriptorFactory.createSingleFileDescriptor()

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

    val fileInputStream = new FileInputStream(file)
    val design: schema.Design = schema.Design.parseFrom(fileInputStream)
    design.contents match {
      case Some(block) =>
        edgFileAbsPath = Some(absolutePath)
        val edgirGraph = EdgirGraph.blockToNode(block, "root", library)
        val layoutGraphRoot = HierarchyGraphElk.HGraphNodeToElk(
          CollapseBridgeTransform(CollapseLinkTransform(
            InferEdgeDirectionTransform(SimplifyPortTransform(
              PruneDepthTransform(edgirGraph, 2))))))  // TODO configurable depth
        graph.setGraph(layoutGraphRoot)
        designTree.setModel(new EdgTreeTableModel(block))
        designTree.setRootVisible(false)  // this seems to get overridden when the model is updated
      case None =>
        edgFileAbsPath = None
        //fileLabel.setText(s"Invalid file format: $absolutePath")
    }
    fileInputStream.close()
  }

  def openEdgLibrary(file: File): Unit = {
    val absolutePath = file.getAbsolutePath

    val fileInputStream = new FileInputStream(file)
    val libraryProto: schema.Library = schema.Library.parseFrom(fileInputStream)
    libraryProto.root match {
      case Some(namespace) =>
        edgLibraryAbsPath = Some(absolutePath)
        library = new EdgirLibrary(libraryProto)
        libraryTree.setModel(new EdgirLibraryTreeTableModel(library))
        libraryTree.setRootVisible(false)  // this seems to get overridden when the model is updated
        // TODO: actual loading here
      case None =>
        edgLibraryAbsPath = None
        library = new EdgirLibrary(schema.Library())
    }
    fileInputStream.close()
  }

  //
  // Implementation for abstract TextEditor
  //
  override def getName = "PyCharm with EDG Live Visualization"

  override def getComponent: JComponent = mainSplitter
  override def getPreferredFocusedComponent: JComponent = textEditor.getPreferredFocusedComponent

  override def getState(level: FileEditorStateLevel) =
    new SplitFileEditorState(
      edgFileAbsPath,
      edgLibraryAbsPath,
      textEditor.getState(level)
    )
  override def setState(state: FileEditorState): Unit = state match {
    case state: SplitFileEditorState =>
      // TODO: Fill visualization if there are already .edg files and library files
      textEditor.setState(state.textState)
    case _ =>  // discard state type
  }

  override def isModified: Boolean = textEditor.isModified
  override def isValid: Boolean = textEditor.isValid
  override def selectNotify(): Unit = textEditor.selectNotify()
  override def deselectNotify(): Unit = textEditor.deselectNotify()

  override def addPropertyChangeListener(listener: PropertyChangeListener): Unit =
    textEditor.addPropertyChangeListener(listener)
  override def removePropertyChangeListener(listener: PropertyChangeListener): Unit =
    textEditor.removePropertyChangeListener(listener)

  override def getBackgroundHighlighter: BackgroundEditorHighlighter = textEditor.getBackgroundHighlighter
  override def getCurrentLocation: FileEditorLocation = textEditor.getCurrentLocation
  override def getStructureViewBuilder: StructureViewBuilder = textEditor.getStructureViewBuilder

  override def dispose(): Unit = Disposer.dispose(textEditor)

  override def getEditor: Editor = textEditor.asInstanceOf[TextEditor].getEditor

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
