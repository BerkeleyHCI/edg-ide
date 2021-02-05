package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.fileChooser.{FileChooserDescriptor, FileChooserDescriptorFactory}
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.{TextBrowseFolderListener, TextFieldWithBrowseButton}
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}
import com.intellij.ui.JBSplitter
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.FileContentUtil
import edg.elem.elem
import edg.schema.schema
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, HierarchyGraphElk, InferEdgeDirectionTransform, PruneDepthTransform, SimplifyPortTransform}
import edg_ide.{EdgTreeTableModel, EdgirLibrary, EdgirLibraryTreeTableModel, JElkGraph, ZoomingScrollPane}
import org.eclipse.elk.graph.ElkGraphElement

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.io.{File, FileInputStream}
import javax.swing.{BorderFactory, JButton, JLabel, JPanel, JTextField}


object Gbc {
  def apply(gridx: Int, gridy: Int, fill: Int = GridBagConstraints.NONE,
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
}


class BlockVisualizerPanel(val project: Project) extends JPanel {
  // State
  //
  private var library = new EdgirLibrary(schema.Library())
  private var design = schema.Design()

  // GUI-facing state
  //
  private var selectedPath: Seq[String] = Seq()  // root implicitly selected by default

  // GUI Components
  //
  private val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.ui.BlockVisualizerPanel")

  private val mainSplitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)

  // Top half (status and block visualization)
  //
  private val visualizationPanel = new JPanel(new GridBagLayout())
  mainSplitter.setFirstComponent(visualizationPanel)

  private val blockFileLabel = new JLabel("Block File")
  visualizationPanel.add(blockFileLabel, Gbc(0, 0, GridBagConstraints.HORIZONTAL))
  private val blockFile = new TextFieldWithBrowseButton()
  visualizationPanel.add(blockFile, Gbc(0, 1, GridBagConstraints.HORIZONTAL))
  private val fileDescriptor: FileChooserDescriptor = FileChooserDescriptorFactory.createSingleFileDescriptor()
  blockFile.addBrowseFolderListener(new TextBrowseFolderListener(fileDescriptor, null) {
    override def onFileChosen(chosenFile: VirtualFile) {
      val file = VfsUtilCore.virtualToIoFile(chosenFile)
      val fileInputStream = new FileInputStream(file)
      val design = schema.Design.parseFrom(fileInputStream)
      setDesign(design)
      fileInputStream.close()
    }
  })

  private val blockNameLabel = new JLabel("Block Name")
  visualizationPanel.add(blockNameLabel, Gbc(1, 0, GridBagConstraints.HORIZONTAL))
  private val blockName = new JTextField()
  visualizationPanel.add(blockName, Gbc(1, 1, GridBagConstraints.HORIZONTAL))

  private val button = new JButton("Update")
  visualizationPanel.add(button, Gbc(2, 1, GridBagConstraints.HORIZONTAL))
  button.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      // TODO IMPLEMENT ME
    }
  })

  private val status = new JLabel("TODO Status here")
  visualizationPanel.add(status, Gbc(0, 2, GridBagConstraints.HORIZONTAL, xsize=3))

  private val emptyHGraph = HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(elem.HierarchyBlock(), "empty", library))

  private val graph = new JElkGraph(emptyHGraph) {
    override def onSelected(node: ElkGraphElement): Unit = {
      selectedPath = getSelectedByPath
      selectedPath = selectedPath.slice(1, selectedPath.length)  // TODO this prunes the prefixing 'design' elt
      notificationGroup.createNotification(
        s"selected path $selectedPath", NotificationType.WARNING)
          .notify(project)
    }
  }
  private val graphScrollPane = new JBScrollPane(graph) with ZoomingScrollPane
  visualizationPanel.add(graphScrollPane, Gbc(0, 3, GridBagConstraints.BOTH, xsize=3))

  // Bottom half (design tree and task tabs)
  //
  private val bottomSplitter = new JBSplitter(false, 0.33f, 0.1f, 0.9f)
  mainSplitter.setSecondComponent(bottomSplitter)

  private val designTree = new TreeTable(new EdgTreeTableModel(edg.elem.elem.HierarchyBlock()))
  designTree.setShowColumns(true)
  private val designTreeScrollPane = new JBScrollPane(designTree)
  bottomSplitter.setFirstComponent(designTreeScrollPane)

  private val libraryPanel = new LibraryPanel()
  bottomSplitter.setSecondComponent(libraryPanel)

  setLayout(new BorderLayout())
  add(mainSplitter)

  // Actions
  //
  def setFileBlock(file: VirtualFile, block: String) = {
    blockFile.setText(file.getCanonicalPath)
    blockName.setText(block)
  }

  def setDesign(design: schema.Design): Unit = design.contents match {
    case Some(block) =>
      this.design = design
      val edgirGraph = EdgirGraph.blockToNode(block, "root", library)
      val layoutGraphRoot = HierarchyGraphElk.HGraphNodeToElk(
        CollapseBridgeTransform(CollapseLinkTransform(
          InferEdgeDirectionTransform(SimplifyPortTransform(
            PruneDepthTransform(edgirGraph, 2))))))  // TODO configurable depth
      graph.setGraph(layoutGraphRoot)
      designTree.setModel(new EdgTreeTableModel(block))
      designTree.setRootVisible(false)  // this seems to get overridden when the model is updated
    case None => graph.setGraph(emptyHGraph)
  }

  def setLibrary(library: schema.Library): Unit = libraryPanel.setLibrary(library)
}


class LibraryPanel() extends JPanel {
  // State
  //
  private var library = new EdgirLibrary(schema.Library())

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  private val libraryTree = new TreeTable(new EdgirLibraryTreeTableModel(new EdgirLibrary(edg.schema.schema.Library())))
  libraryTree.setShowColumns(true)
  private val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  splitter.setSecondComponent(libraryTreeScrollPane)

  private val visualizer = new JLabel("TODO Library Visualizer here")
  splitter.setSecondComponent(visualizer)

  setLayout(new BorderLayout())
  add(splitter)

  // Actions
  //
  def setLibrary(library: schema.Library): Unit = library.root match {
    case Some(namespace) =>
      this.library = new EdgirLibrary(library)
      libraryTree.setModel(new EdgirLibraryTreeTableModel(this.library))
      libraryTree.setRootVisible(false)  // this seems to get overridden when the model is updated
    // TODO: actual loading here
    case None =>
      this.library = new EdgirLibrary(schema.Library())
  }
}
