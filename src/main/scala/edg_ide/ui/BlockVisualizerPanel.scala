package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.fileChooser.{FileChooserDescriptor, FileChooserDescriptorFactory}
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.{TextBrowseFolderListener, TextFieldWithBrowseButton}
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}
import com.intellij.ui.JBSplitter
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.compiler.{Compiler, CompilerError, DesignStructuralValidate, PythonInterface, PythonInterfaceLibrary}
import edg.elem.elem
import edg.schema.schema
import edg.ElemBuilder
import edg.util.timeExec
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, HierarchyGraphElk, InferEdgeDirectionTransform, PruneDepthTransform, SimplifyPortTransform}
import edg_ide.swing.{BlockTreeTableModel, CompilerErrorTreeTableModel, EdgirLibraryTreeTableModel, JElkGraph, ZoomingScrollPane}
import edg.wir
import edg.wir.DesignPath
import org.eclipse.elk.graph.ElkGraphElement

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.io.FileInputStream
import javax.swing.event.{ListSelectionEvent, TreeSelectionEvent, TreeSelectionListener}
import javax.swing.{JButton, JLabel, JPanel, JTextArea, JTextField, ListSelectionModel}


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
  // Internal State
  //
  private var design = schema.Design()
  private val pyLib = new PythonInterfaceLibrary(new PythonInterface())
  private var compiler = new Compiler(design, pyLib)

  // GUI-facing state
  //
  private var selectedPath: Seq[String] = Seq()  // root implicitly selected by default

  // GUI Components
  //
  private val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.ui.BlockVisualizerPanel")

  private val mainSplitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)

  // GUI: Top half (status and block visualization)
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
      super.onFileChosen(chosenFile)  // TODO implement me
    }
  })

  private val blockModuleLabel = new JLabel("Block Module")
  visualizationPanel.add(blockModuleLabel, Gbc(1, 0, GridBagConstraints.HORIZONTAL))
  private val blockModule = new JTextField()
  visualizationPanel.add(blockModule, Gbc(1, 1, GridBagConstraints.HORIZONTAL))

  private val blockNameLabel = new JLabel("Block Name")
  visualizationPanel.add(blockNameLabel, Gbc(2, 0, GridBagConstraints.HORIZONTAL))
  private val blockName = new JTextField()
  visualizationPanel.add(blockName, Gbc(2, 1, GridBagConstraints.HORIZONTAL))

  private val button = new JButton("Update")
  visualizationPanel.add(button, Gbc(3, 1, GridBagConstraints.HORIZONTAL))
  button.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      update()
    }
  })

  private val status = new JLabel("Ready")
  visualizationPanel.add(status, Gbc(0, 2, GridBagConstraints.HORIZONTAL, xsize=4))

  // TODO remove library requirement
  private val emptyHGraph = HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(DesignPath.root, elem.HierarchyBlock()))

  private val graph = new JElkGraph(emptyHGraph) {
    override def onSelected(node: ElkGraphElement): Unit = {
      selectedPath = getSelectedByPath
      selectedPath = selectedPath.slice(1, selectedPath.length)  // TODO this prunes the prefixing 'design' elt
    }
  }
  private val graphScrollPane = new JBScrollPane(graph) with ZoomingScrollPane
  visualizationPanel.add(graphScrollPane, Gbc(0, 3, GridBagConstraints.BOTH, xsize=4))

  // GUI: Bottom half (design tree and task tabs)
  //
  private val bottomSplitter = new JBSplitter(false, 0.33f, 0.1f, 0.9f)
  mainSplitter.setSecondComponent(bottomSplitter)

  private val designTree = new TreeTable(new BlockTreeTableModel(edg.elem.elem.HierarchyBlock()))
  private val designTreeListener = new TreeSelectionListener {  // an object so it can be re-used since a model change wipes everything out
    override def valueChanged(e: TreeSelectionEvent): Unit = {
      import edg_ide.swing.HierarchyBlockNode
      e.getPath.getLastPathComponent match {
        case node: HierarchyBlockNode =>
          if (node.path == DesignPath.root) {
            tabbedPane.setTitleAt(TAB_INDEX_DETAIL, s"Detail (root)")
          } else {
            tabbedPane.setTitleAt(TAB_INDEX_DETAIL, s"Detail (${node.path.steps.last})")
          }
          detailPanel.setLoaded(node.path, design, compiler)
        case value => notificationGroup.createNotification(
          s"Unknown selection $value", NotificationType.WARNING)
            .notify(project)
      }
    }
  }
  designTree.getTree.addTreeSelectionListener(designTreeListener)
  designTree.setShowColumns(true)
  private val designTreeScrollPane = new JBScrollPane(designTree)
  bottomSplitter.setFirstComponent(designTreeScrollPane)

  // GUI: Task Tabs
  //
  private val tabbedPane = new JBTabbedPane()
  bottomSplitter.setSecondComponent(tabbedPane)

  private val libraryPanel = new LibraryPanel()
  tabbedPane.addTab("Library", libraryPanel)
  val TAB_INDEX_LIBRARY = 0

  private val detailPanel = new DetailPanel()
  tabbedPane.addTab("Detail", detailPanel)
  val TAB_INDEX_DETAIL = 1

  private val errorPanel = new ErrorPanel()
  tabbedPane.addTab("Errors", errorPanel)
  val TAB_INDEX_ERRORS = 2
  tabbedPane.setEnabledAt(TAB_INDEX_ERRORS, false)  // no errors by default

  setLayout(new BorderLayout())
  add(mainSplitter)

  // Actions
  //
  def update(): Unit = {
    status.setText(s"Compiling")
    pyLib.setModules(Seq(blockModule.getText()))
    try {
      val fullName = blockModule.getText() + "." + blockName.getText()
      val block = pyLib.getBlock(ElemBuilder.LibraryPath(fullName))
      val design = schema.Design(contents = Some(block))
      val compiler = new Compiler(design, pyLib)
      val (compiled, time) = timeExec {
        compiler.compile()
      }
      val checker = new DesignStructuralValidate()
      val errors = compiler.getErrors() ++ checker.map(compiled)
      if (errors.isEmpty) {
        status.setText(s"Compiled ($time ms)")
        tabbedPane.setEnabledAt(TAB_INDEX_ERRORS, false)
        tabbedPane.setTitleAt(TAB_INDEX_ERRORS, s"Errors")
      } else {
        status.setText(s"Compiled, with ${errors.length} errors ($time ms)")
        tabbedPane.setEnabledAt(TAB_INDEX_ERRORS, true)
        tabbedPane.setTitleAt(TAB_INDEX_ERRORS, s"Errors (${errors.length})")
      }
      setDesign(compiled, compiler)
      libraryPanel.setLibrary(pyLib)
      errorPanel.setErrors(errors)
    } catch {
      case e: Throwable =>
        import java.io.PrintWriter
        import java.io.StringWriter
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        status.setText(s"Failed: ${e.toString}")
        // TODO staleness indicator
    }
  }

  def setFileBlock(file: VirtualFile, module: String, block: String): Unit = {
    blockFile.setText(file.getCanonicalPath)
    blockModule.setText(module)
    blockName.setText(block)
    update()
  }

  def setDesign(design: schema.Design, compiler: Compiler): Unit = design.contents match {
    case Some(block) =>
      this.design = design
      this.compiler = compiler
      // TODO remove EdgirLibrary requirement
      val edgirGraph = EdgirGraph.blockToNode(DesignPath.root, block)
      val layoutGraphRoot = HierarchyGraphElk.HGraphNodeToElk(
        CollapseBridgeTransform(CollapseLinkTransform(
          InferEdgeDirectionTransform(SimplifyPortTransform(
            PruneDepthTransform(edgirGraph, 2))))))  // TODO configurable depth
      graph.setGraph(layoutGraphRoot)
      designTree.setModel(new BlockTreeTableModel(block))
      designTree.setRootVisible(false)  // this seems to get overridden when the model is updated
      designTree.getTree.addTreeSelectionListener(designTreeListener)  // this seems to get overridden when the model is updated
    case None => graph.setGraph(emptyHGraph)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.panelBlockFile = blockFile.getText
    state.panelBlockModule = blockModule.getText()
    state.panelBlockName = blockName.getText()
    state.panelMainSplitterPos = mainSplitter.getProportion
    state.panelBottomSplitterPos = bottomSplitter.getProportion
    state.panelTabIndex = tabbedPane.getSelectedIndex
    libraryPanel.saveState(state)
    detailPanel.saveState(state)
    errorPanel.saveState(state)
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    blockFile.setText(state.panelBlockFile)
    blockModule.setText(state.panelBlockModule)
    blockName.setText(state.panelBlockName)
    mainSplitter.setProportion(state.panelMainSplitterPos)
    bottomSplitter.setProportion(state.panelBottomSplitterPos)
    tabbedPane.setSelectedIndex(state.panelTabIndex)
    libraryPanel.loadState(state)
    detailPanel.loadState(state)
    errorPanel.loadState(state)
  }
}


class LibraryPanel() extends JPanel {
  // State
  //
  private var library: wir.Library = new wir.EdgirLibrary(schema.Library())

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  private val libraryTree = new TreeTable(new EdgirLibraryTreeTableModel(library))
  libraryTree.setShowColumns(true)
  private val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  splitter.setFirstComponent(libraryTreeScrollPane)

  private val visualizer = new JLabel("TODO Library Visualizer here")
  splitter.setSecondComponent(visualizer)

  setLayout(new BorderLayout())
  add(splitter)

  // Actions
  //
  def setLibrary(library: wir.Library): Unit = {
    this.library = library
    libraryTree.setModel(new EdgirLibraryTreeTableModel(this.library))
    libraryTree.setRootVisible(false)  // this seems to get overridden when the model is updated
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.panelLibrarySplitterPos = splitter.getProportion
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    splitter.setProportion(state.panelLibrarySplitterPos)
  }
}


class DetailPanel extends JPanel {
  import edg_ide.swing.ElementDetailTreeModel

  private val tree = new TreeTable(new BlockTreeTableModel(edg.elem.elem.HierarchyBlock()))
  tree.setShowColumns(true)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)

  // Actions
  //
  def setLoaded(path: DesignPath, root: schema.Design, compiler: Compiler): Unit = {
    tree.setModel(new ElementDetailTreeModel(path, root, compiler))
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}


class ErrorPanel extends JPanel {
  private val tree = new TreeTable(new CompilerErrorTreeTableModel(Seq()))
  tree.setShowColumns(true)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)

  // Actions
  //
  def setErrors(errs: Seq[CompilerError]): Unit = {
    tree.setModel(new CompilerErrorTreeTableModel(errs))
    tree.setRootVisible(false)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}
