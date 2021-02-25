package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.fileChooser.{FileChooserDescriptor, FileChooserDescriptorFactory}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.{TextBrowseFolderListener, TextFieldWithBrowseButton}
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiDocumentManager
import com.intellij.ui.{JBIntSpinner, JBSplitter, TreeTableSpeedSearch}
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane, JBTextArea}
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.jetbrains.python.psi.PyPsiFacade
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.compiler.{Compiler, CompilerError, DesignStructuralValidate, PythonInterfaceLibrary, hdl => edgrpc}
import edg.elem.elem
import edg.schema.schema
import edg.ElemBuilder
import edg.util.Errorable
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, ElkEdgirGraphUtils, HierarchyGraphElk, InferEdgeDirectionTransform, NodeDataWrapper, PortWrapper, PruneDepthTransform, SimplifyPortTransform}
import edg_ide.swing.{BlockTreeTableModel, CompilerErrorTreeTableModel, EdgirLibraryTreeNode, EdgirLibraryTreeTableModel, JElkGraph, RefinementsTreeTableModel, ZoomingScrollPane}
import edg.wir
import edg.wir.DesignPath
import edg_ide.EdgirUtils
import edg_ide.build.BuildInfo
import org.eclipse.elk.graph.ElkGraphElement

import java.awt.event.{ActionEvent, ActionListener, MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.TreePath
import javax.swing.{JButton, JLabel, JMenuItem, JPanel, JPopupMenu, JTextField}
import scala.jdk.CollectionConverters.IterableHasAsScala


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
  private var compiler = new Compiler(design, EdgCompilerService(project).pyLib)

  // GUI-facing state
  //
  private var selectedPath: DesignPath = DesignPath()  // root implicitly selected by default
  // This hack ignores actions when programmatically synchronizing the design tree and graph
  // TODO refactor w/ shared model eg https://docs.oracle.com/javase/tutorial/uiswing/examples/components/index.html#SharedModelDemo
  private var ignoreActions: Boolean = false
  private var compilerRunning = new AtomicBoolean(false)

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
  visualizationPanel.add(button, Gbc(3, 0, GridBagConstraints.HORIZONTAL))
  button.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      update()
    }
  })

  // TODO max value based on depth of tree?
  private val depthSpinner = new JBIntSpinner(1, 1, 100)
  // TODO update visualization on change?
  visualizationPanel.add(depthSpinner, Gbc(3, 1, GridBagConstraints.HORIZONTAL))

  private val status = new JLabel(s"Ready " +
      s"(version ${BuildInfo.version} built at ${BuildInfo.builtAtString}, " +
      s"scala ${BuildInfo.scalaVersion}, sbt ${BuildInfo.sbtVersion})"
  )
  visualizationPanel.add(status, Gbc(0, 2, GridBagConstraints.HORIZONTAL, xsize=4))

  // TODO remove library requirement
  private val emptyHGraph = HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(DesignPath(), elem.HierarchyBlock()))

  private val graph = new JElkGraph(emptyHGraph) {
    override def onSelected(node: ElkGraphElement): Unit = {
      if (ignoreActions) {
        return
      }
      Option(node.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property)).foreach(select)
    }
  }
  private val graphScrollPane = new JBScrollPane(graph) with ZoomingScrollPane
  visualizationPanel.add(graphScrollPane, Gbc(0, 3, GridBagConstraints.BOTH, xsize=4))

  // GUI: Bottom half (design tree and task tabs)
  //
  private val bottomSplitter = new JBSplitter(false, 0.33f, 0.1f, 0.9f)
  mainSplitter.setSecondComponent(bottomSplitter)

  private var designTreeModel = new BlockTreeTableModel(edg.elem.elem.HierarchyBlock())
  private val designTree = new TreeTable(designTreeModel)
  new TreeTableSpeedSearch(designTree)
  private val designTreeListener = new TreeSelectionListener {  // an object so it can be re-used since a model change wipes everything out
    override def valueChanged(e: TreeSelectionEvent): Unit = {
      import edg_ide.swing.HierarchyBlockNode
      if (ignoreActions) {
        return
      }
      e.getPath.getLastPathComponent match {
        case node: HierarchyBlockNode => select(node.path)
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

  private val libraryPanel = new LibraryPanel(project)
  tabbedPane.addTab("Library", libraryPanel)
  val TAB_INDEX_LIBRARY = 0

  private val refinementsPanel = new RefinementsPanel()
  tabbedPane.addTab("Refinements", refinementsPanel)
  val TAB_INDEX_REFINEMENTS = 1

  private val detailPanel = new DetailPanel()
  tabbedPane.addTab("Detail", detailPanel)
  val TAB_INDEX_DETAIL = 2

  private val errorPanel = new ErrorPanel()
  tabbedPane.addTab("Errors", errorPanel)
  val TAB_INDEX_ERRORS = 3

  setLayout(new BorderLayout())
  add(mainSplitter)

  // Actions
  //
  def getModule: String = blockModule.getText()

  def select(path: DesignPath): Unit = {
    if (path == DesignPath()) {
      tabbedPane.setTitleAt(TAB_INDEX_DETAIL, s"Detail (root)")
    } else {
      tabbedPane.setTitleAt(TAB_INDEX_DETAIL, s"Detail (${path.steps.last})")
    }
    detailPanel.setLoaded(path, design, compiler)
    selectedPath = path

    ignoreActions = true

    val (targetElkPrefix, targetElkNode) = ElkEdgirGraphUtils.follow(path, graph.getGraph)
    graph.setSelected(targetElkPrefix.last)

    val (targetDesignPrefix, targetDesignNode) = BlockTreeTableModel.follow(path, designTreeModel)
    designTree.clearSelection()
    val treePath = targetDesignPrefix.tail.foldLeft(new TreePath(targetDesignPrefix.head)) { _.pathByAddingChild(_) }
    designTree.addSelectedPath(treePath)

    ignoreActions = false
  }

  def update(): Unit = {
    if (!compilerRunning.compareAndSet(false, true)) {
      notificationGroup.createNotification(
        s"Compiler already running", NotificationType.WARNING)
          .notify(project)
      return
    }

    ProgressManager.getInstance().run(new Task.Backgroundable(project, "EDG compiling") {
      override def run(indicator: ProgressIndicator): Unit = {
        status.setText(s"Compiling")
        indicator.setIndeterminate(true)

        try {
          indicator.setText("EDG compiling ... reloading")
          EdgCompilerService(project).pyLib.reloadModule(blockModule.getText())

          indicator.setText("EDG compiling ... design top")
          val fullName = blockModule.getText() + "." + blockName.getText()
          val (block, refinements) = EdgCompilerService(project).pyLib.getDesignTop(ElemBuilder.LibraryPath(fullName)).get  // TODO propagate Errorable
          val design = schema.Design(contents = Some(block))

          indicator.setText("EDG compiling ...")
          val (compiled, compiler, time) = EdgCompilerService(project).compile(design, refinements, Some(indicator))

          indicator.setText("EDG compiling ... validating")
          val checker = new DesignStructuralValidate()
          val errors = compiler.getErrors() ++ checker.map(compiled)
          if (errors.isEmpty) {
            status.setText(s"Compiled ($time ms)")
          } else {
            status.setText(s"Compiled, with ${errors.length} errors ($time ms)")
          }
          tabbedPane.setTitleAt(TAB_INDEX_ERRORS, s"Errors (${errors.length})")
          indicator.setText("EDG compiling ... done")

          updateLibrary(EdgCompilerService(project).pyLib)
          refinementsPanel.setRefinements(refinements)
          errorPanel.setErrors(errors)

          setDesign(compiled, compiler)
        } catch {
          case e: Throwable =>
            import java.io.PrintWriter
            import java.io.StringWriter
            val sw = new StringWriter
            e.printStackTrace(new PrintWriter(sw))
            status.setText(s"Compiler error: ${e.toString}")
            notificationGroup.createNotification(
              s"Compiler error", s"${e.toString}",
              s"${sw.toString}",
              NotificationType.WARNING)
                .notify(project)
          // TODO staleness indicator for graph / design tree
        }

        compilerRunning.set(false)
      }
    })

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

      // Update the design tree first, in case graph layout fails
      designTreeModel = new BlockTreeTableModel(block)
      designTree.setModel(designTreeModel)
      designTree.getTree.addTreeSelectionListener(designTreeListener)  // this seems to get overridden when the model is updated

      // TODO layout happens in background task?
      val edgirGraph = EdgirGraph.blockToNode(DesignPath(), block)
      val transformedGraph = CollapseBridgeTransform(CollapseLinkTransform(
        InferEdgeDirectionTransform(SimplifyPortTransform(
          PruneDepthTransform(edgirGraph, depthSpinner.getNumber)))))  // TODO configurable depth
      val layoutGraphRoot = HierarchyGraphElk.HGraphNodeToElk(transformedGraph,
        Some(ElkEdgirGraphUtils.DesignPathMapper))

      graph.setGraph(layoutGraphRoot)

      // TODO this should resolve as far as possible here, instead of passing a newly invalid path
      select(selectedPath)  // reload previous selection to the extent possible
    case None => graph.setGraph(emptyHGraph)
  }

  def updateLibrary(library: PythonInterfaceLibrary): Unit = {
    libraryPanel.setLibrary(library)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.panelBlockFile = blockFile.getText
    state.panelBlockModule = blockModule.getText()
    state.panelBlockName = blockName.getText()
    state.depthSpinner = depthSpinner.getNumber
    state.panelMainSplitterPos = mainSplitter.getProportion
    state.panelBottomSplitterPos = bottomSplitter.getProportion
    state.panelTabIndex = tabbedPane.getSelectedIndex
    libraryPanel.saveState(state)
    refinementsPanel.saveState(state)
    detailPanel.saveState(state)
    errorPanel.saveState(state)
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    blockFile.setText(state.panelBlockFile)
    blockModule.setText(state.panelBlockModule)
    blockName.setText(state.panelBlockName)
    depthSpinner.setNumber(state.depthSpinner)
    mainSplitter.setProportion(state.panelMainSplitterPos)
    bottomSplitter.setProportion(state.panelBottomSplitterPos)
    tabbedPane.setSelectedIndex(state.panelTabIndex)
    libraryPanel.loadState(state)
    refinementsPanel.loadState(state)
    detailPanel.loadState(state)
    errorPanel.loadState(state)
  }
}


class BlockPopupMenu(node: EdgirLibraryTreeNode.BlockNode, project: Project) extends JPopupMenu {
  add(new JLabel(EdgirUtils.SimpleLibraryPath(node.path)))

  private val pyPsi = PyPsiFacade.getInstance(project)
  private val pyClass = Errorable(pyPsi.findClass(node.path.getTarget.getName), "no class")
  private val pyNavigatable = pyClass.require("class not navigatable")(_.canNavigateToSource)

  private val psiFile = pyClass.map(_.getContainingFile)
  private val psiDocumentManager = PsiDocumentManager.getInstance(project)
  private val psiDocument = psiFile.map("no document")(psiDocumentManager.getDocument(_))
  private val fileLine = (pyClass + (psiFile + psiDocument)).mapToString { case (pyClass, (psiFile, psiDocument)) =>
    val lineNumber = psiDocument.getLineNumber(pyClass.getTextOffset)
    s"${psiFile.getName}:$lineNumber"
  }

  private val gotoItem = pyNavigatable match {
    case Errorable.Success(pyNavigatable) =>
      val item = new JMenuItem(s"Goto Definition (${fileLine})")
      item.addActionListener((e: ActionEvent) => {
        pyNavigatable.navigate(true)
      })
      item
    case Errorable.Error(msg) =>
      val item = new JMenuItem(s"Goto Definition ($msg)")
      item.setEnabled(false)
      item
  }
  add(gotoItem)

  val item = new JMenuItem("Inheritor Search")
  item.addActionListener((e: ActionEvent) => {
    val inheritors = pyClass.map { pyClass =>
      val results = PyClassInheritorsSearch.search(pyClass, false).findAll().asScala
      results.map(_.getName).mkString(", ")
    }
    println(inheritors)
  })
  add(item)
}


class LibraryPanel(project: Project) extends JPanel {
  // State
  //
  private var library: wir.Library = EdgCompilerService(project).pyLib

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  private val visualizer = new JBTextArea("TODO Library Visualizer here")
  splitter.setSecondComponent(visualizer)

  private val libraryTree = new TreeTable(new EdgirLibraryTreeTableModel(library))
  new TreeTableSpeedSearch(libraryTree)
  private val libraryTreeListener = new TreeSelectionListener {  // an object so it can be re-used since a model change wipes everything out
    override def valueChanged(e: TreeSelectionEvent): Unit = {
      e.getPath.getLastPathComponent match {
        case node: EdgirLibraryTreeNode.BlockNode =>
          visualizer.setText(
            s"${EdgirUtils.SimpleLibraryPath(node.path)}\n" +
            s"Superclasses: ${node.block.superclasses.map{EdgirUtils.SimpleLibraryPath}.mkString(", ")}"
          )
        case node =>
      }
    }
  }
  private val libraryMouseListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      super.mousePressed(e)
      val selectedPath = libraryTree.getTree.getPathForLocation(e.getX, e.getY)
      if (selectedPath == null) {
        return
      }
      val selected = selectedPath.getLastPathComponent
      if (selected.isInstanceOf[EdgirLibraryTreeNode.BlockNode]) {
        val selectedNode = selected.asInstanceOf[EdgirLibraryTreeNode.BlockNode]
        val menu = new BlockPopupMenu(selectedNode, project)
        menu.show(e.getComponent, e.getX, e.getY)
      }
    }
  }
  libraryTree.getTree.addTreeSelectionListener(libraryTreeListener)
  libraryTree.addMouseListener(libraryMouseListener)
  libraryTree.setRootVisible(false)
  libraryTree.setShowColumns(true)
  private val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  splitter.setFirstComponent(libraryTreeScrollPane)

  setLayout(new BorderLayout())
  add(splitter)

  // Actions
  //
  def setLibrary(library: wir.Library): Unit = {
    this.library = library
    libraryTree.setModel(new EdgirLibraryTreeTableModel(this.library))
    libraryTree.getTree.addTreeSelectionListener(libraryTreeListener)
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


class RefinementsPanel extends JPanel {
  private val tree = new TreeTable(new RefinementsTreeTableModel(edgrpc.Refinements()))
  new TreeTableSpeedSearch(tree)
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)

  // Actions
  //
  def setRefinements(refinements: edgrpc.Refinements): Unit = {
    tree.setModel(new RefinementsTreeTableModel(refinements))
    tree.setRootVisible(false)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
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
  tree.setRootVisible(false)
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
