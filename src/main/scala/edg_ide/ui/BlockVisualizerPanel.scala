package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import com.intellij.ui.{JBIntSpinner, JBSplitter, TreeTableSpeedSearch}
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.compiler.{Compiler, CompilerError, DesignStructuralValidate, PythonInterfaceLibrary, hdl => edgrpc}
import edg.elem.elem
import edg.schema.schema
import edg.ElemBuilder
import edg.util.Errorable
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, ElkEdgirGraphUtils, HierarchyGraphElk, InferEdgeDirectionTransform, PruneDepthTransform, SimplifyPortTransform}
import edg_ide.swing.{BlockTreeTableModel, CompilerErrorTreeTableModel, JElkGraph, RefinementsTreeTableModel, ZoomingScrollPane}
import edg.wir.DesignPath
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.build.BuildInfo
import edg_ide.util.ErrorableNotify.ErrorableNotify
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}
import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}

import java.awt.event.{ActionEvent, ActionListener, MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.TreePath
import javax.swing.{JButton, JLabel, JMenuItem, JPanel, JPopupMenu, SwingUtilities}


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


class DesignBlockPopupMenu(path: DesignPath, design: schema.Design, project: Project) extends JPopupMenu {
  private val block = Errorable(EdgirUtils.resolveBlockFromBlock(path, design.getContents), "no block at path")
  private val blockClass = block.map(_.superclasses).require("invalid class")(_.length == 1)
      .map(_.head)

  add(new JLabel(s"${blockClass.mapToString(EdgirUtils.SimpleLibraryPath)} at $path"))

  DesignAnalysisUtils.allAssignsTo(path, design, project) match {
    case Errorable.Error(msg) =>
      val errorItem = new JMenuItem(s"Goto Instantiation ($msg)")
      errorItem.setEnabled(false)
      add(errorItem)
    case Errorable.Success(assigns) => assigns.foreach { assign =>
      val fileLine = PsiUtils.fileLineOf(assign, project).mapToString(identity)
      val gotoInstantiationItem = new JMenuItem(s"Goto Instantiation ($fileLine)")
      gotoInstantiationItem.addActionListener((e: ActionEvent) => {
        assign.navigate(true)
      })
      add(gotoInstantiationItem)
    }
  }

  private val pyClass = blockClass.flatMap(DesignAnalysisUtils.pyClassOf(_, project))
  private val pyNavigatable = pyClass.require("class not navigatable")(_.canNavigateToSource)

  private val fileLine = pyClass.flatMap(PsiUtils.fileLineOf(_, project)).mapToString(identity)
  val gotoDefinitionItem = new JMenuItem(s"Goto Definition (${fileLine})")
  pyNavigatable match {
    case Errorable.Success(pyNavigatable) =>
      gotoDefinitionItem.addActionListener((e: ActionEvent) => {
        pyNavigatable.navigate(true)
      })
    case Errorable.Error(msg) =>
      gotoDefinitionItem.setEnabled(false)
  }
  add(gotoDefinitionItem)

  // TODO add goto parent / goto root if selected current focus?

  val setFocusItem = new JMenuItem(s"Focus View on $path")
  setFocusItem.addActionListener((e: ActionEvent) => {
    BlockVisualizerService(project).setContext(path)
  })
  add(setFocusItem)
}


class BlockVisualizerPanel(val project: Project) extends JPanel {
  // Internal State
  //
  private var design = schema.Design()
  private var compiler = new Compiler(design, EdgCompilerService(project).pyLib)

  private var blockModule = ""
  private var blockName = ""

  // GUI-facing state
  //
  private var focusPath: DesignPath = DesignPath()  // visualize from root by default
  private var selectedPath: DesignPath = DesignPath()  // root implicitly selected by default
  // This hack ignores actions when programmatically synchronizing the design tree and graph
  // TODO refactor w/ shared model eg https://docs.oracle.com/javase/tutorial/uiswing/examples/components/index.html#SharedModelDemo
  private var ignoreActions: Boolean = false
  private val compilerRunning = new AtomicBoolean(false)

  // GUI Components
  //
  private val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.ui.BlockVisualizerPanel")

  private val mainSplitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)

  // GUI: Top half (status and block visualization)
  //
  private val visualizationPanel = new JPanel(new GridBagLayout())
  mainSplitter.setFirstComponent(visualizationPanel)

  private val blockNameLabel = new JLabel("")
  visualizationPanel.add(blockNameLabel, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  private val button = new JButton("Update")
  visualizationPanel.add(button, Gbc(1, 0, GridBagConstraints.HORIZONTAL))
  button.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      update()
    }
  })

  // TODO max value based on depth of tree?
  private val depthSpinner = new JBIntSpinner(1, 1, 8)
  // TODO update visualization on change?
  visualizationPanel.add(depthSpinner, Gbc(2, 0, GridBagConstraints.HORIZONTAL))

  private val status = new JLabel(s"Ready " +
      s"(version ${BuildInfo.version} built at ${BuildInfo.builtAtString}, " +
      s"scala ${BuildInfo.scalaVersion}, sbt ${BuildInfo.sbtVersion})"
  )
  visualizationPanel.add(status, Gbc(0, 1, GridBagConstraints.HORIZONTAL, xsize=3))

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
  graph.addMouseListener(new MouseAdapter {  // right click context menu
    override def mouseClicked(e: MouseEvent): Unit = {
      if (ignoreActions) {
        return
      }
      if (!SwingUtilities.isRightMouseButton(e) || e.getClickCount != 1) {
        return
      }
      val clickedNode = graph.getElementForLocation(e.getX, e.getY)
      clickedNode match {
        case Some(node: ElkNode) =>
          val path = Errorable(node.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property), "node missing path")
          path.mapOrNotify(notificationGroup, project) { path =>
            val menu = new DesignBlockPopupMenu(path, design, project)
            menu.show(e.getComponent, e.getX, e.getY)
          }
        case _ =>  // ignored
      }
    }
  })
  graph.addMouseListener(new MouseAdapter { // double click navigate to source
    override def mouseClicked(e: MouseEvent): Unit = {
      if (!SwingUtilities.isLeftMouseButton(e) || e.getClickCount != 2) {
        return
      }
      val clickedNode = graph.getElementForLocation(e.getX, e.getY)
      clickedNode match {
        case Some(node: ElkNode) => exceptable {
            val blockPath = node.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property)
                .exceptNull("node missing path")
            DesignAnalysisUtils.allAssignsTo(blockPath, design, project).exceptError
          }.mapOrNotify(notificationGroup, project) {
            _.head.navigate(true)
          }
        case None =>  // ignored
      }
    }
  })

  private val graphScrollPane = new JBScrollPane(graph) with ZoomingScrollPane
  visualizationPanel.add(graphScrollPane, Gbc(0, 2, GridBagConstraints.BOTH, xsize=3))

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
  def getModule: String = blockModule

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

  def setContext(path: DesignPath): Unit = {

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
          EdgCompilerService(project).pyLib.reloadModule(blockModule)

          indicator.setText("EDG compiling ... design top")
          val fullName = blockModule + "." + blockName
          val designPathTop = ElemBuilder.LibraryPath(fullName)
          val (block, refinements) = EdgCompilerService(project).pyLib.getDesignTop(designPathTop).get  // TODO propagate Errorable
          val design = schema.Design(contents = Some(block.copy(superclasses = Seq(designPathTop))))  // TODO dedup w/ superclass resolution in BlockLink.Block

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

  def setFileBlock(module: String, block: String): Unit = {
    blockModule = module
    blockName = block
    blockNameLabel.setText(s"$module.$blockName")
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
    state.panelBlockModule = blockModule
    state.panelBlockName = blockName
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
    setFileBlock(state.panelBlockModule, state.panelBlockName)
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
