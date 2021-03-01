package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.{JBSplitter, TreeTableSpeedSearch}
import com.intellij.ui.components.{JBScrollPane, JBTextArea}
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.wir
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.swing.{EdgirLibraryTreeNode, EdgirLibraryTreeTableModel}
import edg_ide.util.DesignAnalysisUtils

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, MouseAdapter, MouseEvent}
import javax.swing.{JLabel, JMenuItem, JPanel, JPopupMenu, SwingUtilities}
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import scala.jdk.CollectionConverters.CollectionHasAsScala


class LibraryBlockPopupMenu(node: EdgirLibraryTreeNode.BlockNode, project: Project) extends JPopupMenu {
  add(new JLabel(EdgirUtils.SimpleLibraryPath(node.path)))

  private val pyClass = DesignAnalysisUtils.pyClassOf(node.path, project)
  private val pyNavigatable = pyClass.require("class not navigatable")(_.canNavigateToSource)

  private val fileLine = pyNavigatable.flatMap(PsiUtils.fileLineOf(_, project)).mapToString(identity)
  val gotoDefinitionItem = PopupMenuUtils.MenuItemFromErrorable(pyNavigatable,
    s"Goto Definition (${fileLine})") { pyNavigatable =>
    pyNavigatable.navigate(true)
  }
  add(gotoDefinitionItem)

  // TODO temporary item remove me
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
  libraryTree.getTree.addTreeSelectionListener(libraryTreeListener)
  private val libraryMouseListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      if (!SwingUtilities.isRightMouseButton(e) || e.getClickCount != 1) {
        return
      }

      val selectedPath = libraryTree.getTree.getPathForLocation(e.getX, e.getY)
      if (selectedPath == null) {
        return
      }
      selectedPath.getLastPathComponent match {
        case selected: EdgirLibraryTreeNode.BlockNode =>
          val menu = new LibraryBlockPopupMenu(selected, project)
          menu.show(e.getComponent, e.getX, e.getY)
        case _ =>  // any other type ignored
      }
    }
  }
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
