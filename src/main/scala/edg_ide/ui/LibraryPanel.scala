package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.{JBSplitter, TreeTableSpeedSearch}
import com.intellij.ui.components.{JBScrollPane, JBTextArea}
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.wir
import edg.ref.ref
import edg.util.Errorable
import edg_ide.actions.{InsertAction, InsertBlockAction}
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.swing.{EdgirLibraryTreeNode, EdgirLibraryTreeTableModel}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionPopup, requireExcept}

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, MouseAdapter, MouseEvent}
import javax.swing.{JLabel, JMenuItem, JPanel, JPopupMenu, SwingUtilities}
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import scala.jdk.CollectionConverters.CollectionHasAsScala


class LibraryBlockPopupMenu(path: ref.LibraryPath, project: Project) extends JPopupMenu {
  val libName = EdgirUtils.SimpleLibraryPath(path)
  add(new JLabel(s"Library Block: $libName"))
  addSeparator()

  private val libPyClass = DesignAnalysisUtils.pyClassOf(path, project)
  private val contextPyClass = InsertAction.getPyClassOfContext(project)
  private val contextPyName = contextPyClass.mapToString(_.getName)

  // Edit actions
  private val caretPsiElement = exceptable {
    val contextPsiFile = contextPyClass.exceptError.getContainingFile.exceptNull("no file")
    InsertAction.getCaretAtFile(contextPsiFile, contextPyClass.exceptError, project).exceptError
  }
  val caretInsertAction: Errorable[() => Unit] = exceptable {
    InsertBlockAction.createInsertBlockFlow(caretPsiElement.exceptError, libPyClass.exceptError,
        s"Insert $libName at $contextPyName caret",
        project, InsertAction.navigateElementFn).exceptError
  }
  private val caretFileLine = exceptable {
    caretInsertAction.exceptError
    PsiUtils.fileNextLineOf(caretPsiElement.exceptError, project).exceptError
  }.mapToStringOrElse(fileLine => s" ($fileLine)", err => "")

  private val caretInsertItem = PopupMenuUtils.MenuItemFromErrorable(
    caretInsertAction, s"Insert at $contextPyName caret$caretFileLine")
  add(caretInsertItem)

  private val insertionPairs = exceptable {
    InsertAction.findInsertionPoints(contextPyClass.exceptError, project).exceptError
        .map { fn =>
          val fileLine = PsiUtils.fileNextLineOf(fn.getStatementList.getLastChild, project)
              .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
          val label = s"Insert at ${contextPyName}.${fn.getName}$fileLine"
          val action = InsertBlockAction.createInsertBlockFlow(fn.getStatementList.getStatements.last, libPyClass.exceptError,
            s"Insert $libName at $contextPyName.${fn.getName}",
            project, InsertAction.navigateElementFn)
          (label, action)
        } .collect {
          case (fn, Errorable.Success(action)) => (fn, action)
        }.exceptEmpty("no insertion points")
  }
  PopupMenuUtils.MenuItemsFromErrorableSeq(insertionPairs, s"Insert into $contextPyName")
      .foreach(add)
  addSeparator()

  // Navigation actions
  val gotoDefinitionAction: Errorable[() => Unit] = exceptable {
    requireExcept(libPyClass.exceptError.canNavigateToSource, "class not navigatable")
    () => libPyClass.exceptError.navigate(true)
  }
  private val libFileLine = libPyClass.flatMap(PsiUtils.fileLineOf(_, project))
      .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val gotoDefinitionItem = PopupMenuUtils.MenuItemFromErrorable(gotoDefinitionAction,
    s"Goto Definition$libFileLine")
  add(gotoDefinitionItem)
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
      val selectedTreePath = libraryTree.getTree.getPathForLocation(e.getX, e.getY)
      if (selectedTreePath == null) {
        return
      }
      val selectedPath = selectedTreePath.getLastPathComponent match {
        case selected: EdgirLibraryTreeNode.BlockNode => selected.path
        case _ => return  // any other type ignored
      }

      if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
        // double click quick insert at caret
        exceptionPopup(e) {
          (new LibraryBlockPopupMenu(selectedPath, project).caretInsertAction.exceptError)()
        }
      } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
        // right click context menu
        new LibraryBlockPopupMenu(selectedPath, project).show(e.getComponent, e.getX, e.getY)
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
