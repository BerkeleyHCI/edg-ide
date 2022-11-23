package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.{TreeTable, TreeTableModel}
import edg.EdgirUtils.SimpleLibraryPath
import edg_ide.dse.{DseConfigElement, DseObjective, DseObjectiveFootprintArea, DseObjectiveFootprintCount, DseObjectiveParameter, DseParameterSearch, DseSubclassSearch}

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath
import scala.collection.{SeqMap, mutable}


sealed trait SeqNodeBase {
  val children: Seq[SeqNodeBase]
  def getColumns(index: Int): String
}


object DseConfigTreeNode {
  sealed trait DseConfigTreeNode extends SeqNodeBase {
    val path: String
    val value: String

    override def getColumns(index: Int): String = index match {
      case 1 => value
      case _ => "???"
    }
    override def toString = path
  }

  class LeafNode(val path: String, val value: String) extends DseConfigTreeNode {
    override val children: Seq[SeqNodeBase] = Seq()
  }

  class Root(configs: Seq[DseConfigElement], objectives: SeqMap[String, DseObjective[Any]]) extends DseConfigTreeNode {
    override lazy val children = Seq(
      new SearchConfigs(configs),
      new Objectives(objectives)
    )
    override val path = ""
    override val value = ""
  }

  class SearchConfigs(configs: Seq[DseConfigElement]) extends DseConfigTreeNode {
    override lazy val children = configs.map {
      case config: DseParameterSearch => new DseParameterSearchNode(config)
      case config: DseSubclassSearch => new DseSubclassSearchNode(config)
    }
    override val path = "Search Configs"
    override val value = ""
  }

  class Objectives(objectives: SeqMap[String, DseObjective[Any]]) extends DseConfigTreeNode {
    override lazy val children =  objectives.map { case (name, objective) => objective match {
      case config: DseObjectiveParameter => new DseObjectiveParameterNode(name, config)
      case config: DseObjectiveFootprintArea => new DseObjectiveFootprintAreaNode(name, config)
      case config: DseObjectiveFootprintCount => new DseObjectiveFootprintCountNode(name, config)
    } }.toSeq
    override val path = "Objective Functions"
    override val value = ""
  }

  sealed trait DseSearchConfigNode extends DseConfigTreeNode {
    def config: DseConfigElement
  }

  class DseParameterSearchNode(val config: DseParameterSearch) extends DseSearchConfigNode {
    override val path = config.path.toString
    override val value = f"Parameters (${config.values.length})"
    override lazy val children = config.values.map { value =>
      new LeafNode("", value.toStringValue)
    }
  }

  class DseSubclassSearchNode(val config: DseSubclassSearch) extends DseSearchConfigNode {
    override val path = config.path.toString
    override val value = f"Subclasses (${config.subclasses.length})"
    override lazy val children = config.subclasses.map { subclass =>
      new LeafNode("", subclass.toSimpleString)
    }
  }

  sealed trait DseObjectiveNode extends DseConfigTreeNode {
    def config: DseObjective[Any]
  }

  class DseObjectiveParameterNode(name: String, val config: DseObjectiveParameter) extends DseObjectiveNode {
    override val path = name
    override val value = f"Parameter @ ${config.path}"
    override lazy val children = Seq()
  }

  class DseObjectiveFootprintAreaNode(name: String, val config: DseObjectiveFootprintArea) extends DseObjectiveNode {
    override val path = name
    override val value = f"Footprint Area in ${config.rootPath}"
    override lazy val children = Seq()
  }

  class DseObjectiveFootprintCountNode(name: String, val config: DseObjectiveFootprintCount) extends DseObjectiveNode {
    override val path = name
    override val value = f"Footprint Count in ${config.rootPath}"
    override lazy val children = Seq()
  }
}


// Utility methods to preserve user-visible state when the tree model is updated.
// The clean solution would be to use TreeModelListener events, but these don't seem to behave
// when the underlying node structure is modified, even if it's mostly isomorphic.
object TreeUpdateUtils {
  def saveAndRestore(treeTable: TreeTable)(modifyAction: => Unit): Unit = {
    val savedNodes = getExpandedNodes(treeTable.getTree)
    val isRootVisible = treeTable.getTree.isRootVisible
    modifyAction  // note that setModel resets TreeTable.getTree, so we need to get a fresh tree handle
    restoreExpandedNodes(treeTable.getTree, savedNodes)
    treeTable.setRootVisible(isRootVisible)

  }

  // Returns the tree path of all expanded nodes (as sequence of node objects).
  protected def getExpandedNodes(tree: JTree): Seq[TreePath] = {
    val expandedNodes = mutable.ListBuffer[TreePath]()
    val model = tree.getModel

    def traverse(path: TreePath): Unit = {
      val thisNode = path.getLastPathComponent
      if (tree.isExpanded(path)) {  // only recurse if expanded
        expandedNodes.append(path)
        (0 until model.getChildCount(thisNode)).map(model.getChild(thisNode, _)) foreach { childNode =>
          val childPath = path.pathByAddingChild(childNode)
          traverse(childPath)
        }
      }
    }
    traverse(new TreePath(model.getRoot))

    expandedNodes.toSeq
  }

  // Restores expanded node state, by walking the tree and comparing node approximate-equality.
  protected def restoreExpandedNodes(tree: JTree, expanded: Seq[TreePath]): Unit = {
    // This is pretty computationally inefficient. Storing the expanded nodes in a tree structure can be
    // much more efficient, but this is the lazy standard-classes based solution.
    val model = tree.getModel

    // Checks whether to expand the node at treePath, then recursively calls with filtered expandedPaths for each child
    def traverse(treePath: TreePath, expandedPaths: Seq[TreePath]): Unit = {
      val treeNode = treePath.getLastPathComponent
      val expandedByThisNode = expandedPaths.groupBy(_.getPathComponent(treePath.getPathCount - 1))

      expandedByThisNode foreach { case (expandedNode, expandedPaths) =>
        // while it's theoretically possible to get multiple expandedNode matches this is probably unlikely
        if (treeNode.toString == expandedNode.toString) {
          tree.expandPath(treePath)
          val treeNodeChildren = (0 until model.getChildCount(treeNode)).map(model.getChild(treeNode, _))
          treeNodeChildren.foreach { childNode =>
            val childExpandedPaths = expandedPaths.filter(_.getPathCount > treePath.getPathCount)
            traverse(treePath.pathByAddingChild(childNode), childExpandedPaths)
          }
        }
      }
    }
    traverse(new TreePath(model.getRoot), expanded)
  }
}


class DseConfigTreeTableModel(searchConfigs: Seq[DseConfigElement], objectives: SeqMap[String, DseObjective[Any]])
    extends SeqTreeTableModel[SeqNodeBase] {
  val rootNode: SeqNodeBase = new DseConfigTreeNode.Root(searchConfigs, objectives)
  val COLUMNS = Seq("Path", "Value")

  // TreeView abstract methods
  //
  override def getRootNode: SeqNodeBase = rootNode

  override def getNodeChildren(node: SeqNodeBase): Seq[SeqNodeBase] = node.children

  // These aren't relevant for trees that can't be edited
  override def valueForPathChanged(path: TreePath, newValue: Any): Unit = {}
  override def addTreeModelListener(l: TreeModelListener): Unit = {}
  override def removeTreeModelListener(l: TreeModelListener): Unit = {}

  // TreeTableView abstract methods
  //
  override def getColumnCount: Int = COLUMNS.length

  override def getColumnName(column: Int): String = COLUMNS(column)

  override def getColumnClass(column: Int): Class[_] = column match {
    case 0 => classOf[TreeTableModel]
    case _ => classOf[String]
  }

  override def getNodeValueAt(node: SeqNodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: SeqNodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: SeqNodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
