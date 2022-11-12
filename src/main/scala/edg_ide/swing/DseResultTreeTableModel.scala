package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg_ide.dse.DseResult

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


object DseResultTreeNode {
  trait NodeBase {
    val children: Seq[NodeBase]

    def getColumns(index: Int): String
  }

  class RootNode(results: Seq[DseResult]) extends NodeBase {

  }

  class ResultSetNode(setMembers: Seq[DseResult]) extends NodeBase {

  }

  class ResultNode(result: DseResult) extends NodeBase {

  }
}


class DseResultTreeTableModel(results: Seq[DseResult])
    extends SeqTreeTableModel[DseResultTreeNode.NodeBase] {
  val rootNode: DseResultTreeNode.NodeBase = new DseResultTreeNode.RootNode(results)
  val COLUMNS = Seq("Config", "Errors", "Values")

  // TreeView abstract methods
  //
  override def getRootNode: DseResultTreeNode.NodeBase = rootNode

  override def getNodeChildren(node: DseResultTreeNode.NodeBase): Seq[DseResultTreeNode.NodeBase] = node.children

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

  override def getNodeValueAt(node: DseResultTreeNode.NodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: DseResultTreeNode.NodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: DseResultTreeNode.NodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
