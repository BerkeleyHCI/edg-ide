package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg_ide.dse.DseConfigElement

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


trait SeqNodeBase {
  val children: Seq[SeqNodeBase]
  def getColumns(index: Int): String
}


object DseConfigTreeNode {
  class Root(configs: Seq[DseConfigElement]) extends SeqNodeBase {

  }
}



class DseConfigTreeTableModel(configs: Seq[DseConfigElement]) extends SeqTreeTableModel[SeqNodeBase] {
  val rootNode: SeqNodeBase = new DseConfigTreeNode.Root(configs)
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
