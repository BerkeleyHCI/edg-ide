package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.elem.elem.HierarchyBlock
import edg_ide.EdgirUtils

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._


class HierarchyBlockNode(val name: String, val block: HierarchyBlock) {
  import edg.elem.elem.BlockLike

  lazy protected val children: Seq[HierarchyBlockNode] = block.blocks.map { case (name, subblock) =>
    (name, subblock.`type`)
  }.collect {
    case (name, BlockLike.Type.Hierarchy(subblock)) => new HierarchyBlockNode(name, subblock)
  }.toSeq

  def getChildren: Seq[HierarchyBlockNode] = children  // must be deterministic

  override def equals(other: Any): Boolean = other match {
    case other: HierarchyBlockNode => other.block == block
    case _ => false
  }

  override def toString: String = getColumns.head

  def getColumns: Seq[String] = Seq(
    name,
    block.superclasses.map(EdgirUtils.LibraryPathToString).mkString(", ")
  )
}


class BlockTreeTableModel(root: HierarchyBlock) extends SeqTreeTableModel[HierarchyBlockNode] {
  val rootNode: HierarchyBlockNode = new HierarchyBlockNode("(design)", root)
  val COLUMNS = Seq("Path", "Class")

  // TreeView abstract methods
  //
  override def getRootNode: HierarchyBlockNode = rootNode

  override def getNodeChildren(node: HierarchyBlockNode): Seq[HierarchyBlockNode] = node.getChildren

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

  override def getNodeValueAt(node: HierarchyBlockNode, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: HierarchyBlockNode, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: HierarchyBlockNode, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
