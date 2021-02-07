package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.elem.elem
import edg.wir.{DesignPath, MapSort, ProtoUtil}
import edg_ide.EdgirUtils

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._


class HierarchyBlockNode(val path: DesignPath, val block: elem.HierarchyBlock) {
  import edg.elem.elem.BlockLike

  lazy private val nameOrder = ProtoUtil.getNameOrder(block.meta)
  lazy val children: Seq[HierarchyBlockNode] = MapSort(block.blocks.map { case (name, subblock) =>
    (name, subblock.`type`)
  }, nameOrder).collect {
    case (name, BlockLike.Type.Hierarchy(subblock)) => new HierarchyBlockNode(path + name, subblock)
  }.toSeq

  override def equals(other: Any): Boolean = other match {
    case other: HierarchyBlockNode => other.block == block
    case _ => false
  }

  override def toString: String = path.steps match {
    case Seq() => ""
    case steps => steps.last
  }

  def getColumns(index: Int): String = block.superclasses.map(EdgirUtils.LibraryPathToString).mkString(", ")
}


class BlockTreeTableModel(root: elem.HierarchyBlock) extends SeqTreeTableModel[HierarchyBlockNode] {
  val rootNode: HierarchyBlockNode = new HierarchyBlockNode(DesignPath.root, root)
  val COLUMNS = Seq("Path", "Class")

  // TreeView abstract methods
  //
  override def getRootNode: HierarchyBlockNode = rootNode

  override def getNodeChildren(node: HierarchyBlockNode): Seq[HierarchyBlockNode] = node.children

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
