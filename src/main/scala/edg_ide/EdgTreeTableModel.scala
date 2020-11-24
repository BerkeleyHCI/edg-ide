package edg_ide

import com.intellij.openapi.diagnostic.Logger
import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.elem.elem.{BlockLike, HierarchyBlock}

import javax.swing.tree._
import javax.swing.event.TreeModelListener
import javax.swing.JTree


class HierarchyBlockNode(val name: String, val block: HierarchyBlock) {
  import edg.elem.elem.BlockLike

  lazy val children = block.blocks.map { case (name, subblock) =>
    (name, subblock.`type`)
  }.collect {
    case (name, BlockLike.Type.Hierarchy(subblock)) => new HierarchyBlockNode(name, subblock)
  }.toSeq

  def getChildren(): Seq[HierarchyBlockNode] = children  // must be deterministic

  override def equals(other: Any): Boolean = other match {
    case other: HierarchyBlockNode => other.block == block
    case _ => false
  }

  override def toString(): String = getColumns()(0)

  def getColumns(): Seq[String] = Seq(
    name,
    block.superclasses.map { EdgirUtils.LibraryPathToString(_) }.mkString(", ")
  )
}


class EdgTreeTableModel(root: HierarchyBlock) extends ParameterizedTreeTableModel[HierarchyBlockNode] {
  val rootNode: HierarchyBlockNode = new HierarchyBlockNode("(design)", root)
  val COLUMNS = Seq("Path", "Class")

  // TreeView abstract methods
  //
  override def getRoot: HierarchyBlockNode = rootNode

  override def isLeaf(node: HierarchyBlockNode): Boolean = node.getChildren().isEmpty
  override def getChildCount(node: HierarchyBlockNode): Int = node.getChildren().length
  override def getChild(parent: HierarchyBlockNode, index: Int): HierarchyBlockNode = {
    val children = parent.getChildren()
    if (index < children.size) {
      parent.getChildren()(index)
    } else {
      null
    }
  }

  override def getIndexOfChild(parent: HierarchyBlockNode, child: HierarchyBlockNode): Int =
    parent.getChildren().indexOf(child)

  // These aren't relevant for trees that can't be edited
  override def valueForPathChanged(path: TreePath, newValue: Any): Unit = {}
  override def addTreeModelListener(l: TreeModelListener): Unit = {}
  override def removeTreeModelListener(l: TreeModelListener): Unit = {}

  // TreeTableView abstract methods
  //
  override def getColumnCount: Int = COLUMNS.length

  override def getColumnName(column: Int): String = COLUMNS(column)

  override def getColumnClass(column: Int): Class[_] = {
    if (column == 0) {
      return classOf[TreeTableModel]
    } else {
      return classOf[String]
    }
  }

  override def getValueAt(node: HierarchyBlockNode, column: Int): Object = node.getColumns()(column)

  // These aren't relevant for trees that can't be edited
  override def isCellEditable(node: HierarchyBlockNode, column: Int): Boolean = false
  override def setValueAt(aValue: Any, node: HierarchyBlockNode, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
