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


class EdgTreeTableModel(root: HierarchyBlock) extends TreeTableModel {
  val logger = Logger.getInstance(classOf[EdgTreeTableModel])
  val rootNode = new HierarchyBlockNode("(design)", root)
  val COLUMNS = Seq("Path", "Class")

  // TreeView abstract methods
  //
  override def getRoot: Object = rootNode

  override def isLeaf(node: Object): Boolean = node match {
    case node: HierarchyBlockNode => node.getChildren().isEmpty
    case _ =>
      logger.error(s"isLeaf got node of unexpected type ${node.getClass}")
      true
  }

  override def getChildCount(node: Object): Int = node match {
    case node: HierarchyBlockNode => node.getChildren().length
    case _ =>
      logger.error(s"getChildCount got node of unexpected type ${node.getClass}")
      0
  }

  override def getChild(parent: Object, index: Int): Object = parent match {
    case parent: HierarchyBlockNode =>
      val children = parent.getChildren()
      if (index < children.size) {
        parent.getChildren()(index)
      } else {
        null
      }
    case _ =>
      logger.error(s"getChild got parent of unexpected type ${parent.getClass}")
      null
  }

  override def getIndexOfChild(parent: Object, child: Object): Int = parent match {
    case parent: HierarchyBlockNode => parent.getChildren().indexOf(child)
    case _ =>
      logger.error(s"getIndexOfChild got parent of unexpected type ${parent.getClass}")
      -1
  }

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

  override def getValueAt(node: Object, column: Int): Object = node match {
    case node: HierarchyBlockNode => node.getColumns()(column)
    case _ =>
      logger.error(s"getValueAt got node of unexpected type ${node.getClass}")
      null
  }

  // These aren't relevant for trees that can't be edited
  override def isCellEditable(node: Object, column: Int): Boolean = false
  override def setValueAt(aValue: Any, node: Object, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { logger.warn(s"setTree $tree") }  // idk what this is
}
