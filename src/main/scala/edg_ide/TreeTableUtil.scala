package edg_ide

import scala.reflect.ClassTag

import com.intellij.openapi.diagnostic.Logger
import com.intellij.ui.treeStructure.treetable.TreeTableModel

import javax.swing.tree._
import javax.swing.event.TreeModelListener
import javax.swing.JTree


abstract class ParameterizedTreeTableModel[NodeType <: Object](implicit tag: ClassTag[NodeType]) { outer =>
  val logger = Logger.getInstance(classOf[EdgTreeTableModel])

  // TreeView abstract methods
  //
  def getRoot: NodeType
  def isLeaf(node: NodeType): Boolean
  def getChildCount(node: NodeType): Int
  def getChild(parent: NodeType, index: Int): NodeType
  def getIndexOfChild(parent: NodeType, child: NodeType): Int

  def valueForPathChanged(path: TreePath, newValue: Any): Unit
  def addTreeModelListener(l: TreeModelListener): Unit
  def removeTreeModelListener(l: TreeModelListener): Unit

  // TreeTableView abstract methods
  //
  def getColumnCount(): Int
  def getColumnName(column: Int): String
  def getColumnClass(column: Int): Class[_]

  def getValueAt(node: NodeType, column: Int): Object

  def isCellEditable(node: NodeType, column: Int): Boolean = false
  def setValueAt(aValue: Any, node: NodeType, column: Int): Unit = {}

  def setTree(tree: JTree): Unit

  // Actual interface adapter here
  //
  val treeModel = new TreeTableModel {
    // TreeView abstract methods
    //
    override def getRoot: Object = outer.getRoot

    override def isLeaf(node: Object): Boolean = node match {
      case node: NodeType => outer.isLeaf(node)
      case null =>
        logger.error(s"isLeaf got node of unexpected null")
        true
      case None =>
        logger.error(s"isLeaf got node of unexpected type ${node.getClass}")
        true
    }

    override def getChildCount(node: Object): Int = node match {
      case node: NodeType => outer.getChildCount(node)
      case null =>
        logger.error(s"getChildCount got node of unexpected null")
        0
      case _ =>
        logger.error(s"getChildCount got node of unexpected type ${node.getClass}")
        0
    }

    override def getChild(parent: Object, index: Int): Object = parent match {
      case parent: NodeType =>
        outer.getChild(parent, index)
      case null =>
        logger.error(s"getChild got parent of unexpected null")
        null
      case _ =>
        logger.error(s"getChild got parent of unexpected type ${parent.getClass}")
        null
    }

    override def getIndexOfChild(parent: Object, child: Object): Int = (parent, child) match {
      case (parent: NodeType, child: NodeType) => outer.getIndexOfChild(parent, child)
      case null =>
        logger.error(s"getIndexOfChild got parent of unexpected null")
        -1
      case _ =>
        logger.error(s"getIndexOfChild got parent of unexpected type ${parent.getClass}")
        -1
    }

    override def valueForPathChanged(path: TreePath, newValue: Any): Unit = outer.valueForPathChanged(path, newValue)
    override def addTreeModelListener(l: TreeModelListener): Unit = outer.addTreeModelListener(l)
    override def removeTreeModelListener(l: TreeModelListener): Unit = outer.removeTreeModelListener(l)

    // TreeTableView abstract methods
    //
    override def getColumnCount(): Int = outer.getColumnCount()
    override def getColumnName(column: Int): String = outer.getColumnName(column)
    override def getColumnClass(column: Int): Class[_] = outer.getColumnClass(column)

    override def getValueAt(node: Object, column: Int): Object = node match {
      case node: NodeType => outer.getValueAt(node, column)
      case null =>
        logger.error(s"getValueAt got node of unexpected type null")
        null
      case _ =>
        logger.error(s"getValueAt got node of unexpected type ${node.getClass}")
        null
    }

    // These aren't relevant for trees that can't be edited
    override def isCellEditable(node: Object, column: Int): Boolean = node match {
      case node: NodeType => outer.isCellEditable(node, column)
      case null =>
        logger.error(s"isCellEditable got node of unexpected null")
        false
      case _ =>
        logger.error(s"isCellEditable got node of unexpected type ${node.getClass}")
        false
    }

    override def setValueAt(aValue: Any, node: Object, column: Int): Unit = node match {
      case node: NodeType => outer.setValueAt(aValue, node, column)
      case null =>
        logger.error(s"setValueAt got node of unexpected null")
      case _ =>
        logger.error(s"setValueAt got node of unexpected type ${node.getClass}")
    }

    override def setTree(tree: JTree): Unit = outer.setTree(tree)
  }
}
