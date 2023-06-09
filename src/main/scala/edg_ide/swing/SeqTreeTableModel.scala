package edg_ide.swing

import com.intellij.openapi.diagnostic.Logger
import com.intellij.ui.treeStructure.treetable.TreeTableModel

import scala.reflect.ClassTag

/** A TreeTableModel where all node objects are of a type, and dynamically checked in this class with non-fatal errors.
  * Presents a strongly typed (instead of Object) interface for subclasses.
  * @tparam NodeType
  */
abstract class ParameterizedTreeTableModel[NodeType <: Object](implicit tag: ClassTag[NodeType])
    extends TreeTableModel {
  val logger = Logger.getInstance(classOf[BlockTreeTableModel])

  // TreeView abstract methods
  //
  def getRootNode: NodeType
  final override def getRoot: Object = getRootNode

  def isNodeLeaf(node: NodeType): Boolean
  final override def isLeaf(node: Object): Boolean = node match {
    case node: NodeType => isNodeLeaf(node)
    case null =>
      logger.error(s"isLeaf got node of unexpected null")
      true
    case _ =>
      logger.error(s"isLeaf got node of unexpected type ${node.getClass}")
      true
  }

  def getNodeChildCount(node: NodeType): Int
  final override def getChildCount(node: Object): Int = node match {
    case node: NodeType => getNodeChildCount(node)
    case null =>
      logger.error(s"getChildCount got node of unexpected null")
      0
    case _ =>
      logger.error(s"getChildCount got node of unexpected type ${node.getClass}")
      0
  }

  def getNodeChild(parent: NodeType, index: Int): NodeType
  final override def getChild(parent: Object, index: Int): Object = parent match {
    case parent: NodeType =>
      getNodeChild(parent, index)
    case null =>
      logger.error(s"getChild got parent of unexpected null")
      null
    case _ =>
      logger.error(s"getChild got parent of unexpected type ${parent.getClass}")
      null
  }

  def getIndexOfNodeChild(parent: NodeType, child: NodeType): Int
  final override def getIndexOfChild(parent: Object, child: Object): Int = (parent, child) match {
    case (parent: NodeType, child: NodeType) => getIndexOfNodeChild(parent, child)
    case null =>
      logger.error(s"getIndexOfChild got parent of unexpected null")
      -1
    case _ =>
      logger.error(s"getIndexOfChild got parent of unexpected type ${parent.getClass}")
      -1
  }

  // TreeTableView abstract methods
  //
  def getNodeValueAt(node: NodeType, column: Int): Object
  final override def getValueAt(node: Object, column: Int): Object = node match {
    case node: NodeType => getNodeValueAt(node, column)
    case null =>
      logger.error(s"getValueAt got node of unexpected type null")
      null
    case _ =>
      logger.error(s"getValueAt got node of unexpected type ${node.getClass}")
      null
  }

  def isNodeCellEditable(node: NodeType, column: Int): Boolean = false
  final override def isCellEditable(node: Object, column: Int): Boolean = node match {
    case node: NodeType => isNodeCellEditable(node, column)
    case null =>
      logger.error(s"isCellEditable got node of unexpected null")
      false
    case _ =>
      logger.error(s"isCellEditable got node of unexpected type ${node.getClass}")
      false
  }

  def setNodeValueAt(aValue: Any, node: NodeType, column: Int): Unit = {}
  final override def setValueAt(aValue: Any, node: Object, column: Int): Unit = node match {
    case node: NodeType => setNodeValueAt(aValue, node, column)
    case null =>
      logger.error(s"setValueAt got node of unexpected null")
    case _ =>
      logger.error(s"setValueAt got node of unexpected type ${node.getClass}")
  }
}

/** TreeTable model where the children for each node is defined by a Seq. Also includes default (but overrideable)
  * isLeaf implementation based on child counts.
  */
abstract class SeqTreeTableModel[NodeType <: Object](implicit tag: ClassTag[NodeType])
    extends ParameterizedTreeTableModel[NodeType] {
  def getNodeChildren(node: NodeType): Seq[NodeType] // ordering must be stable!

  override def isNodeLeaf(node: NodeType): Boolean = getNodeChildren(node).isEmpty
  final override def getNodeChildCount(node: NodeType): Int = getNodeChildren(node).length
  final override def getNodeChild(parent: NodeType, index: Int): NodeType = {
    val children = getNodeChildren(parent)
    if (index < children.size) {
      children(index)
    } else {
      null.asInstanceOf[NodeType]
    }
  }
  final override def getIndexOfNodeChild(parent: NodeType, child: NodeType): Int =
    getNodeChildren(parent).indexOf(child)
}
