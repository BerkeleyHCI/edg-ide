package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.IrPort
import edg.elem.elem
import edg.init.init
import edg_ide.EdgirUtils
import edg.wir._
import edg.compiler.Compiler

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._

trait ElementDetailNode {
  def getChildren: Seq[ElementDetailNode]
  def getColumns(index: Int): String = ""
}


class PortNode(path: DesignPath, port: IrPort, compiler: Compiler) extends ElementDetailNode {
  override def getColumns(index: Int): String = port match {
    case IrPort.Bundle(port) => port.superclasses.map(EdgirUtils.SimpleLibraryPathToString).mkString(", ")
    case IrPort.Port(port) => port.superclasses.map(EdgirUtils.SimpleLibraryPathToString).mkString(", ")
    case _ => s"Unknown"
  }
}

class BlockNode(path: DesignPath, block: elem.HierarchyBlock, compiler: Compiler) extends ElementDetailNode {
  override def getColumns(index: Int): String = {
    block.superclasses.map(EdgirUtils.SimpleLibraryPathToString).mkString(", ")
  }
}

class LinkNode(path: DesignPath, relpath: IndirectDesignPath, link: elem.Link, compiler: Compiler)
    extends ElementDetailNode {
  override def getColumns(index: Int): String = {
    link.superclasses.map(EdgirUtils.SimpleLibraryPathToString).mkString(", ")
  }
  override def getColumns(index: Int): String = path.toString
}


class ParamNode(path: IndirectDesignPath, param: init.ValInit, compiler: Compiler) extends ElementDetailNode {
  override def getChildren: Seq[ElementDetailNode] = Seq()

  override def toString: String = path.steps.last.toString

  override def getColumns(index: Int): String = "TODO VALUE"
}


class ElementDetailTreeModel(root: DesignPath, compiler: Compiler) extends SeqTreeTableModel[ElementDetailNode] {
  val rootNode: ElementDetailNode = new HierarchyBlockNode("(design)", root)
  val COLUMNS = Seq("Item", "Value")

  // TreeView abstract methods
  //
  override def getRootNode: ElementDetailNode = rootNode

  override def getNodeChildren(node: ElementDetailNode): Seq[ElementDetailNode] = node.getChildren

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

  override def getNodeValueAt(node: ElementDetailNode, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: ElementDetailNode, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: ElementDetailNode, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
