package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.ExprToString
import edg.compiler.{hdl => edgrpc}
import edg_ide.EdgirUtils

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


trait RefinementsNodeBase {
  val children: Seq[RefinementsNodeBase]
  def getColumns(index: Int): String
}

object RefinementsNodeBase {
  class RefinementsRootNode(refinements: edgrpc.Refinements) extends RefinementsNodeBase {
    override lazy val children = Seq(
      new ClassRefinementsNode(refinements), new InstanceRefinementsNode(refinements),
      new ClassValuesNode(refinements), new InstanceValuesNode(refinements),

    )
    override def getColumns(index: Int): String = ""
    override def toString: String = ""
  }

  class ClassRefinementsNode(refinements: edgrpc.Refinements) extends RefinementsNodeBase {
    override lazy val children = refinements.subclasses.map { subclass =>
      (subclass.source, subclass.getReplacement)
    }.collect {
      case (edgrpc.Refinements.Subclass.Source.Cls(srcType), replaceType) =>
        RefinementsDetailNode(srcType.toSimpleString, replaceType.toSimpleString)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Class Subclasses"
  }

  class InstanceRefinementsNode(refinements: edgrpc.Refinements) extends RefinementsNodeBase {
    override lazy val children = refinements.subclasses.map { subclass =>
      (subclass.source, subclass.getReplacement)
    }.collect {
      case (edgrpc.Refinements.Subclass.Source.Path(srcPath), replaceType) =>
        RefinementsDetailNode(ExprToString(srcPath), replaceType.toSimpleString)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Subclasses"
  }

  class ClassValuesNode(refinements: edgrpc.Refinements) extends RefinementsNodeBase {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.getValue)
    }.collect {
      case (edgrpc.Refinements.Value.Source.ClsParam(srcTypeParam), replaceValue) =>
        RefinementsDetailNode(
          srcTypeParam.getCls.toSimpleString + ":" + ExprToString(srcTypeParam.getParamPath),
          ExprToString(replaceValue))
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Class Values"
  }

  class InstanceValuesNode(refinements: edgrpc.Refinements) extends RefinementsNodeBase {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.getValue)
    }.collect {
      case (edgrpc.Refinements.Value.Source.Path(srcPath), replaceValue) =>
        RefinementsDetailNode(ExprToString(srcPath), ExprToString(replaceValue))
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Values"
  }

  // Freeform node with arbitrary text and path
  case class RefinementsDetailNode(target: String, value: String) extends RefinementsNodeBase {
    override lazy val children = Seq()
    override def getColumns(index: Int): String = value
    override def toString: String = target
  }
}


class RefinementsTreeTableModel(refinements: edgrpc.Refinements) extends SeqTreeTableModel[RefinementsNodeBase] {
  val rootNode: RefinementsNodeBase = new RefinementsNodeBase.RefinementsRootNode(refinements)
  val COLUMNS = Seq("Target", "Value")

  // TreeView abstract methods
  //
  override def getRootNode: RefinementsNodeBase = rootNode

  override def getNodeChildren(node: RefinementsNodeBase): Seq[RefinementsNodeBase] = node.children

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

  override def getNodeValueAt(node: RefinementsNodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: RefinementsNodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: RefinementsNodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
