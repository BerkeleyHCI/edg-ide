package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.EdgirUtils.SimpleLibraryPath
import edg_ide.dse.{DseConfigElement, DseObjective, DseObjectiveFootprintArea, DseObjectiveFootprintCount, DseObjectiveParameter, DseParameterSearch, DseSubclassSearch}

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath
import scala.collection.SeqMap


trait SeqNodeBase {
  val children: Seq[SeqNodeBase]
  def getColumns(index: Int): String
}


object DseConfigTreeNode {
  trait DseConfigTreeNode extends SeqNodeBase {
    val path: String
    val value: String

    override def getColumns(index: Int): String = index match {
      case 1 => value
      case _ => "???"
    }
    override def toString = path
  }

  class LeafNode(val path: String, val value: String) extends DseConfigTreeNode {
    override val children: Seq[SeqNodeBase] = Seq()
  }

  class Root(configs: Seq[DseConfigElement], objectives: SeqMap[String, DseObjective[Any]]) extends DseConfigTreeNode {
    override lazy val children = Seq(
      new SearchConfigs(configs),
      new Objectives(objectives)
    )
    override val path = ""
    override val value = ""
  }

  class SearchConfigs(configs: Seq[DseConfigElement]) extends DseConfigTreeNode {
    override lazy val children = configs.map {
      case config: DseParameterSearch => new DseParameterSearchNode(config)
      case config: DseSubclassSearch => new DseSubclassSearchNode(config)
    }
    override val path = "Search Configs"
    override val value = ""
  }

  class Objectives(objectives: SeqMap[String, DseObjective[Any]]) extends DseConfigTreeNode {
    override lazy val children =  objectives.map { case (name, objective) => objective match {
      case config: DseObjectiveParameter => new DseObjectiveParameterNode(name, config)
      case config: DseObjectiveFootprintArea => new DseObjectiveFootprintAreaNode(name, config)
      case config: DseObjectiveFootprintCount => new DseObjectiveFootprintCountNode(name, config)
    } }.toSeq
    override val path = "Objective Functions"
    override val value = ""
  }

  class DseParameterSearchNode(config: DseParameterSearch) extends DseConfigTreeNode {
    override val path = config.path.toString
    override val value = f"Parameters (${config.values.length})"
    override lazy val children = config.values.map { value =>
      new LeafNode("", value.toStringValue)
    }
  }

  class DseSubclassSearchNode(config: DseSubclassSearch) extends DseConfigTreeNode {
    override val path = config.path.toString
    override val value = f"Subclasses (${config.subclasses.length})"
    override lazy val children = config.subclasses.map { subclass =>
      new LeafNode("", subclass.toSimpleString)
    }
  }

  class DseObjectiveParameterNode(name: String, config: DseObjectiveParameter) extends DseConfigTreeNode {
    override val path = name
    override val value = f"Parameter @ ${config.path}"
    override lazy val children = Seq()
  }

  class DseObjectiveFootprintAreaNode(name: String, config: DseObjectiveFootprintArea) extends DseConfigTreeNode {
    override val path = name
    override val value = f"Footprint Area in ${config.rootPath}"
    override lazy val children = Seq()
  }

  class DseObjectiveFootprintCountNode(name: String, config: DseObjectiveFootprintCount) extends DseConfigTreeNode {
    override val path = name
    override val value = f"Footprint Count in ${config.rootPath}"
    override lazy val children = Seq()
  }
}


class DseConfigTreeTableModel(searchConfigs: Seq[DseConfigElement], objectives: SeqMap[String, DseObjective[Any]])
    extends SeqTreeTableModel[SeqNodeBase] {
  val rootNode: SeqNodeBase = new DseConfigTreeNode.Root(searchConfigs, objectives)
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
