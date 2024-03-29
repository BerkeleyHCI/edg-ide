package edg_ide.swing.dse

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg_ide.dse.{DseConfigElement, DseDerivedConfig, DseObjective, DseRefinementElement}
import edg_ide.swing.SeqTreeTableModel

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath
import scala.collection.SeqMap

sealed trait SeqNodeBase {
  def getColumns(index: Int): String
  val children: Seq[SeqNodeBase]
}

object DseConfigTreeNode {
  sealed trait DseConfigTreeNode extends SeqNodeBase {
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

  class Root(configs: Seq[DseConfigElement], objectives: Seq[DseObjective]) extends DseConfigTreeNode {
    override val path = ""
    override val value = ""

    lazy val searchConfigNode = new SearchConfigs(configs)
    lazy val objectivesNode = new Objectives(objectives)
    override lazy val children = Seq(searchConfigNode, objectivesNode)
  }

  class SearchConfigs(configs: Seq[DseConfigElement]) extends DseConfigTreeNode {
    override val path = "Search Configs"
    override val value = ""
    override lazy val children = configs.map {
      case config: DseRefinementElement[Any] => new DseRefinementElementNode(config)
      case config: DseDerivedConfig => new DseDerivedConfigNode(config)
    }
  }

  class Objectives(objectives: Seq[DseObjective]) extends DseConfigTreeNode {
    override val path = "Objective Functions"
    override val value = ""
    override lazy val children = objectives.map { config =>
      new DseObjectiveSingleNode(config)
    }.toSeq
  }

  sealed trait DseSearchConfigNode extends DseConfigTreeNode {
    def config: DseConfigElement
  }

  class DseDerivedConfigNode(val config: DseDerivedConfig) extends DseSearchConfigNode {
    override val path = config.configToString
    override val value = "(dynamically determined)"
    override lazy val children = Seq()
  }

  class DseRefinementElementNode(val config: DseRefinementElement[Any]) extends DseSearchConfigNode {
    override val path = config.configToString
    override val value = config.getValues.length.toString
    override lazy val children = config.getValues.map { case (value, refinement) =>
      new LeafNode("", config.valueToString(value))
    }
  }

  sealed trait DseObjectiveNode extends DseConfigTreeNode {
    def config: DseObjective
  }

  class DseObjectiveSingleNode(val config: DseObjective) extends DseObjectiveNode {
    override val path = config.objectiveToString
    override val value = ""
    override lazy val children = Seq()
  }
}

class DseConfigTreeTableModel(searchConfigs: Seq[DseConfigElement], objectives: Seq[DseObjective])
    extends SeqTreeTableModel[SeqNodeBase] {
  val rootNode = new DseConfigTreeNode.Root(searchConfigs, objectives)
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

  def setTree(tree: JTree): Unit = {} // tree updates ignored
}
