package edg_ide.swing.dse

import com.intellij.ui.JBColor
import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.compiler.CompilerError
import edg_ide.dse.{CombinedDseResultSet, DseConfigElement, DseObjective, DseResult}
import edg_ide.swing.SeqTreeTableModel

import java.awt.{Color, Component}
import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.{DefaultTreeCellRenderer, TreePath}


trait DseResultNodeBase {
  val children: Seq[DseResultNodeBase]
  val config: String
  def getColumns(index: Int): String

  override def toString = config
}

class DseResultTreeNode(results: CombinedDseResultSet, objective: Seq[DseObjective], inProgress: Boolean)
    extends DseResultNodeBase {
  private val informationalHeader = if (inProgress) {
    Seq(new InformationalNode("... search in progress ..."))
  } else {
    Seq()
  }
  private val objectiveByColumn = objective.zipWithIndex.map { case (objective, index) =>
    index + 1 -> objective
  }.toMap


  // Defines the root node
  override lazy val children = informationalHeader ++ results.groupedResults.map { resultsSet =>
      if (resultsSet.length == 1) {
        new ResultNode(resultsSet.head, true)
      } else {
        new ResultSetNode(resultsSet)
      }
  }
  override val config = "" // empty, since the root node is hidden
  override def getColumns(index: Int): String = ""


  // Displays a set of equivalent results, useful for deduplicating similar results
  class ResultSetNode(val setMembers: Seq[DseResult]) extends DseResultNodeBase {
    private val exampleResult = setMembers.head
    private val errString = if (exampleResult.errors.nonEmpty) {
      " (with errors)"
    } else {
      ""
    }
    override val config = f"${setMembers.length} points" + errString
    // internal objectives not shown to avoid visual clutter
    override lazy val children = setMembers.map(result => new ResultNode(result, false))
    override def getColumns(index: Int): String = objectiveByColumn.get(index) match {
      case Some(objective) => exampleResult.objectives.get(objective) match {
        case Some(value) => DseConfigElement.valueToString(value)
        case _ => "(unknown)"
      }
      case _ => "???"
    }
  }

  class ResultNode(val result: DseResult, showObjectives: Boolean) extends DseResultNodeBase {
    override val config = f"${result.index}: ${DseConfigElement.configMapToString(result.config)}"
    override val children: Seq[DseResultNodeBase] = Seq()

    override def getColumns(index: Int): String = {
      if (showObjectives) {
        objectiveByColumn.get(index) match {
          case Some(objective) => result.objectives.get(objective) match {
            case Some(value) => DseConfigElement.valueToString(value)
            case _ => "(unknown)"
          }
          case _ => "???"
        }
      } else {
        ""
      }
    }
  }

  class InformationalNode(text: String) extends DseResultNodeBase {
    override val config = text
    override def getColumns(index: Int): String = ""
    override val children: Seq[DseResultNodeBase] = Seq()
  }
}


class DseResultTreeTableModel(results: CombinedDseResultSet, objectives: Seq[DseObjective], inProgress: Boolean)
    extends SeqTreeTableModel[DseResultNodeBase] {
  val COLUMNS = Seq("Config") ++ objectives.map(_.objectiveToString)

  val rootNode: DseResultTreeNode = new DseResultTreeNode(results, objectives, inProgress)

  // TreeView abstract methods
  //
  override def getRootNode: DseResultNodeBase = rootNode

  override def getNodeChildren(node: DseResultNodeBase): Seq[DseResultNodeBase] = node.children

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

  override def getNodeValueAt(node: DseResultNodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: DseResultNodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: DseResultNodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}


// TODO maybe this should go elsewhere? but nothing else uses ideal errors
// TODO Maybe this should be shared with the block diagram renderer?
object DseResultModel {
  private val kIdealConstraintName = "ideal model"

  val kColorOtherError: Color = JBColor.RED
  val kColorIdealError: Color = JBColor.ORANGE

  // Partitions the input compiler errors as (ideal model errors, any other error)
  def partitionByIdeal(errors: Seq[CompilerError]): (Seq[CompilerError], Seq[CompilerError]) = errors.partition {
    case CompilerError.FailedAssertion(_, constrName, _, _, _) if constrName == kIdealConstraintName => true
    case _ => false
  }
}

class DseResultTreeRenderer extends DefaultTreeCellRenderer {
  override def getTreeCellRendererComponent(tree: JTree, value: Any, sel: Boolean, expanded: Boolean, leaf: Boolean,
                                            row: Int, hasFocus: Boolean): Component = {
    val component = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    val example = value match {
      case node: DseResultTreeNode#ResultSetNode => Some(node.setMembers.head)
      case node: DseResultTreeNode#ResultNode => Some(node.result)
      case _ => None
    }
    val color = example.flatMap { example =>
      val (idealErrors, otherErrors) = DseResultModel.partitionByIdeal(example.errors)
      (idealErrors.nonEmpty, otherErrors.nonEmpty) match {
        case (_, true) => Some(DseResultModel.kColorOtherError)
        case (true, false) => Some(DseResultModel.kColorIdealError)
        case (false, false) => None
      }
    }
    color.foreach(component.setForeground(_))
    setIcon(null)
    component
  }
}
