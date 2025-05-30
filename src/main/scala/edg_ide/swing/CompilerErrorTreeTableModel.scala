package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, CompilerError, ElaborateRecord, ExprToString, ExprVarToValue}

import java.awt.event.MouseEvent
import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.table.{JTableHeader, TableColumnModel}
import javax.swing.tree.TreePath

trait CompilerErrorNodeBase {
  val children: Seq[CompilerErrorNodeBase]
  def getColumns(index: Int): String
}

object CompilerErrorNodeBase {
  class CompilerErrorTopNode(errs: Seq[CompilerError], compiler: Compiler) extends CompilerErrorNodeBase {
    override lazy val children: Seq[CompilerErrorNodeBase] = errs.map {
      new CompilerErrorNode(_, compiler)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "All Errors"
  }

  // Freeform node with arbitrary text and path
  class CompilerErrorDetailNode(text: String, path: String) extends CompilerErrorNodeBase {
    override lazy val children = Seq()
    override def getColumns(index: Int): String = path
    override def toString: String = text
  }

  // Error node defining text, path, and children for a compiler error
  class CompilerErrorNode(err: CompilerError, compiler: Compiler) extends CompilerErrorNodeBase {
    def elaborateRecordToDetailNode(rec: ElaborateRecord): CompilerErrorDetailNode = rec match {
      case rec: ElaborateRecord.ElaborateTask =>
        new CompilerErrorDetailNode(s"Unexpected Missing ElaborateTask Record $rec", "")
      case ElaborateRecord.ParamValue(path) =>
        new CompilerErrorDetailNode("Missing Param Value", path.toString)
      case ElaborateRecord.Port(path) =>
        new CompilerErrorDetailNode("Missing Port", path.toString)
    }

    private lazy val all: (String, String, Seq[CompilerErrorNodeBase]) = err match {
      case CompilerError.Unelaborated(ElaborateRecord.Block(path, _), deps) =>
        ("Unelaborated Block", path.toString, deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.Unelaborated(ElaborateRecord.Link(path), deps) =>
        ("Unelaborated Link", path.toString, deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.Unelaborated(ElaborateRecord.ParamValue(path), deps) =>
        ("Unelaborated Param", path.toString, deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.Unelaborated(
          ElaborateRecord.Connect(toLinkPortPath, fromLinkPortPath, root),
          deps
        ) =>
        (
          s"Unelaborated Connect",
          root.toString,
          Seq(
            new CompilerErrorDetailNode("Connect Towards Link Port", toLinkPortPath.toString),
            new CompilerErrorDetailNode("Connect Away From Link Port", fromLinkPortPath.toString)
          ) ++ deps.toSeq.map(elaborateRecordToDetailNode)
        )
      case CompilerError.Unelaborated(unelaborated, deps) =>
        (s"Unknown unelaborated $unelaborated", "", deps.toSeq.map(elaborateRecordToDetailNode))

      case CompilerError.LibraryElement(path, target) =>
        (s"Missing library element ${target.toSimpleString}", path.toString, Seq())

      case CompilerError.BadRef(path, ref) =>
        (s"Bad reference $ref", path.toString, Seq())
      case CompilerError.UndefinedPortArray(path, portType) =>
        (s"Undefined port array", path.toString, Seq())
      case CompilerError.LibraryError(path, target, err) =>
        (
          s"Library error, ${target.toSimpleString}",
          path.toString,
          err.split('\n').toSeq.map(new CompilerErrorDetailNode(_, ""))
        )
      case CompilerError.GeneratorError(path, target, err) =>
        (
          s"Generator error, ${target.toSimpleString}",
          path.toString,
          err.split('\n').toSeq.map(new CompilerErrorDetailNode(_, ""))
        )
      case CompilerError.RefinementSubclassError(path, refinedLibrary, designLibrary) =>
        (
          s"Refinement class ${refinedLibrary.toSimpleString} " +
            s"not a subclass of design class ${designLibrary.toSimpleString}",
          path.toString,
          Seq()
        )

      case CompilerError.ExprError(target, msg) =>
        (
          "Expr error",
          target.toString,
          Seq(new CompilerErrorDetailNode(msg, ""))
        )
      case CompilerError.AbstractBlock(path, blockType) =>
        (s"Abstract block, ${blockType.toSimpleString}", path.toString, Seq())
      case CompilerError.FailedAssertion(root, constrName, value, result, compiler) => {
        (
          s"Failed assertion",
          s"$root:$constrName",
          Seq(new CompilerErrorDetailNode(ExprVarToValue(value, compiler, root), result.toStringValue))
        )
      }
      case CompilerError.MissingAssertion(root, constrName, value, missing) =>
        (
          s"Missing assertion",
          s"$root:$constrName",
          missing.toSeq.map { param =>
            new CompilerErrorDetailNode("Missing param", param.toString)
          }
        )

      case CompilerError.InconsistentLinkArrayElements(
          root,
          linkPath,
          linkElements,
          blockPortPath,
          blockPortElements
        ) =>
        (
          s"Inconsistent link array elements",
          s"$linkPath",
          Seq(
            new CompilerErrorDetailNode("Link elements", linkElements.toStringValue),
            new CompilerErrorDetailNode(
              f"Block port elements @ $blockPortPath",
              blockPortElements.toStringValue
            )
          )
        )
    }

    override lazy val children: Seq[CompilerErrorNodeBase] = all._3
    override def getColumns(index: Int): String = all._2
    override def toString: String = all._1
  }
}

class CustomTooltipTableHeader(columnModel: TableColumnModel)
    extends JTableHeader(columnModel: TableColumnModel) {
  override def getToolTipText(e: MouseEvent): String = {
    val col = columnModel.getColumnIndexAtX(e.getX)
    val name = columnModel.getColumn(col).getHeaderValue.toString
    val headerTooltipText = "Path from design root to constraint of the component causing the error. <br> " +
      "For example: mcu.gnd:overcurrent, indicates that the gnd <br>" +
      "overcurrent constraint is failing."
    val tooltip = name match {
      case "Path ⓘ" => headerTooltipText
      case "Error" => "Error message"
      case _ => "Unknown"
    }
    tooltip
  }
}

class CompilerErrorTreeTableModel(errs: Seq[CompilerError], compiler: Compiler)
    extends SeqTreeTableModel[CompilerErrorNodeBase] {
  val rootNode: CompilerErrorNodeBase = new CompilerErrorNodeBase.CompilerErrorTopNode(errs, compiler)
  val COLUMNS = Seq("Error", "Path ⓘ")

  // TreeView abstract methods
  //
  override def getRootNode: CompilerErrorNodeBase = rootNode

  override def getNodeChildren(node: CompilerErrorNodeBase): Seq[CompilerErrorNodeBase] = node.children

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

  override def getNodeValueAt(node: CompilerErrorNodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: CompilerErrorNodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: CompilerErrorNodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = {} // tree updates ignored
}
