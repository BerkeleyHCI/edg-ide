package edg_ide.swing

import com.intellij.openapi.project.Project
import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.ProtoUtil._
import edg.wir.DesignPath
import edg_ide.proven.ProvenFeature
import edg_ide.ui.BlockVisualizerService
import edgir.elem.elem

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._


class HierarchyBlockNode(project: Project, val path: DesignPath, val block: elem.HierarchyBlock) {
  import edgir.elem.elem.BlockLike

  lazy val children: Seq[HierarchyBlockNode] = block.blocks.asPairs.map { case (name, subblock) =>
    (name, subblock.`type`)
  }.collect {
    case (name, BlockLike.Type.Hierarchy(subblock)) => new HierarchyBlockNode(project, path + name, subblock)
  }.toSeq

  override def equals(other: Any): Boolean = other match {
    case other: HierarchyBlockNode => other.block == block
    case _ => false
  }

  override def toString: String = path.lastString

  lazy val classString = block.getSelfClass.toSimpleString

  lazy val proven = {
    new BlockProven(block.getSelfClass, BlockVisualizerService(project).getProvenDatabase.getRecords(block.getSelfClass))
  }
}


object BlockTreeTableModel {
  def follow(path: DesignPath, model: BlockTreeTableModel): (Seq[HierarchyBlockNode], Option[HierarchyBlockNode]) = {

    def inner(nodePrefix: Seq[HierarchyBlockNode], node: HierarchyBlockNode): (Seq[HierarchyBlockNode], Option[HierarchyBlockNode]) = {
      if (node.path == path) {
        (nodePrefix :+ node, Some(node))
      } else {
        val nextChildNodes = node.children.filter { node => path.steps.startsWith(node.path.steps) }
        nextChildNodes match {
          case Seq() => (nodePrefix :+ node, None)  // no further steps possible
          case Seq(childNode) => inner(nodePrefix :+ node, childNode)  // exactly one next step
          case Seq(childNode, _) => inner(nodePrefix :+ node, childNode)  // multiple possible, just pick one
            // TODO maybe this should error or warn
        }
      }
    }

    inner(Seq(), model.rootNode)
  }
}


class BlockTreeTableModel(project: Project, root: elem.HierarchyBlock) extends SeqTreeTableModel[HierarchyBlockNode] {
  val rootNode: HierarchyBlockNode = new HierarchyBlockNode(project, DesignPath(), root)
  val COLUMNS = if (ProvenFeature.kEnabled) {
    Seq("Path", "Class", "Proven")
  } else {
    Seq("Path", "Class")
  }

  // TreeView abstract methods
  //
  override def getRootNode: HierarchyBlockNode = rootNode

  override def getNodeChildren(node: HierarchyBlockNode): Seq[HierarchyBlockNode] = node.children

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
    case 1 => classOf[String]
    case 2 => classOf[ProvenNodeBase]
    case _ => null
  }

  override def getNodeValueAt(node: HierarchyBlockNode, column: Int): Object = column match {
    case 1 => node.classString
    case 2 => node.proven
    case _ => null
  }

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: HierarchyBlockNode, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: HierarchyBlockNode, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
