package edg_ide

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.ref.ref.LibraryPath

import javax.swing.tree._
import javax.swing.event.TreeModelListener
import javax.swing.JTree


abstract class EdgirLibraryTreeNode {  // abstract base class for tree node model
  protected val children: Seq[EdgirLibraryTreeNode]
  def getChildren: Seq[EdgirLibraryTreeNode] = children
}

abstract class LibraryElementNode(path: LibraryPath) extends EdgirLibraryTreeNode {
  override def toString: String = EdgirUtils.SimpleLibraryPathToString(path)
}


class EdgirLibraryTreeTableModel(library: EdgirLibrary) extends SeqTreeTableModel[EdgirLibraryTreeNode] {
  // Inner classes for node structure
  //
  class EdgirLibraryRootNode extends EdgirLibraryTreeNode {
    override def toString: String = "(root)"

    lazy val children: Seq[EdgirLibraryTreeNode] = Seq(
      new BlockRootNode(),
      new PortRootNode(),
      new LinkRootNode(),
    )
  }

  class BlockRootNode() extends EdgirLibraryTreeNode {
    override def toString: String = "All Blocks"

    lazy val children: Seq[EdgirLibraryTreeNode] = {
      val rootChildren = library.blockChildren.getOrElse(library.rootPath, Set()).toSeq
          .sortBy(EdgirUtils.LibraryPathToString)
          .map(new BlockNode(_))
      rootChildren ++ Seq(new BlockUnreachableRootNode)
    }
  }

  class BlockUnreachableRootNode() extends EdgirLibraryTreeNode {
    override def toString: String = "(unreachable from root)"

    lazy val children: Seq[EdgirLibraryTreeNode] = {
      library.unreachableBlocks.toSeq
          .sortBy(EdgirUtils.LibraryPathToString)
          .map(new BlockNode(_))
    }
  }

  class BlockNode(path: LibraryPath) extends LibraryElementNode(path) {
    lazy val children: Seq[EdgirLibraryTreeNode] = {
      library.blockChildren.getOrElse(path, Seq()).toSeq
          .sortBy(EdgirUtils.LibraryPathToString)
          .map(new BlockNode(_))
    }
  }

  class PortRootNode() extends EdgirLibraryTreeNode {
    override def toString: String = "All Ports"

    lazy val children: Seq[EdgirLibraryTreeNode] = {
      Seq()  // TODO implement me
    }
  }

  class LinkRootNode() extends EdgirLibraryTreeNode {
    override def toString: String = "All Links"

    lazy val children: Seq[EdgirLibraryTreeNode] = {
      Seq()  // TODO implement me
    }
  }

  // Actual tree model implementation
  //
  val rootNode: EdgirLibraryTreeNode = new EdgirLibraryRootNode()
  val COLUMNS = Seq("Path")

  // TreeView abstract methods
  //
  override def getRootNode: EdgirLibraryTreeNode = rootNode

  override def getNodeChildren(node: EdgirLibraryTreeNode): Seq[EdgirLibraryTreeNode] = node.getChildren

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

  override def getNodeValueAt(node: EdgirLibraryTreeNode, column: Int): Object = None

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: EdgirLibraryTreeNode, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: EdgirLibraryTreeNode, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
