package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.ref.ref
import edg.elem.elem
import edg_ide.{EdgirLibrary, EdgirUtils}
import edg.wir
import edg.IrPort

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._


abstract class EdgirLibraryTreeNode {  // abstract base class for tree node model
  protected val children: Seq[EdgirLibraryTreeNode]
  def getChildren: Seq[EdgirLibraryTreeNode] = children
}

abstract class LibraryElementNode(path: ref.LibraryPath) extends EdgirLibraryTreeNode {
  override def toString: String = EdgirUtils.SimpleLibraryPathToString(path)
}


class BlockUnreachableRootNode(paths: Seq[ref.LibraryPath], root: BlockRootNode) extends EdgirLibraryTreeNode {
  override def toString: String = "(unreachable from root)"

  lazy val children: Seq[EdgirLibraryTreeNode] = {
    paths.map { childPath => new BlockNode(childPath, root.blocks(childPath), root) }
  }
}

class BlockNode(path: ref.LibraryPath, block: elem.HierarchyBlock, root: BlockRootNode)
    extends LibraryElementNode(path) {
  lazy val children: Seq[EdgirLibraryTreeNode] = {
    root.childMap.getOrElse(path, Seq())
        .map { childPath => new BlockNode(childPath, root.blocks(childPath), root) }
        .sortBy(_.toString)
  }
}

class BlockRootNode(val blocks: Map[ref.LibraryPath, elem.HierarchyBlock]) extends EdgirLibraryTreeNode {
  override def toString: String = "All Blocks"

  // Returns the reachable set from a node, including the starting if it is present in the graph
  def graphReachable[T](graph: Map[T, Seq[T]], from: T): Seq[T] = {
    if (graph.contains(from)) {
      val reachable = graph.getOrElse(from, Seq()).flatMap { childElt =>
        graphReachable(graph, childElt)
      }
      Seq(from) ++ reachable
    } else {
      Seq()
    }
  }

  private val rootPath = ref.LibraryPath(target=None)
  val childMap: Map[ref.LibraryPath, Seq[ref.LibraryPath]] = blocks.flatMap { case (path, block) =>
    block.superclasses match {  // for each block, generate all pairs (superclass path, path)
      case Nil => Seq((rootPath, path))
      case superclasses => superclasses.map(superclassPath => (superclassPath, path))
    }
  }   .groupBy { case (superclassPath, path) => superclassPath }
      .mapValues { superclassPairs => superclassPairs.map(_._2).toSeq }.toMap

  private val rootReachable = graphReachable(childMap, rootPath)
  private val unreachableBlocks = blocks.keys.toSet -- rootReachable

  lazy val children: Seq[EdgirLibraryTreeNode] = {
    val rootChildren = childMap.getOrElse(rootPath, Set())
        .map { childPath => new BlockNode(childPath, blocks(childPath), this) }
        .toSeq
        .sortBy(_.toString)
    rootChildren ++ Seq(new BlockUnreachableRootNode(unreachableBlocks.toSeq, this))
  }
}


class PortNode(path: ref.LibraryPath, port: IrPort) extends LibraryElementNode(path) {
  val children: Seq[EdgirLibraryTreeNode] = Seq()
}

class PortRootNode(ports: Map[ref.LibraryPath, IrPort]) extends EdgirLibraryTreeNode {
  override def toString: String = "All Ports"

  lazy val children: Seq[EdgirLibraryTreeNode] = {
    ports.map { case (path, port) => new PortNode(path, port) }
  }.toSeq.sortBy { _.toString }
}


class LinkNode(path: ref.LibraryPath, link: elem.Link) extends LibraryElementNode(path) {
  val children: Seq[EdgirLibraryTreeNode] = Seq()
}

class LinkRootNode(links: Map[ref.LibraryPath, elem.Link]) extends EdgirLibraryTreeNode {
  override def toString: String = "All Links"

  lazy val children: Seq[EdgirLibraryTreeNode] = {
    links.map { case (path, link) => new LinkNode(path, link) }
  }.toSeq.sortBy { _.toString }
}


class EdgirLibraryTreeTableModel(library: edg.wir.Library) extends SeqTreeTableModel[EdgirLibraryTreeNode] {
  // Inner classes for node structure
  //
  class EdgirLibraryRootNode extends EdgirLibraryTreeNode {
    override def toString: String = "(root)"

    lazy val children: Seq[EdgirLibraryTreeNode] = Seq(
      new BlockRootNode(library.allBlocks),
      new PortRootNode(library.allPorts),
      new LinkRootNode(library.allLinks),
    )
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
