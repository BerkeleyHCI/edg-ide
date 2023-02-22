package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edgir.ref.ref
import edgir.elem.elem
import edg.EdgirUtils.SimpleLibraryPath
import edg.IrPort
import edg_ide.EdgirUtils
import edg_ide.proven.ProvenFeature

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._


sealed trait EdgirLibraryNodeTraits  // categories to pass to the tree renderer icon

object EdgirLibraryNodeTraits {
  object Abstract extends EdgirLibraryNodeTraits
  object Category extends EdgirLibraryNodeTraits
  object Footprint extends EdgirLibraryNodeTraits
}


trait EdgirLibraryTreeNode {  // abstract base class for tree node model
  val children: Seq[EdgirLibraryTreeNode]
  val traits: Set[EdgirLibraryNodeTraits] = Set()
}

object EdgirLibraryTreeNode {

  abstract class LibraryElementNode(path: ref.LibraryPath) extends EdgirLibraryTreeNode {
    override def toString: String = path.toSimpleString
  }


  class BlockUnreachableRootNode(paths: Seq[ref.LibraryPath], root: BlockRootNode) extends EdgirLibraryTreeNode {
    override def toString: String = "(unreachable from root)"

    override lazy val children: Seq[EdgirLibraryTreeNode] = {
      paths.map { childPath => new BlockNode(childPath, root.blocks(childPath), root) }
          .sortBy(_.toString)
    }
  }

  class BlockNode(val path: ref.LibraryPath, val block: elem.HierarchyBlock, root: BlockRootNode)
      extends LibraryElementNode(path) {
    override val traits = {
      Set(
        if (EdgirUtils.isCategory(path)) Some(EdgirLibraryNodeTraits.Category) else None,
        if (block.isAbstract) Some(EdgirLibraryNodeTraits.Abstract) else None,
        if (root.allSuperclassesOf(path).contains(EdgirUtils.FootprintBlockType))
          Some(EdgirLibraryNodeTraits.Footprint) else None,
      ).flatten
    }

    override lazy val children: Seq[EdgirLibraryTreeNode] = {
      root.childMap.getOrElse(path, Seq())
          .map { childPath => new BlockNode(childPath, root.blocks(childPath), root) }
          .sortBy(_.toString)
    }
  }

  class BlockRootNode(val blocks: Map[ref.LibraryPath, elem.HierarchyBlock]) extends EdgirLibraryTreeNode {
    override def toString: String = "All Blocks"

    def allSuperclassesOf(blockType: ref.LibraryPath): Seq[ref.LibraryPath] = {
      blocks.get(blockType) match {
        case Some(block) => Seq(blockType) ++ block.superclasses.flatMap { superclassType =>
          allSuperclassesOf(superclassType)
        }
        case None => Seq(blockType)
      }
    }

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
    val childMap: Map[ref.LibraryPath, Seq[ref.LibraryPath]] = blocks.toSeq.flatMap { case (path, block) =>
      block.superclasses match {  // for each block, generate all pairs (superclass path, path)
        case Seq() => Seq((rootPath, path))
        case superclasses => superclasses.map(superclassPath => (superclassPath, path))
      }
    }   .groupBy { case (superclassPath, path) => superclassPath }
        .view.mapValues { superclassPairs => superclassPairs.map(_._2) }.toMap

    private val rootReachable = graphReachable(childMap, rootPath)
    private val unreachableBlocks = blocks.keys.toSet -- rootReachable
    // Only keep blocks that aren't subclasses (root nodes)
    private val allSubclasses = childMap.values.flatten
    private val unreachableSuperclasses = (unreachableBlocks -- allSubclasses).toSeq

    override lazy val children: Seq[EdgirLibraryTreeNode] = {
      val rootChildren = childMap.getOrElse(rootPath, Set())
          .map { childPath => new BlockNode(childPath, blocks(childPath), this) }
          .toSeq
          .sortBy(_.toString)
      rootChildren ++
          (if (unreachableSuperclasses.isEmpty)
            Seq() else
            Seq(new BlockUnreachableRootNode(unreachableSuperclasses, this)))
    }
  }


  class PortNode(val path: ref.LibraryPath, val port: IrPort) extends LibraryElementNode(path) {
    override val children: Seq[EdgirLibraryTreeNode] = Seq()
  }

  class PortRootNode(ports: Map[ref.LibraryPath, IrPort]) extends EdgirLibraryTreeNode {
    override def toString: String = "All Ports"

    override lazy val children: Seq[EdgirLibraryTreeNode] = {
      ports.map { case (path, port) => new PortNode(path, port) }
    }.toSeq.sortBy { _.toString }
  }


  class LinkNode(val path: ref.LibraryPath, val link: elem.Link) extends LibraryElementNode(path) {
    override val children: Seq[EdgirLibraryTreeNode] = Seq()
  }

  class LinkRootNode(links: Map[ref.LibraryPath, elem.Link]) extends EdgirLibraryTreeNode {
    override def toString: String = "All Links"

    lazy val children: Seq[EdgirLibraryTreeNode] = {
      links.map { case (path, link) => new LinkNode(path, link) }
    }.toSeq.sortBy { _.toString }
  }
}


class EdgirLibraryTreeTableModel(library: edg.wir.Library) extends SeqTreeTableModel[EdgirLibraryTreeNode] {
  // Inner classes for node structure
  //
  class EdgirLibraryRootNode extends EdgirLibraryTreeNode {
    override def toString: String = "(root)"

    lazy val children: Seq[EdgirLibraryTreeNode] = Seq(
      new EdgirLibraryTreeNode.BlockRootNode(library.allBlocks),
      new EdgirLibraryTreeNode.PortRootNode(library.allPorts),
      new EdgirLibraryTreeNode.LinkRootNode(library.allLinks),
    )
  }


  // Actual tree model implementation
  //
  val rootNode: EdgirLibraryTreeNode = new EdgirLibraryRootNode()
  val COLUMNS = if (ProvenFeature.kEnabled) {
    Seq("Path", "Proven")
  } else {
    Seq("Path")
  }

  // TreeView abstract methods
  //
  override def getRootNode: EdgirLibraryTreeNode = rootNode

  override def getNodeChildren(node: EdgirLibraryTreeNode): Seq[EdgirLibraryTreeNode] = node.children

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
