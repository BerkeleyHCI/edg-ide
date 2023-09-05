package edg_ide.swing

import com.intellij.openapi.project.Project
import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.EdgirUtils.SimpleLibraryPath
import edg_ide.EdgirUtils
import edg_ide.ui.{BlockVisualizerService, EdgSettingsState}
import edg.ElemBuilder.LibraryPath
import edgir.elem.elem
import edgir.ref.ref

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._

sealed trait EdgirLibraryNodeTraits // categories to pass to the tree renderer icon

object EdgirLibraryNodeTraits {
  object Abstract extends EdgirLibraryNodeTraits
  object Category extends EdgirLibraryNodeTraits
}

trait EdgirLibraryNodeBase { // abstract base class for tree node model
  val children: Seq[EdgirLibraryNodeBase]
  val traits: Set[EdgirLibraryNodeTraits] = Set()
  def proven: ProvenNodeBase = EmptyProven
}

class EdgirLibraryNode(project: Project, library: edg.wir.Library) extends EdgirLibraryNodeBase {
  // This (root) node properties
  //
  override def toString: String = "(root)"

  lazy val children: Seq[EdgirLibraryNodeBase] = Seq(
    new BlockRootNode(),
    new PortRootNode(),
    new LinkRootNode()
  )

  // Child nodes
  //
  abstract class LibraryElementNode(path: ref.LibraryPath) extends EdgirLibraryNodeBase {
    override def toString: String = path.toSimpleString
  }

  class BlockUnreachableRootNode(paths: Seq[ref.LibraryPath], root: BlockRootNode)
      extends EdgirLibraryNodeBase {
    override def toString: String = "(unreachable from root)"

    override lazy val children: Seq[EdgirLibraryNodeBase] = {
      paths
        .map { childPath => new BlockNode(childPath, library.allBlocks(childPath), root) }
        .sortBy(_.toString)
    }
  }

  class BlockNode(val path: ref.LibraryPath, val block: elem.HierarchyBlock, root: BlockRootNode)
      extends LibraryElementNode(path) {
    override val traits = {
      Set(
        if (EdgirUtils.isCategory(path)) Some(EdgirLibraryNodeTraits.Category) else None,
        if (block.isAbstract) Some(EdgirLibraryNodeTraits.Abstract) else None
      ).flatten
    }

    override lazy val children: Seq[EdgirLibraryNodeBase] = {
      childMap
        .getOrElse(path, Seq())
        .map { childPath => new BlockNode(childPath, library.allBlocks(childPath), root) }
        .sortBy(_.toString)
    }

    override lazy val proven = {
      new BlockProven(path, BlockVisualizerService(project).getProvenDatabase.getRecords(path))
    }
  }

  def allSuperclassesOf(blockType: ref.LibraryPath): Seq[ref.LibraryPath] = {
    library.allBlocks.get(blockType) match {
      case Some(block) =>
        Seq(blockType) ++ block.superclasses.flatMap { superclassType =>
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

  private val rootPath = ref.LibraryPath(target = None)
  val childMap: Map[ref.LibraryPath, Seq[ref.LibraryPath]] = library.allBlocks.toSeq
    .flatMap { case (path, block) =>
      block.superclasses match { // for each block, generate all pairs (superclass path, path)
        case Seq() => Seq((rootPath, path))
        case superclasses => superclasses.map(superclassPath => (superclassPath, path))
      }
    }
    .groupBy { case (superclassPath, path) => superclassPath }
    .view
    .mapValues { superclassPairs => superclassPairs.map(_._2) }
    .toMap

  class BlockRootNode() extends EdgirLibraryNodeBase {
    override def toString: String = "All Blocks"

    override lazy val children: Seq[EdgirLibraryNodeBase] = {
      val rootChildren = childMap
        .getOrElse(rootPath, Set())
        .map { childPath => new BlockNode(childPath, library.allBlocks(childPath), this) }
        .toSeq
        .sortWith { case (a, b) =>
          if (EdgirUtils.isInternal(a.path) && !EdgirUtils.isInternal(b.path)) {
            false
          } else if (!EdgirUtils.isInternal(a.path) && EdgirUtils.isInternal(b.path)) {
            true
          } else {
            a.toString < b.toString
          }
        }

      // display blocks that are unreachable from the root (eg, if superclasses are missing, because of bad compile)
      val rootReachable = graphReachable(childMap, rootPath)
      val unreachableBlocks = library.allBlocks.keys.toSet -- rootReachable
      val allSubclasses = childMap.values.flatten // prune these to only show the top-level superclasses
      val unreachableSuperclasses = (unreachableBlocks -- allSubclasses).toSeq

      rootChildren ++
        (if (unreachableSuperclasses.isEmpty)
           Seq()
         else
           Seq(new BlockUnreachableRootNode(unreachableSuperclasses, this)))
    }
  }

  class PortNode(val path: ref.LibraryPath) extends LibraryElementNode(path) {
    override val children: Seq[EdgirLibraryNodeBase] = Seq()
  }

  class PortRootNode() extends EdgirLibraryNodeBase {
    override def toString: String = "All Ports"

    override lazy val children: Seq[EdgirLibraryNodeBase] = {
      library.allPorts.map { case (path, _) => new PortNode(path) }
    }.toSeq.sortBy { _.toString }
  }

  class LinkNode(val path: ref.LibraryPath) extends LibraryElementNode(path) {
    override val children: Seq[EdgirLibraryNodeBase] = Seq()
  }

  class LinkRootNode() extends EdgirLibraryNodeBase {
    override def toString: String = "All Links"

    lazy val children: Seq[EdgirLibraryNodeBase] = {
      library.allLinks.map { case (path, _) => new LinkNode(path) }
    }.toSeq.sortBy { _.toString }
  }
}

class EdgirLibraryTreeTableModel(project: Project, library: edg.wir.Library)
    extends SeqTreeTableModel[EdgirLibraryNodeBase] {
  // Actual tree model implementation
  //
  val rootNode: EdgirLibraryNode = new EdgirLibraryNode(project, library)
  val COLUMNS = if (EdgSettingsState.getInstance().showProvenStatus) {
    Seq("Path", "Proven")
  } else {
    Seq("Path")
  }

  // TreeView abstract methods
  //
  override def getRootNode: EdgirLibraryNode = rootNode

  override def getNodeChildren(node: EdgirLibraryNodeBase): Seq[EdgirLibraryNodeBase] = node.children

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
    case 1 => classOf[ProvenNodeBase]
    case _ => null
  }

  override def getNodeValueAt(node: EdgirLibraryNodeBase, column: Int): Object = column match {
    case 1 => node.proven
    case _ => null
  }

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: EdgirLibraryNodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: EdgirLibraryNodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = {} // tree updates ignored
}
