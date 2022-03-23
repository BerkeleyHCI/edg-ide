package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edgir.common.common
import edgir.elem.elem
import edgir.expr.expr
import edgir.init.init
import edgir.schema.schema
import edg.wir._
import edg.EdgirUtils.SimpleLibraryPath
import edg.ExprBuilder
import edg.compiler.{Compiler, ExprResult, ExprToString}
import edg.util.SeqMapSortableFrom._
import edg_ide.EdgirUtils

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._
import scala.collection.SeqMap


trait ElementDetailNode {
  val children: Seq[ElementDetailNode]
  def getColumns(index: Int): String = ""
}


object ElementDetailNode {
  class Dummy(text: String) extends ElementDetailNode {
    override val children: Seq[ElementDetailNode] = Seq()
    override def toString: String = text
  }
}


class ElementDetailNodes(root: schema.Design, compiler: Compiler) {
  sealed trait BasePortNode extends ElementDetailNode {
    val path: DesignPath
    val fromLink: Boolean

    lazy val linkNode: Option[ElementDetailNode] = if (!fromLink) {
      compiler.getConnectedLink(path) match {
        case Some(linkPath) => EdgirUtils.resolveExactLink(linkPath, root) match {
          case Some(link) =>
            Some(new LinkNode(linkPath, path.asIndirect + IndirectStep.ConnectedLink, link))
          case None =>
            Some(new ElementDetailNode.Dummy(s"Invalid connected @ $path"))
        }
        case None => Some(new ElementDetailNode.Dummy("Disconnected"))  // not connected
      }
    } else {
      None
    }

    override def toString = if (!fromLink) {
      compiler.getConnectedLink(path) match {
        case Some(linkPath) => s"${path.lastString} @ $linkPath"
        case None => path.lastString
      }
    } else {
      path.lastString
    }
  }

  class UnelaboratedNode(path: DesignPath, desc: String) extends ElementDetailNode {
    override val children: Seq[ElementDetailNode] = Seq()
    override def toString = path.toString
    override def getColumns(index: Int): String = desc
  }

  class PortNode(val path: DesignPath, port: elem.Port,
                 val fromLink: Boolean=false)
      extends BasePortNode {
    override lazy val children = {
      val nameOrder = ProtoUtil.getNameOrder(port.meta)
      Seq(
        linkNode,
        Some(new ParamNode(path.asIndirect + IndirectStep.IsConnected, ExprBuilder.ValInit.Boolean)),
        port.params.sortKeysFrom(nameOrder).map {
          case (name, param) => new ParamNode(path.asIndirect + name, param)
        },
      ).flatten
    }

    override def getColumns(index: Int): String = port.getSelfClass.toSimpleString
  }

  class BundleNode(val path: DesignPath, port: elem.Bundle,
                   val fromLink: Boolean=false)
      extends BasePortNode {
    override lazy val children = {
      val nameOrder = ProtoUtil.getNameOrder(port.meta)
      Seq(
        Some(new ParamNode(path.asIndirect + IndirectStep.IsConnected, ExprBuilder.ValInit.Boolean)),
        port.ports.sortKeysFrom(nameOrder).map {
          case (name, subport) => PortLikeNode(path + name, subport, fromLink)
        },
        port.params.sortKeysFrom(nameOrder).map {
          case (name, param) => new ParamNode(path.asIndirect + name, param)
        },
      ).flatten
    }

    override def getColumns(index: Int): String = port.getSelfClass.toSimpleString
  }

  class ArrayNode(val path: DesignPath, port: elem.PortArray, val fromLink: Boolean=false)
      extends BasePortNode {
    override lazy val children = {
      val nameOrder = ProtoUtil.getNameOrder(port.meta)
      Seq(
        linkNode,
        Some(new ParamNode(path.asIndirect + IndirectStep.IsConnected, ExprBuilder.ValInit.Boolean)),
        port.contains.ports.getOrElse(elem.PortArray.Ports()).ports.sortKeysFrom(nameOrder).map {
          case (name, subport) => PortLikeNode(path + name, subport, fromLink)
        },
      ).flatten
    }

    override def getColumns(index: Int): String = s"Array[${port.getSelfClass.toSimpleString}]"
  }

  def PortLikeNode(path: DesignPath, port: elem.PortLike, fromLink: Boolean=false): ElementDetailNode = {
    port.is match {
      case elem.PortLike.Is.Port(port) => new PortNode(path, port, fromLink)
      case elem.PortLike.Is.Bundle(port) => new BundleNode(path, port, fromLink)
      case elem.PortLike.Is.Array(port) => new ArrayNode(path, port, fromLink)
      case elem.PortLike.Is.LibElem(port) =>
        new UnelaboratedNode(path, s"unelaborated ${port.toSimpleString}")
      case _ =>
        new UnelaboratedNode(path, "unknown")
    }
  }


  def BlockNode(path: DesignPath, block: elem.HierarchyBlock): BlockNode = new BlockNode(path, block)

  class BlockNode(path: DesignPath, block: elem.HierarchyBlock) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = {  // don't recurse into blocks here
      val nameOrder = ProtoUtil.getNameOrder(block.meta)
      Seq(
        block.ports.sortKeysFrom(nameOrder).map {
          case (name, port) => PortLikeNode(path + name, port)
        },
        block.links.sortKeysFrom(nameOrder).map { case (name, sublink) =>
          LinkLikeNode(path + name, path.asIndirect + name, sublink)
        },
        block.meta.map { meta =>
          new MetadataNode("Metadata", meta)
        },
        Some(new ConstraintsNode(path, block.constraints.sortKeysFrom(nameOrder))),
        block.params.sortKeysFrom(nameOrder).map {
          case (name, param) => new ParamNode(path.asIndirect + name, param)
        },
      ).flatten
    }

    override def toString: String = path.lastString

    override def getColumns(index: Int): String = block.getSelfClass.toSimpleString
  }

  def BlockLikeNode(path: DesignPath, block: elem.BlockLike): ElementDetailNode = {
    block.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) => new BlockNode(path, block)
      case elem.BlockLike.Type.LibElem(block) =>
        new UnelaboratedNode(path, s"unelaborated ${block.toSimpleString}")
      case _ =>
        new UnelaboratedNode(path, "unknown")
    }
  }


  class LinkNode(path: DesignPath, relpath: IndirectDesignPath, link: elem.Link) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = {
      val nameOrder = ProtoUtil.getNameOrder(link.meta)
      Seq(
        Option.when(path.asIndirect == relpath) {  // only show ports if not CONNECTED_LINK
          link.ports.sortKeysFrom(nameOrder).map {
            case (name, port) => PortLikeNode(path + name, port, true)
          }
        }.toSeq.flatten,
        link.links.sortKeysFrom(nameOrder).map {
          case (name, sublink) => LinkLikeNode(path + name, relpath + name, sublink)
        },
        link.meta.map { meta =>
          new MetadataNode("Metadata", meta)
        },
        Some(new ConstraintsNode(path, link.constraints.sortKeysFrom(nameOrder))),
        link.params.sortKeysFrom(nameOrder).map {
          case (name, param) => new ParamNode(relpath + name, param)
        },
      ).flatten
    }

    override def toString: String = {
      if (relpath.steps.nonEmpty && relpath.steps.last == IndirectStep.ConnectedLink) {
        s"Connected @ ${path}"
      } else {
        path.lastString
      }
    }

    override def getColumns(index: Int): String = link.getSelfClass.toSimpleString
  }

  def LinkLikeNode(path: DesignPath, relpath: IndirectDesignPath, link: elem.LinkLike): ElementDetailNode = {
    link.`type` match {
      case elem.LinkLike.Type.Link(link) => new LinkNode(path, relpath, link)
      case elem.LinkLike.Type.LibElem(link) =>
        new UnelaboratedNode(path, s"unelaborated ${link.toSimpleString}")
      case _ =>
        new UnelaboratedNode(path, "unknown")
    }
  }


  class ParamNode(path: IndirectDesignPath, param: init.ValInit) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = Seq()

    override def toString: String = path.steps match {
      case Seq() => ""
      case steps => steps.last.toString
    }

    override def getColumns(index: Int): String = {
      val typeName = param.`val` match {
        case init.ValInit.Val.Floating(_) => "float"
        case init.ValInit.Val.Boolean(_) => "boolean"
        case init.ValInit.Val.Integer(_) => "integer"
        case init.ValInit.Val.Range(_) => "range"
        case init.ValInit.Val.Text(_) => "text"
        case param => s"unknown ${param.getClass}"
      }
      val value = compiler.getParamValue(path) match {
        case Some(value) => value.toStringValue
        case None => "Unsolved"
      }
      s"$value ($typeName)"
    }
  }

  class ConstraintsNode(root: DesignPath, constraints: SeqMap[String, expr.ValueExpr])
      extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = constraints.map { case (name, value) =>
      new ConstraintNode(root, name, value) }.toSeq

    override def toString: String = "Constraints"

    override def getColumns(index: Int): String = constraints.size.toString
  }

  class ConstraintDetailNode(desc: String, path: IndirectDesignPath) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = Seq()

    override def toString: String = desc

    override def getColumns(index: Int): String = path.toString
  }

  class ConstraintNode(root: DesignPath, name: String, constraint: expr.ValueExpr)
      extends ElementDetailNode {
    private lazy val nameDescChildren: (String, String, Seq[ConstraintDetailNode]) = {
      val constraintStr = ExprToString(constraint)
      constraint.expr match {
        case expr.ValueExpr.Expr.Assign(constraint) =>  // special case for assign: show final value and missing
          compiler.evaluateExpr(root, constraint.getSrc) match {
            case ExprResult.Result(result) =>
              (s"$name ⇐ ${result.toStringValue}", constraintStr, Seq())
            case ExprResult.Missing(missing) =>
              (s"$name ⇐ unknown", constraintStr, missing.toSeq.map { param =>
                new ConstraintDetailNode("Missing param", param)
              })
          }
        case _ => (name, constraintStr, Seq())
      }
    }

    override lazy val children: Seq[ElementDetailNode] = nameDescChildren._3

    override def toString: String = nameDescChildren._1

    override def getColumns(index: Int): String = nameDescChildren._2
  }

  class MetadataNode(name: String, meta: common.Metadata) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = meta.meta match {
      case common.Metadata.Meta.Members(members) => members.node.map { case (subName, subMeta) =>
        new MetadataNode(subName, subMeta)
      }.toSeq
      case _ => Seq()
    }

    override def toString: String = name

    override def getColumns(index: Int): String = meta.meta match {
      case common.Metadata.Meta.Members(members) => "(dict)"
      case common.Metadata.Meta.BinLeaf(binary) => s"(binary, ${binary.size()} long)"
      case common.Metadata.Meta.TextLeaf(text) => s"$text (text)"
      case other => s"(unknown ${other.getClass.getSimpleName})"
    }
  }
}


class ElementDetailTreeModel(path: DesignPath, root: schema.Design, compiler: Compiler) extends SeqTreeTableModel[ElementDetailNode] {
  val (rootBlockPath, rootBlock) = EdgirUtils.resolveDeepestBlock(path, root)
  val rootNode: ElementDetailNode = new ElementDetailNodes(root, compiler).BlockNode(rootBlockPath, rootBlock)
  val COLUMNS = Seq("Item", "Value")

  // TreeView abstract methods
  //
  override def getRootNode: ElementDetailNode = rootNode

  override def getNodeChildren(node: ElementDetailNode): Seq[ElementDetailNode] = node.children

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

  override def getNodeValueAt(node: ElementDetailNode, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: ElementDetailNode, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: ElementDetailNode, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
