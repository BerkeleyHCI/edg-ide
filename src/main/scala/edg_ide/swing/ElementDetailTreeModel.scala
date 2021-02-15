package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.ExprBuilder
import edg.elem.elem
import edg.expr.expr
import edg.init.init
import edg.schema.schema
import edg_ide.EdgirUtils
import edg.wir._
import edg.util.SeqMapSortableFrom._
import edg.compiler.{Compiler, ExprRef, ExprResult, ExprToString, ExprValue}

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree._
import scala.:+
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

  class PortNode(path: DesignPath, port: elem.PortLike,
                 root: schema.Design, compiler: Compiler, fromLink: Boolean=false) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = {
      val connectedNode = if (!fromLink) {
        compiler.getConnectedLink(path) match {
          case Some(linkPath) => EdgirUtils.resolveFromBlock(linkPath, root.getContents) match {
            case Some(link: elem.LinkLike) =>
              Seq(new LinkNode(linkPath, path.asIndirect + IndirectStep.ConnectedLink, link, root, compiler))
            case Some(target) =>
              Seq(new ElementDetailNode.Dummy(s"Unknown connected ${target.getClass} @ $linkPath"))
            case None =>
              Seq(new ElementDetailNode.Dummy(s"Invalid connected @ $path"))
          }
          case None => Seq(new ElementDetailNode.Dummy("Disconnected"))  // not connected
        }
      } else {
        Seq()
      }

      port.is match {
        case elem.PortLike.Is.Port(port) =>
          val nameOrder = ProtoUtil.getNameOrder(port.meta)
          (connectedNode :+
              new ParamNode(path.asIndirect + IndirectStep.IsConnected, ExprBuilder.ValInit.Boolean, compiler)) ++
              port.params.sortKeysFrom(nameOrder).map {
                case (name, param) => new ParamNode(path.asIndirect + name, param, compiler)
              }.toSeq
        case elem.PortLike.Is.Bundle(port) => port.superclasses.map(EdgirUtils.SimpleLibraryPath).mkString(", ")
          val nameOrder = ProtoUtil.getNameOrder(port.meta)
          (connectedNode :+
              new ParamNode(path.asIndirect + IndirectStep.IsConnected, ExprBuilder.ValInit.Boolean, compiler)) ++
              port.ports.sortKeysFrom(nameOrder).map {
                case (name, subport) => new PortNode(path + name, subport, root, compiler, fromLink)
              } ++
              port.params.sortKeysFrom(nameOrder).map {
                case (name, param) => new ParamNode(path.asIndirect + name, param, compiler)
              }
        case elem.PortLike.Is.Array(port) =>
          val nameOrder = ProtoUtil.getNameOrder(port.meta)
          (connectedNode :+
              new ParamNode(path.asIndirect + IndirectStep.IsConnected, ExprBuilder.ValInit.Boolean, compiler)) ++
              port.ports.sortKeysFrom(nameOrder).map {
                case (name, subport) => new PortNode(path + name, subport, root, compiler, fromLink)
              }.toSeq
        case _ => Seq()
      }
    }

    override def toString: String = {
      // TODO this should instead be replaced with a rendering thing
      val thisName = path.steps match {
        case Seq() => "(root)"
        case steps => steps.last
      }

      if (!fromLink) {
        compiler.getConnectedLink(path) match {
          case Some(linkPath) => s"$thisName @ $linkPath"
          case None => thisName
        }
      } else {
        thisName
      }
    }

    override def getColumns(index: Int): String = port.is match {
      case elem.PortLike.Is.Port(port) => port.superclasses.map(EdgirUtils.SimpleLibraryPath).mkString(", ")
      case elem.PortLike.Is.Bundle(port) => port.superclasses.map(EdgirUtils.SimpleLibraryPath).mkString(", ")
      case elem.PortLike.Is.Array(port) =>
        val superclasses = port.superclasses.map(EdgirUtils.SimpleLibraryPath).mkString(", ")
        s"Array[$superclasses]"
      case elem.PortLike.Is.LibElem(port) => "unelaborated " + EdgirUtils.SimpleLibraryPath(port)
      case _ => s"Unknown"
    }
  }

  class BlockNode(path: DesignPath, block: elem.BlockLike,
                  root: schema.Design, compiler: Compiler) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = block.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) =>  // don't recurse into blocks here
        val nameOrder = ProtoUtil.getNameOrder(block.meta)
        (((block.ports.sortKeysFrom(nameOrder).map {
          case (name, port) => new PortNode(path + name, port, root, compiler)
        } ++ block.links.sortKeysFrom(nameOrder).map { case (name, sublink) =>
              new LinkNode(path + name, path.asIndirect + name, sublink, root, compiler)
        }).toSeq :+ new ConstraintsNode(path, block.constraints.sortKeysFrom(nameOrder), compiler)
        ) ++ block.params.sortKeysFrom(nameOrder).map {
          case (name, param) => new ParamNode(path.asIndirect + name, param, compiler)
        }).toSeq
      case _ => Seq()
    }

    override def toString: String = path.steps match {
      case Seq() => "(root)"
      case steps => steps.last
    }

    override def getColumns(index: Int): String = block.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) => block.superclasses.map(EdgirUtils.SimpleLibraryPath).mkString(", ")
      case elem.BlockLike.Type.LibElem(block) => "unelaborated " + EdgirUtils.SimpleLibraryPath(block)
      case _ => s"Unknown"
    }
  }

  class LinkNode(path: DesignPath, relpath: IndirectDesignPath, link: elem.LinkLike,
                 root: schema.Design, compiler: Compiler) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = link.`type` match {
      case elem.LinkLike.Type.Link(link) =>
        val nameOrder = ProtoUtil.getNameOrder(link.meta)
        val portNodes = if (path.asIndirect != relpath) {
          // Don't display ports if this is a CONNECTED_LINK
          Seq()
        } else {
          link.ports.sortKeysFrom(nameOrder).map {
            case (name, port) => new PortNode(path + name, port, root, compiler, true)
          }
        }
        (((portNodes ++
            link.links.sortKeysFrom(nameOrder).map {
              case (name, sublink) => new LinkNode(path + name, relpath + name, sublink, root, compiler)
            }).toSeq :+ new ConstraintsNode(path, link.constraints.sortKeysFrom(nameOrder), compiler)
            ) ++ link.params.sortKeysFrom(nameOrder).map {
              case (name, param) => new ParamNode(relpath + name, param, compiler)
        }).toSeq
      case _ => Seq()
    }

    override def toString: String = {
      if (relpath.steps.nonEmpty && relpath.steps.last == IndirectStep.ConnectedLink) {
        s"Connected @ ${path.steps.mkString(".")}"
      } else {
        path.steps match {
          case Seq() => ""
          case steps => steps.last
        }
      }
    }

    override def getColumns(index: Int): String = link.`type` match {
      case elem.LinkLike.Type.Link(link) => link.superclasses.map(EdgirUtils.SimpleLibraryPath).mkString(", ")
      case elem.LinkLike.Type.LibElem(link) => "unelaborated " + EdgirUtils.SimpleLibraryPath(link)
      case _ => s"Unknown"
    }
  }

  class ParamNode(path: IndirectDesignPath, param: init.ValInit, compiler: Compiler) extends ElementDetailNode {
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

  class ConstraintsNode(root: DesignPath, constraints: SeqMap[String, expr.ValueExpr], compiler: Compiler)
      extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = constraints.map { case (name, value) =>
      new ConstraintNode(root, name, value, compiler) }.toSeq

    override def toString: String = "Constraints"

    override def getColumns(index: Int): String = constraints.size.toString
  }

  class ConstraintDetailNode(desc: String, path: IndirectDesignPath) extends ElementDetailNode {
    override lazy val children: Seq[ElementDetailNode] = Seq()

    override def toString: String = desc

    override def getColumns(index: Int): String = path.toString
  }

  class ConstraintNode(root: DesignPath, name: String, constraint: expr.ValueExpr, compiler: Compiler)
      extends ElementDetailNode {
    private lazy val nameDescChildren: (String, String, Seq[ConstraintDetailNode]) = {
      val constraintStr = ExprToString(constraint)
      constraint.expr match {
        case expr.ValueExpr.Expr.Assign(constraint) =>  // special case for assign: show final value and missing
          compiler.evaluateExpr(root, constraint.getSrc) match {
            case ExprResult.Result(result) =>
              (s"$name ⇐ ${result.toStringValue}", constraintStr, Seq())
            case ExprResult.Missing(missing) =>
              (s"$name ⇐ unknown", constraintStr, missing.toSeq.map {
                case ExprRef.Param(param) => new ConstraintDetailNode("Missing param", param)
                case ExprRef.Array(array) => new ConstraintDetailNode("Missing array", array.asIndirect)
              })
          }
        case _ => (name, constraintStr, Seq())
      }
    }

    override lazy val children: Seq[ElementDetailNode] = nameDescChildren._3

    override def toString: String = nameDescChildren._1

    override def getColumns(index: Int): String = nameDescChildren._2
  }
}


class ElementDetailTreeModel(path: DesignPath, root: schema.Design, compiler: Compiler) extends SeqTreeTableModel[ElementDetailNode] {
  val rootNode: ElementDetailNode = EdgirUtils.resolveFromBlock(path, root.getContents) match {
    case Some(block: elem.BlockLike) => new ElementDetailNode.BlockNode(path, block, root, compiler)
    case Some(port: elem.PortLike) => new ElementDetailNode.PortNode(path, port, root, compiler)
    case Some(link: elem.LinkLike) => new ElementDetailNode.LinkNode(path, path.asIndirect, link, root, compiler)
    case Some(target) =>
      new ElementDetailNode.Dummy(s"Unknown $target @ $path")
    case None =>
      new ElementDetailNode.Dummy(s"Invalid path @ $path")
  }
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
