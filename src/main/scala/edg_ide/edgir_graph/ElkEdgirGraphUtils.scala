package edg_ide.edgir_graph

import edg.wir.DesignPath
import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._


object ElkEdgirGraphUtils {
  import org.eclipse.elk.graph.properties.IProperty
  object DesignPathMapper
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper, DesignPath] {
    object DesignPathProperty extends IProperty[DesignPath] {
      override def getDefault: DesignPath = null
      override def getId: String = "DesignPath"
      override def getLowerBound: Comparable[_ >: DesignPath] = null
      override def getUpperBound: Comparable[_ >: DesignPath] = null
    }

    override val property: IProperty[DesignPath] = DesignPathProperty

    override def nodeConv(node: NodeDataWrapper): DesignPath = node.path
    override def portConv(port: PortWrapper): DesignPath = port.path
    override def edgeConv(edge: EdgeWrapper): DesignPath = edge.path
  }

  /** From a root ElkNode structured with the DesignPathMapper property, tries to follow the DesignPath.
    * Returns (nodes to target, target node).
    * target node: not None only if its path matches the input path
    * nodes to target: follows the DesignPath as far as possible, but may be non-empty even if target node is None.
    *   The last element is the target.
    */
  def follow(path: DesignPath, root: ElkNode): (Seq[ElkGraphElement], Option[ElkGraphElement]) = {

    def inner(nodePrefix: Seq[ElkGraphElement], elkNode: ElkNode): (Seq[ElkGraphElement], Option[ElkGraphElement]) = {
      if (elkNode.getProperty(DesignPathMapper.property) == path) {  // reached target node
        (nodePrefix :+ elkNode, Some(elkNode))
      } else {
        val nextChildNodes = elkNode.getChildren.asScala.filter { node =>
          node.getProperty(DesignPathMapper.property) match {
            case DesignPath(steps) => path.steps.startsWith(steps)
            case _ => false
          }
        }

        nextChildNodes.toSeq match {
          case Seq() =>  // continue to search my ports and edges
          case Seq(childNode) => return inner(nodePrefix :+ elkNode, childNode)  // exactly one next step
          case Seq(childNode, _) => return inner(nodePrefix :+ elkNode, childNode)  // multiple possible, just pick one
          // TODO maybe this should error or warn
        }

        val nextPorts = elkNode.getPorts.asScala.filter { node =>
          node.getProperty(DesignPathMapper.property) match {
            case DesignPath(steps) => path.steps.startsWith(steps)
            case _ => false
          }
        }
        val nextEdges = elkNode.getContainedEdges.asScala.filter { node =>
          node.getProperty(DesignPathMapper.property) match {
            case DesignPath(steps) => path.steps.startsWith(steps)
            case _ => false
          }
        }
        (nextPorts ++ nextEdges).toSeq match {
          case Seq() => (nodePrefix :+ elkNode, None)  // continue to search my ports and edges
          case Seq(childElt) if childElt.getProperty(DesignPathMapper.property) == path =>  // exact match
            (nodePrefix :+ elkNode :+ childElt, Some(childElt))
          case Seq(childElt) =>  // partial match
            (nodePrefix :+ elkNode :+ childElt, None)
          case Seq(childElt) if childElt.getProperty(DesignPathMapper.property) == path =>  // exact match
            (nodePrefix :+ elkNode :+ childElt, Some(childElt))  // TODO warning
          case Seq(childElt, _*) =>  // partial match
            (nodePrefix :+ elkNode :+ childElt, None)  // TODO warning
        }
      }
    }

    inner(Seq(), root)
  }
}
