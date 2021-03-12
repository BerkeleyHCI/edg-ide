package edg_ide.edgir_graph

import edg.wir.{BlockConnectivityAnalysis, DesignPath}
import edg_ide.EdgirUtils
import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._


object ElkEdgirGraphUtils {
  import org.eclipse.elk.graph.properties.IProperty
  object DesignPathMapper
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    type PropertyType = DesignPath

    object DesignPathProperty extends IProperty[DesignPath] {
      override def getDefault: DesignPath = null
      override def getId: String = "DesignPath"
      override def getLowerBound: Comparable[_ >: DesignPath] = null
      override def getUpperBound: Comparable[_ >: DesignPath] = null
    }

    override val property: IProperty[DesignPath] = DesignPathProperty

    override def nodeConv(node: NodeDataWrapper): Option[DesignPath] = Some(node.path)
    override def portConv(port: PortWrapper): Option[DesignPath] = Some(port.path)
    override def edgeConv(edge: EdgeWrapper): Option[DesignPath] = Some(edge.path)
  }

  import org.eclipse.elk.core.options.PortSide
  object PortSideMapper
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    import org.eclipse.elk.core.options.CoreOptions.PORT_SIDE
    type PropertyType = PortSide

    override val property: IProperty[PortSide] = PORT_SIDE

    override def nodeConv(node: NodeDataWrapper): Option[PortSide] = None
    override def portConv(port: PortWrapper): Option[PortSide] = {
      val portType = BlockConnectivityAnalysis.typeOfPortLike(port.portLike)
      val portName = port.path.steps.last

      EdgirUtils.SimpleLibraryPath(portType) match {
        case "ElectricalSource" => Some(PortSide.EAST)
        case "ElectricalSink" => portName match {
          case "gnd" | "vss" => Some(PortSide.SOUTH)
          case _ => Some(PortSide.NORTH)
        }

        case "DigitalSource" => Some(PortSide.EAST)
        case "DigitalSingleSource" => Some(PortSide.EAST)
        case "DigitalSink" => Some(PortSide.WEST)
        case "DigitalBidir" => None

        case "AnalogSource" => Some(PortSide.EAST)
        case "AnalogSink" => Some(PortSide.WEST)

        case "CanControllerPort" => Some(PortSide.EAST)
        case "CanTransceiverPort" => Some(PortSide.WEST)
        case "CanDiffPort" => None

        case "CrystalDriver" => Some(PortSide.EAST)
        case "CrystalPort" => Some(PortSide.WEST)

        case "I2cMaster" => Some(PortSide.EAST)
        case "I2cPullupPort" => Some(PortSide.EAST)
        case "I2cSlave" => Some(PortSide.WEST)

        case "SpeakerDriverPort" => Some(PortSide.EAST)
        case "SpeakerPort" => Some(PortSide.WEST)

        case "SpiMaster" => Some(PortSide.EAST)
        case "SpiSlave" => Some(PortSide.WEST)

        case "SwdHostPort" => Some(PortSide.EAST)
        case "SwdTargetPort" => Some(PortSide.WEST)

        case "UartPrt" => None

        case "UsbHostPort" => Some(PortSide.EAST)
        case "UsbDevicePort" => Some(PortSide.WEST)
        case "UsbPassivePort" => None

        case _ => None
      }
    }
    override def edgeConv(edge: EdgeWrapper): Option[PortSide] = None
  }

  import org.eclipse.elk.core.options.PortConstraints
  object PortConstraintMapper
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    import org.eclipse.elk.core.options.CoreOptions.PORT_CONSTRAINTS
    type PropertyType = PortConstraints

    override val property: IProperty[PortConstraints] = PORT_CONSTRAINTS

    override def nodeConv(node: NodeDataWrapper): Option[PortConstraints] = Some(PortConstraints.FIXED_ORDER)
    override def portConv(port: PortWrapper): Option[PortConstraints] = None
    override def edgeConv(edge: EdgeWrapper): Option[PortConstraints] = None
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
