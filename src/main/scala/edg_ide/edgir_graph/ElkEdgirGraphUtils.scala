package edg_ide.edgir_graph

import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.{BlockConnectivityAnalysis, DesignPath}
import edg_ide.EdgirUtils
import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}

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

      portType.toSimpleString match {
        case "VoltageSource" => Some(PortSide.EAST)
        case "VoltageSink" => portName match {
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

    override def nodeConv(node: NodeDataWrapper): Option[PortConstraints] = Some(PortConstraints.FIXED_SIDE)
    override def portConv(port: PortWrapper): Option[PortConstraints] = None
    override def edgeConv(edge: EdgeWrapper): Option[PortConstraints] = None
  }

  /** From a root ElkNode structured with the DesignPathMapper property, tries to follow the DesignPath.
    * Returns target node(s) matching the path.
    */
  def follow(path: DesignPath, root: ElkNode): Seq[ElkGraphElement] = {
    def inner(elkNode: ElkNode): Seq[ElkGraphElement] = {
      if (elkNode.getProperty(DesignPathMapper.property) == path) {  // reached target node
        Seq(elkNode)
      } else {
        val nextNodeResults = elkNode.getChildren.asScala.toSeq.filter { node =>
          node.getProperty(DesignPathMapper.property) match {
            case DesignPath(steps) => path.steps.startsWith(steps)
            case _ => false
          }
        }.flatMap { childNode =>
          inner(childNode)
        }

        val nextPorts = elkNode.getPorts.asScala.toSeq.filter { node =>
          node.getProperty(DesignPathMapper.property) == path
        }
        val nextEdges = elkNode.getContainedEdges.asScala.toSeq.filter { node =>
          node.getProperty(DesignPathMapper.property) == path
        }
        nextNodeResults ++ nextPorts ++ nextEdges
      }
    }

    inner(root)
  }
}
