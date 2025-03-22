package edg_ide.edgir_graph

import com.intellij.openapi.diagnostic.Logger
import com.intellij.ui.JBColor
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, RangeValue, TextValue}
import edg.wir.{BlockConnectivityAnalysis, DesignPath}
import edg_ide.util.EdgirAnalysisUtils
import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}
import edgir.elem.elem

import java.awt.Color
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object ElkEdgirGraphUtils {
  import org.eclipse.elk.graph.properties.IProperty

  // Adds the DesignPathProperty, containing the node's DesignPath, to nodes.
  // Useful, for example, to resolve the ElkNode by walking from the root inwards given a path.
  object DesignPathMapper
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    type PropertyType = DesignPath

    object DesignPathProperty extends IProperty[DesignPath] {
      override def getDefault: DesignPath = null
      override def getId: String = this.getClass.getSimpleName
      override def getLowerBound: Comparable[_ >: DesignPath] = null
      override def getUpperBound: Comparable[_ >: DesignPath] = null
    }

    override val property: IProperty[DesignPath] = DesignPathProperty

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[DesignPath] = Some(node.data.path)
    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[DesignPath] = Some(port.data.path)
    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[DesignPath] =
      Some(edge.data.path)
  }

  object TitleProperty extends IProperty[String] {
    override def getDefault: String = null
    override def getId: String = this.getClass.getSimpleName
    override def getLowerBound: Comparable[_ >: String] = null
    override def getUpperBound: Comparable[_ >: String] = null
  }

  // Adds an optional title to nodes, here defaulting to the refdes (if applicable) and node name.
  class TitleMapper(compiler: Compiler)
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    type PropertyType = String
    override val property: IProperty[String] = TitleProperty

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[String] = node.data match {
      case BlockWrapper(path, block) =>
        val refdesStringMaybe = block.`type`.hierarchy
          .flatMap { block =>
            EdgirAnalysisUtils.getInnermostSubblock(node.data.path, block)
          }
          .flatMap { case (innerPath, innerBlock) =>
            compiler.getParamValue((innerPath + "fp_refdes").asIndirect)
          } match {
          case Some(TextValue(refdes)) => Some(f"${node.data.path.lastString}, $refdes")
          case _ => None
        }
        Some(refdesStringMaybe.getOrElse(node.data.path.lastString))
      case GroupWrapper(path, name) => Some(name)
      case _ => None // use default for non-blocks
    }

    // port name not supported, since ports are not hierarchical (can't just use the last path component)
    // and we don't have a link to the block (so can't do a path subtraction to get the port subpath)
    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[String] = None

    // edge labels don't currently exist
    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[String] = None
  }

  import org.eclipse.elk.core.options.PortSide
  // Generates port side based on the port type only, with a default fallback for bidirectional / unknown
  object SimplePortSideMapper extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    private val logger = Logger.getInstance(this.getClass)

    import org.eclipse.elk.core.options.CoreOptions.PORT_SIDE
    type PropertyType = PortSide
    override val property: IProperty[PortSide] = PORT_SIDE

    def getSimplePortSide(port: PortWrapper): Option[PortSide] = {
      val portType = BlockConnectivityAnalysis.typeOfPortLike(port.portLike)

      portType.toSimpleString match {
        case "Ground" => Some(PortSide.SOUTH)
        case "GroundReference" => Some(PortSide.SOUTH)

        case "VoltageSource" => Some(PortSide.EAST)
        case "VoltageSink" => Some(PortSide.NORTH)

        case "DigitalSource" => Some(PortSide.EAST)
        case "DigitalSink" => Some(PortSide.WEST)
        case "DigitalBidir" => None

        case "AnalogSource" => Some(PortSide.EAST)
        case "AnalogSink" => Some(PortSide.WEST)

        case "CanControllerPort" => Some(PortSide.EAST)
        case "CanTransceiverPort" => Some(PortSide.WEST)
        case "CanDiffPort" => None

        case "CrystalDriver" => Some(PortSide.EAST)
        case "CrystalPort" => Some(PortSide.WEST)

        case "I2cController" => Some(PortSide.EAST)
        case "I2cPullupPort" => Some(PortSide.EAST)
        case "I2cTarget" => Some(PortSide.WEST)

        case "SpeakerDriverPort" => Some(PortSide.EAST)
        case "SpeakerPort" => Some(PortSide.WEST)

        case "SpiController" => Some(PortSide.EAST)
        case "SpiPeripheral" => Some(PortSide.WEST)

        case "SwdHostPort" => Some(PortSide.EAST)
        case "SwdTargetPort" => Some(PortSide.WEST)

        case "UartPort" => None

        case "UsbHostPort" => Some(PortSide.EAST)
        case "UsbDevicePort" => Some(PortSide.WEST)
        case "UsbPassivePort" => Some(PortSide.WEST)

        case "I2sController" => Some(PortSide.EAST)
        case "I2sTargetReceiver" => Some(PortSide.WEST)

        case "TouchDriver" => Some(PortSide.EAST)
        case "TouchPadPort" => Some(PortSide.WEST)

        case "UsbCcPort" => None

        case "Passive" => None

        case port =>
          logger.warn(s"Unknown port type $port")
          None
      }
    }

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[PortSide] = None
    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[PortSide] = {
      Some(getSimplePortSide(port.data).getOrElse(PortSide.EAST))
    }
    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[PortSide] = None
  }

  // Generates a port side based on the port type, then incoming edges if available
  // Stores edges design-wide internally to be accessed when inferring port direction
  class PortSideMapper extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    import org.eclipse.elk.core.options.CoreOptions.PORT_SIDE
    type PropertyType = PortSide
    override val property: IProperty[PortSide] = PORT_SIDE
    val targetPorts = mutable.Set[Seq[String]]()

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[PortSide] = {
      node.edges.foreach { edge =>
        edge.target.foreach(target => targetPorts.add(path ++ target))
      }
      None
    }

    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[PortSide] = {
      SimplePortSideMapper.getSimplePortSide(port.data) match {
        case Some(side) => Some(side)
        case None if targetPorts.contains(path) => Some(PortSide.WEST)
        case None =>
          Some(PortSide.EAST)

      }
    }
    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[PortSide] = None
  }

  import org.eclipse.elk.core.options.PortConstraints
  object PortConstraintMapper
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    import org.eclipse.elk.core.options.CoreOptions.PORT_CONSTRAINTS
    type PropertyType = PortConstraints

    override val property: IProperty[PortConstraints] = PORT_CONSTRAINTS

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[PortConstraints] =
      Some(PortConstraints.FIXED_SIDE)
    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[PortConstraints] = None
    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[PortConstraints] = None
  }

  // Adds a port array-ness property
  object PortArrayMapper extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    type PropertyType = Boolean

    object PortArrayProperty extends IProperty[Boolean] {
      override def getDefault: Boolean = false
      override def getId: String = this.getClass.getSimpleName
      override def getLowerBound: Comparable[_ >: Boolean] = null
      override def getUpperBound: Comparable[_ >: Boolean] = null
    }

    override val property: IProperty[Boolean] = PortArrayProperty

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[Boolean] = None
    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[Boolean] =
      port.data.portLike.is match {
        case elem.PortLike.Is.Array(_) => Some(true)
        case _ => None
      }
    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[Boolean] = None
  }

  // Adds wire colors for common voltage rails, based on ATX wire colors
  object WireColorMapper {
    object WireColorProperty extends IProperty[Option[Color]] {
      override def getDefault: Option[Color] = None
      override def getId: String = this.getClass.getSimpleName
      override def getLowerBound: Comparable[_ >: Option[Color]] = null
      override def getUpperBound: Comparable[_ >: Option[Color]] = null
    }

    // roughly ATX power supply conventions
    protected def voltageRangeToColor(range: RangeValue): Option[Option[Color]] = range match {
      case RangeValue(min, max) if min >= 3.0 && max <= 3.6 =>
        Some(Some(new JBColor(new Color(255, 153, 51), new Color(255, 153, 51))))
      case RangeValue(min, max) if min >= 4.5 && max <= 5.5 => Some(Some(JBColor.RED))
      case RangeValue(min, max) if min >= 10.5 && max <= 14.5 =>
        Some(Some(new JBColor(new Color(204, 204, 0), new Color(255, 255, 51))))
      case RangeValue(0, 0) => Some(Some(JBColor.BLUE))
      case _ => None
    }
  }

  class WireColorMapper(compiler: Compiler)
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    type PropertyType = Option[Color]

    override val property: IProperty[Option[Color]] = WireColorMapper.WireColorProperty

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[Option[Color]] = None

    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[Option[Color]] = None

    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[Option[Color]] = {
      val linkTypeOpt = edge.data match {
        case EdgeLinkWrapper(path, linkLike) => linkLike.`type` match {
            case elem.LinkLike.Type.Link(link) => link.selfClass
            case elem.LinkLike.Type.LibElem(lib) => Some(lib)
            case elem.LinkLike.Type.Array(link) => link.selfClass
            case _ => None
          }
        case _ => None
      }
      linkTypeOpt.map(_.toSimpleString) match {
        case Some("VoltageLink") => compiler.getParamValue(edge.data.path.asIndirect + "voltage") match {
            case Some(range: RangeValue) => WireColorMapper.voltageRangeToColor(range)
            case _ => None
          }
        case Some("GroundLink") => compiler.getParamValue(edge.data.path.asIndirect + "voltage") match {
            case Some(range: RangeValue) => WireColorMapper.voltageRangeToColor(range)
            case _ => None
          }
        case _ => None
      }
    }
  }

  object WireLabelMapper {
    object WireLabelProperty extends IProperty[String] {
      override def getDefault: String = ""
      override def getId: String = this.getClass.getSimpleName
      override def getLowerBound: Comparable[_ >: String] = null
      override def getUpperBound: Comparable[_ >: String] = null
    }

    protected def voltageRangeToString(range: RangeValue): Option[String] = range match {
      case RangeValue(0, 0) => Some("GND")
      case RangeValue(min, max) if (max - min) / ((min + max) / 2) <= 0.25 => Some(f"${(min + max) / 2}%.02gv")
      case _ => None
    }
  }

  class WireLabelMapper(compiler: Compiler)
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    type PropertyType = String

    override val property: IProperty[String] = WireLabelMapper.WireLabelProperty

    override def nodeConv(
        path: Seq[String],
        node: HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
    ): Option[String] = None

    override def portConv(path: Seq[String], port: HGraphPort[PortWrapper]): Option[String] = None

    override def edgeConv(nodePath: Seq[String], edge: HGraphEdge[EdgeWrapper]): Option[String] = {
      val linkTypeOpt = edge.data match {
        case EdgeLinkWrapper(path, linkLike) => linkLike.`type` match {
            case elem.LinkLike.Type.Link(link) => link.selfClass
            case elem.LinkLike.Type.LibElem(lib) => Some(lib)
            case elem.LinkLike.Type.Array(link) => link.selfClass
            case _ => None
          }
        case _ => None
      }
      linkTypeOpt.map(_.toSimpleString) match {
        case Some("VoltageLink") => compiler.getParamValue(edge.data.path.asIndirect + "voltage") match {
            case Some(range: RangeValue) => WireLabelMapper.voltageRangeToString(range)
            case _ => None
          }
        case Some("GroundLink") => compiler.getParamValue(edge.data.path.asIndirect + "voltage") match {
            case Some(range: RangeValue) => WireLabelMapper.voltageRangeToString(range)
            case _ => None
          }
        case _ => None
      }
    }
  }

  /** From a root ElkNode structured with the DesignPathMapper property, tries to follow the DesignPath. Returns target
    * node(s) matching the path.
    */
  def follow(path: DesignPath, root: ElkNode): Seq[ElkGraphElement] = {
    def inner(elkNode: ElkNode): Seq[ElkGraphElement] = {
      if (elkNode.getProperty(DesignPathMapper.property) == path) { // reached target node
        Seq(elkNode)
      } else {
        val nextNodeResults = elkNode.getChildren.asScala.toSeq
          .filter { node =>
            node.getProperty(DesignPathMapper.property) match {
              case DesignPath(steps) => path.steps.startsWith(steps)
              case _ => false
            }
          }
          .flatMap { childNode =>
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
