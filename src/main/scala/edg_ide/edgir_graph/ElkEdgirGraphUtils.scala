package edg_ide.edgir_graph

import com.intellij.ui.JBColor
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, RangeValue, TextValue}
import edg.wir.{BlockConnectivityAnalysis, DesignPath}
import edg_ide.util.EdgirAnalysisUtils
import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}
import edgir.elem.elem

import java.awt.Color
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

    override def nodeConv(node: NodeDataWrapper): Option[DesignPath] = Some(node.path)
    override def portConv(port: PortWrapper): Option[DesignPath] = Some(port.path)
    override def edgeConv(edge: EdgeWrapper): Option[DesignPath] = Some(edge.path)
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

    override def nodeConv(node: NodeDataWrapper): Option[String] = node match {
      case BlockWrapper(path, block) =>
        val refdesStringMaybe = block.`type`.hierarchy
          .flatMap { block =>
            EdgirAnalysisUtils.getInnermostSubblock(node.path, block)
          }
          .flatMap { case (innerPath, innerBlock) =>
            compiler.getParamValue((innerPath + "fp_refdes").asIndirect)
          } match {
          case Some(TextValue(refdes)) => Some(f"${node.path.lastString}, $refdes")
          case _ => None
        }
        Some(refdesStringMaybe.getOrElse(node.path.lastString))
      case _ => None // use default for non-blocks
    }

    // port name not supported, since ports are not hierarchical (can't just use the last path component)
    // and we don't have a link to the block (so can't do a path subtraction to get the port subpath)
    override def portConv(port: PortWrapper): Option[String] = None

    // edge labels don't currently exist
    override def edgeConv(edge: EdgeWrapper): Option[String] = None
  }

  import org.eclipse.elk.core.options.PortSide
  object PortSideMapper extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    import org.eclipse.elk.core.options.CoreOptions.PORT_SIDE
    type PropertyType = PortSide

    override val property: IProperty[PortSide] = PORT_SIDE

    override def nodeConv(node: NodeDataWrapper): Option[PortSide] = None
    override def portConv(port: PortWrapper): Option[PortSide] = {
      val portType = BlockConnectivityAnalysis.typeOfPortLike(port.portLike)
      val portName = port.path.steps.last

      portType.toSimpleString match {
        case "VoltageSource" => Some(PortSide.EAST)
        case "VoltageSink" =>
          portName match {
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

    override def nodeConv(node: NodeDataWrapper): Option[Boolean] = None
    override def portConv(port: PortWrapper): Option[Boolean] = port.portLike.is match {
      case elem.PortLike.Is.Array(_) => Some(true)
      case _ => None
    }
    override def edgeConv(edge: EdgeWrapper): Option[Boolean] = None
  }

  // Adds wire colors for common voltage rails, based on ATX wire colors
  object WireColorMapper {
    object WireColorProperty extends IProperty[Option[Color]] {
      override def getDefault: Option[Color] = None
      override def getId: String = this.getClass.getSimpleName
      override def getLowerBound: Comparable[_ >: Option[Color]] = null
      override def getUpperBound: Comparable[_ >: Option[Color]] = null
    }
  }

  class WireColorMapper(compiler: Compiler)
      extends HierarchyGraphElk.PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper] {
    type PropertyType = Option[Color]

    override val property: IProperty[Option[Color]] = WireColorMapper.WireColorProperty

    override def nodeConv(node: NodeDataWrapper): Option[Option[Color]] = None

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

    override def portConv(port: PortWrapper): Option[Option[Color]] = None

    override def edgeConv(edge: EdgeWrapper): Option[Option[Color]] = {
      val linkTypeOpt = edge match {
        case EdgeLinkWrapper(path, linkLike) => linkLike.`type` match {
            case elem.LinkLike.Type.Link(link) => link.selfClass
            case elem.LinkLike.Type.LibElem(lib) => Some(lib)
            case elem.LinkLike.Type.Array(link) => link.selfClass
            case _ => None
          }
        case _ => None
      }
      linkTypeOpt.map(_.toSimpleString) match {
        case Some("VoltageLink") => compiler.getParamValue(edge.path.asIndirect + "voltage") match {
            case Some(range: RangeValue) => voltageRangeToColor(range)
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
