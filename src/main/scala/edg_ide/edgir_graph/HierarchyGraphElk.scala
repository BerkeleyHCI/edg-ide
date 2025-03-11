package edg_ide.edgir_graph

import com.intellij.openapi.diagnostic.Logger
import edg.ElemBuilder.LibraryPath
import edg.wir.DesignPath
import edg_ide.edgir_graph.ElkEdgirGraphUtils.TitleProperty
import edgir.elem.elem.HierarchyBlock
import org.eclipse.elk.alg.layered.options.{LayeredMetaDataProvider, LayeredOptions}
import org.eclipse.elk.core.RecursiveGraphLayoutEngine
import org.eclipse.elk.core.data.LayoutMetaDataService
import org.eclipse.elk.core.math.KVector
import org.eclipse.elk.core.options._
import org.eclipse.elk.core.util.BasicProgressMonitor
import org.eclipse.elk.graph._
import org.eclipse.elk.graph.properties.IProperty
import org.eclipse.elk.graph.util.ElkGraphUtil

import java.util
import scala.collection.SeqMap

class HierarchyGraphElk {}

object HierarchyGraphElk {
  LayoutMetaDataService.getInstance.registerLayoutMetaDataProviders(new LayeredMetaDataProvider)
  val engine = new RecursiveGraphLayoutEngine()

  private val logger = Logger.getInstance(classOf[HierarchyGraphElk])

  /** Creates a new ELK graph root node, preconfigured for hierarchy block layout
    */
  def makeGraphRoot(): ElkNode = {
    val root = ElkGraphUtil.createGraph()

    // TODO: maybe the layout options should be elsewhere?
    root.setProperty(CoreOptions.ALGORITHM, "org.eclipse.elk.layered")
    root.setProperty(CoreOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN)
    root.setProperty(LayeredOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN)
    root.setProperty(LayeredOptions.THOROUGHNESS, java.lang.Integer.valueOf(7))

    root.setProperty(
      CoreOptions.PORT_LABELS_PLACEMENT,
      util.EnumSet.of(
        PortLabelPlacement.INSIDE,
        PortLabelPlacement.NEXT_TO_PORT_IF_POSSIBLE
      )
    )
    root.setProperty(CoreOptions.NODE_SIZE_CONSTRAINTS, SizeConstraint.minimumSizeWithPorts)

    root
  }

  // Various helper functions to simplify graph building
  //
  protected def addNode(parent: ElkNode, name: String): ElkNode = {
    val node = ElkGraphUtil.createNode(parent)
    node.setIdentifier(name)

    // TODO: maybe the layout options should be elsewhere?
    node.setProperty(
      CoreOptions.PORT_LABELS_PLACEMENT,
      util.EnumSet.of(
        PortLabelPlacement.INSIDE,
        PortLabelPlacement.NEXT_TO_PORT_IF_POSSIBLE
      )
    )
    node.setProperty(CoreOptions.NODE_SIZE_CONSTRAINTS, SizeConstraint.minimumSizeWithPorts)
    node.setProperty(CoreOptions.NODE_SIZE_MINIMUM, new KVector(200, 20))

    node
  }

  protected def addPort(parent: ElkNode, name: String): ElkPort = {
    val port = ElkGraphUtil.createPort(parent)
    port.setIdentifier(name)

    port.setDimensions(10, 10) // TODO make configurable?

    port
  }

  protected def addEdge(
      parent: ElkNode,
      source: ElkConnectableShape,
      target: ElkConnectableShape
  ): ElkEdge = {
    // TODO some kind of naming?
    val edge = ElkGraphUtil.createEdge(parent)
    edge.getSources.add(source)
    edge.getTargets.add(target)

    edge
  }

  // Internal functions for converting HGraph* to ELK objects
  //
  trait PropertyMapper[NodeType, PortType, EdgeType] {
    type PropertyType
    val property: IProperty[PropertyType]
    def nodeConv(node: NodeType): Option[PropertyType]
    def portConv(port: PortType): Option[PropertyType]
    def edgeConv(edge: EdgeType): Option[PropertyType]
  }

  /** Converts a HGraphNode to a ELK node, returning a map of its ports
    */
  def HGraphNodeToElkNode[NodeType, PortType, EdgeType](
      node: HGraphNode[NodeType, PortType, EdgeType],
      name: String,
      parent: Option[ElkNode],
      mappers: Seq[PropertyMapper[NodeType, PortType, EdgeType]] = Seq()
  ): (ElkNode, SeqMap[Seq[String], ElkConnectableShape]) = {
    val elkNode = parent match {
      case Some(parent) => addNode(parent, name)
      case None => makeGraphRoot()
    }
    mappers.foreach { mapper =>
      mapper.nodeConv(node.data).foreach { mapperResult =>
        elkNode.setProperty(mapper.property, mapperResult)
      }
    }

    val title = Option(elkNode.getProperty(TitleProperty)).getOrElse(name)
    ElkGraphUtil
      .createLabel(title, elkNode)
      .setProperty(CoreOptions.NODE_LABELS_PLACEMENT, NodeLabelPlacement.outsideTopLeft())
    ElkGraphUtil
      .createLabel(node.data.toString, elkNode)
      .setProperty(CoreOptions.NODE_LABELS_PLACEMENT, NodeLabelPlacement.insideTopCenter())

    // Create ELK objects for members (blocks and ports)
    val myElkPorts = node.members.collect { case (childName, childElt: HGraphPort[PortType]) =>
      val childElkPort = addPort(elkNode, name)
      mappers.foreach { mapper =>
        mapper.portConv(childElt.data).foreach { mapperResult =>
          childElkPort.setProperty(mapper.property, mapperResult)
        }
      }

      val title = Option(childElkPort.getProperty(TitleProperty)).getOrElse(childName.mkString("."))
      ElkGraphUtil.createLabel(title, childElkPort)
      // TODO: currently only name label is displayed. Is there a sane way to display additional data?
      // ElkGraphUtil.createLabel(childElt.data.toString, childElkPort)
      childName -> childElkPort
    }

    val myElkChildren = node.members
      .collect {
        // really mapping values: HGraphMember => (path: Seq[String], ElkConnectableShape)
        case (childName, childElt: HGraphNode[NodeType, PortType, EdgeType]) =>
          val (childElkNode, childConnectables) =
            HGraphNodeToElkNode(childElt, childName.mkString("."), Some(elkNode), mappers)
          // Add the outer element into the inner namespace path
          childConnectables.map { case (childPath, childElk) =>
            childName ++ childPath -> childElk
          }
      }
      .flatten
      .toMap

    // Create edges
    val myElkElements =
      myElkPorts ++ myElkChildren // unify namespace, data structure should prevent conflicts
    node.edges.foreach { edge =>
      (myElkElements.get(edge.source), myElkElements.get(edge.target)) match {
        case (None, None) => logger.warn(s"edge with invalid source ${edge.source} and target ${edge.target}")
        case (None, _) => logger.warn(s"edge with invalid source ${edge.source}")
        case (_, None) => logger.warn(s"edge with invalid target ${edge.target}")
        case (Some(elkSource), Some(elkTarget)) =>
          val childEdge = addEdge(elkNode, elkSource, elkTarget)
          mappers.foreach { mapper =>
            mapper.edgeConv(edge.data).foreach { mapperResult =>
              childEdge.setProperty(mapper.property, mapperResult)
            }
          }
      }
    }

    (elkNode, myElkPorts)
  }

  /** Converts a HGraphNode to a ELK Node, and performs layout
    */
  def HGraphNodeToElk[NodeType, PortType, EdgeType](
      node: HGraphNode[NodeType, PortType, EdgeType],
      topName: String,
      mappers: Seq[PropertyMapper[NodeType, PortType, EdgeType]] = Seq(),
      makeRoot: Boolean = false
  ): ElkNode = {
    val root = if (makeRoot) {
      val root = makeGraphRoot()
      HGraphNodeToElkNode(node, topName, Some(root), mappers)
      root
    } else {
      val (root, rootConnectables) = HGraphNodeToElkNode(node, "design", None, mappers)
      root
    }

    engine.layout(root, new BasicProgressMonitor())
    root
  }

  def HBlockToElkNode(
      block: HierarchyBlock,
      blockPath: DesignPath = DesignPath(),
      depth: Int = 1,
      mappers: Seq[PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper]] = Seq()
  ): ElkNode = {
    // For now, this only updates the graph visualization, which can change with focus.
    // In the future, maybe this will also update or filter the design tree.
    val edgirGraph = EdgirGraph.blockToNode(blockPath, block)
    val highFanoutTransform = new RemoveHighFanoutEdgeTransform(
      6,
      Set(
        LibraryPath("edg.electronics_model.VoltagePorts.VoltageLink"),
        LibraryPath("edg.electronics_model.GroundPort.GroundLink")
      )
    )
    val blockGroupings = block.meta match {
      case Some(meta) => meta.meta.members.get.node.get("_block_diagram_grouping") match {
          case Some(meta) => meta.meta.members.get.node.map { case (name, group) =>
              name -> group.meta.textLeaf.get.split(',').map(_.strip()).toSeq
            }
          case None => Seq()
        }
      case None => Seq()
    }
    val transformedGraph = highFanoutTransform(
      GroupingTransform(
        CollapseLinkTransform(
          CollapseBridgeTransform(
            InferEdgeDirectionTransform(
              SimplifyPortTransform(
                PruneDepthTransform(edgirGraph, depth)
              )
            )
          )
        ),
        blockGroupings.to(SeqMap)
      )
    )

    val layoutGraphRoot = HierarchyGraphElk.HGraphNodeToElk(
      transformedGraph,
      blockPath.lastString,
      mappers,
      blockPath != DesignPath()
    ) // need to make a root so root doesn't have ports

    layoutGraphRoot
  }
}
