package edg_ide.edgir_graph

import com.intellij.openapi.diagnostic.Logger
import org.eclipse.elk.alg.layered.options.{LayeredMetaDataProvider, LayeredOptions}
import org.eclipse.elk.core.RecursiveGraphLayoutEngine
import org.eclipse.elk.core.data.LayoutMetaDataService
import org.eclipse.elk.core.math.KVector
import org.eclipse.elk.core.options._
import org.eclipse.elk.core.util.BasicProgressMonitor
import org.eclipse.elk.graph._
import org.eclipse.elk.graph.util.ElkGraphUtil


class HierarchyGraphElk {
}


object HierarchyGraphElk {
  LayoutMetaDataService.getInstance.registerLayoutMetaDataProviders(new LayeredMetaDataProvider)
  val engine = new RecursiveGraphLayoutEngine()

  private val logger = Logger.getInstance(classOf[HierarchyGraphElk])

  /**
    * Creates a new ELK graph root node, preconfigured for hierarchy block layout
    */
  def makeGraphRoot(): ElkNode = {
    val root = ElkGraphUtil.createGraph()

    // TODO: maybe the layout options should be elsewhere?
    root.setProperty(CoreOptions.ALGORITHM, "org.eclipse.elk.layered")
    root.setProperty(CoreOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN)
    root.setProperty(LayeredOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN)
    root.setProperty(LayeredOptions.THOROUGHNESS, new java.lang.Integer(7))

    root.setProperty(CoreOptions.NODE_LABELS_PLACEMENT, NodeLabelPlacement.insideTopCenter)
    root.setProperty(CoreOptions.PORT_LABELS_PLACEMENT, PortLabelPlacement.INSIDE)
    root.setProperty(CoreOptions.PORT_LABELS_NEXT_TO_PORT_IF_POSSIBLE, new java.lang.Boolean(true))
    root.setProperty(CoreOptions.NODE_SIZE_CONSTRAINTS, SizeConstraint.minimumSizeWithPorts)

    root
  }

  // Various helper functions to simplify graph building
  //
  protected def addNode(parent: ElkNode, text: String): ElkNode = {
    val node = ElkGraphUtil.createNode(parent)

    // TODO: maybe the layout options should be elsewhere?
    node.setProperty(CoreOptions.NODE_LABELS_PLACEMENT, NodeLabelPlacement.insideTopCenter)
    node.setProperty(CoreOptions.PORT_LABELS_PLACEMENT, PortLabelPlacement.INSIDE)
    node.setProperty(CoreOptions.PORT_LABELS_NEXT_TO_PORT_IF_POSSIBLE, new java.lang.Boolean(true))

    node.setProperty(CoreOptions.NODE_SIZE_CONSTRAINTS, SizeConstraint.minimumSizeWithPorts)
    node.setProperty(CoreOptions.NODE_SIZE_MINIMUM, new KVector(200, 20))

    ElkGraphUtil.createLabel(text, node)

    node
  }

  protected def addPort(parent: ElkNode, text: String): ElkPort = {
    val port = ElkGraphUtil.createPort(parent)

    ElkGraphUtil.createLabel(text, port)
    port.setDimensions(10, 10)  // TODO make configurable?

    port
  }

  protected def addEdge(parent: ElkNode, source: ElkConnectableShape, target: ElkConnectableShape): ElkEdge = {
    val edge = ElkGraphUtil.createEdge(parent)
    edge.getSources.add(source)
    edge.getTargets.add(target)

    edge
  }

  // Internal functions for converting HGraph* to ELK objects
  //

  /**
    * Converts a HGraphNode to a ELK node, returning a map of its ports
    */
  def HGraphNodeToElkNode[NodeType, PortType, EdgeType](node: HGraphNode[NodeType, PortType, EdgeType],
                                                        name: String, parent: ElkNode):
      Map[Seq[String], ElkConnectableShape] = {
    val elkNode = addNode(parent, name)

    // Create ELK objects for members (blocks and ports)
    val myElkPorts = node.members.collect {
      case (childName, childElt: HGraphPort[PortType]) =>
        val childElkPort = addPort(elkNode, childName)
        Seq(childName) -> childElkPort
    }

    val myElkChildren = node.members.collect {
      // really mapping values: HGraphMember => (path: Seq[String], ElkConnectableShape)
      case (childName, childElt: HGraphNode[NodeType, PortType, EdgeType]) =>
        val childConnectables = HGraphNodeToElkNode(childElt, childName, elkNode)
        // Add the outer element into the inner namespace path
        childConnectables.map { case (childPath, childElk) =>
          Seq(childName) ++ childPath -> childElk
        }
    }.flatten.toMap

    // Create edges
    val myElkElements = myElkPorts ++ myElkChildren  // unify namespace, data structure should prevent conflicts
    node.edges.foreach { edge =>
      (myElkElements.get(edge.source), myElkElements.get(edge.target)) match {
        case (None, None) => logger.warn(s"edge with invalid source ${edge.source} and target ${edge.target}")
        case (None, _) => logger.warn(s"edge with invalid source ${edge.source}")
        case (_, None) => logger.warn(s"edge with invalid target ${edge.target}")
        case (Some(elkSource), Some(elkTarget)) => addEdge(elkNode, elkSource, elkTarget)
      }
    }

    myElkPorts
  }

  /**
    * Converts a HGraphNode to a ELK Node, and performs layout
    */
  def HGraphNodeToElk[NodeType, PortType, EdgeType](node: HGraphNode[NodeType, PortType, EdgeType]): ElkNode = {
    // TODO implement me and get rid of this dummy graph
    val root = makeGraphRoot()
    root.setIdentifier("root")

    HGraphNodeToElkNode(node, "design", root)

    engine.layout(root, new BasicProgressMonitor())

    root
  }
}

