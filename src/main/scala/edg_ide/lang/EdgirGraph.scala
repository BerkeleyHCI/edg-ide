package edg_ide

import edg.elem.elem.HierarchyBlock
import org.eclipse.elk.graph._
import org.eclipse.elk.graph.util.ElkGraphUtil
import org.eclipse.elk.core.options._
import org.eclipse.elk.core.data.LayoutMetaDataService
import org.eclipse.elk.core.RecursiveGraphLayoutEngine
import org.eclipse.elk.core.util.BasicProgressMonitor
import org.eclipse.elk.alg.layered.options.{LayeredMetaDataProvider, LayeredOptions}


object EdgirGraph {
  /**
    * Creates a new ELK graph root node, preconfigured for hierarchy block layout
    */
  protected def makeGraphRoot(): ElkNode = {
    val root = ElkGraphUtil.createGraph()
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
    node.setProperty(CoreOptions.NODE_LABELS_PLACEMENT, NodeLabelPlacement.insideTopCenter)
    node.setProperty(CoreOptions.PORT_LABELS_PLACEMENT, PortLabelPlacement.INSIDE)
    node.setProperty(CoreOptions.PORT_LABELS_NEXT_TO_PORT_IF_POSSIBLE, new java.lang.Boolean(true))

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

  /**
    * Lays out the graph, in-place
    */
  def layout(root: ElkNode): ElkNode = {
    LayoutMetaDataService.getInstance.registerLayoutMetaDataProviders(new LayeredMetaDataProvider)

    val engine = new RecursiveGraphLayoutEngine()
    engine.layout(root, new BasicProgressMonitor())
    root
  }

  def hierarchyBlockToGraph(block: HierarchyBlock): ElkNode = {
    val root = makeGraphRoot()

    // TODO actually implement this with conversion from block
    val b1 = addNode(root, "b1")
    val b1p1 = addPort(b1, "p1")
    val b1p2 = addPort(b1, "p2")

    val b2 = addNode(root, "b2")
    val b2p1 = addPort(b2, "p1")

    val edge = addEdge(root, b1p2, b2p1)

    root
  }
}