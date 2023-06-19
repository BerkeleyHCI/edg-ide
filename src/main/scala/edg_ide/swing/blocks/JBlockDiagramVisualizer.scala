package edg_ide.swing.blocks

import edg_ide.swing.Zoomable
import edg_ide.swing.blocks.ElkNodeUtil.edgeSectionPairs
import org.eclipse.elk.graph.{ElkEdge, ElkGraphElement, ElkNode, ElkShape}

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{BasicStroke, Color, Dimension, Graphics, Rectangle}
import javax.swing.{JComponent, Scrollable}
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

/** Block diagram visualizer that customizes the rendering with options specific to design block diagrams:
  *   - tunnel link names by heuristic matching (degenerate self-to-self links)
  *   - additional setError(elts) to render elts as filled in red
  *   - additional setStable(elts) to render elts as stale (???)
  */
class JBlockDiagramVisualizer(var rootNode: ElkNode, var showTop: Boolean = false)
    extends JComponent
    with Scrollable
    with Zoomable {
  private var zoomLevel: Float = 1.0f
  private var mouseOver: Set[ElkGraphElement] = Set()  // elements that are moused over, maintained internally
  private var selected: Set[ElkGraphElement] = Set()  // elements that are selected, set externally
  // if highlight is present, everything else is dimmed, non-selectable, and non-hoverable
  private var highlighted: Option[Set[ElkGraphElement]] = None
  private var errorElts: Set[ElkGraphElement] = Set()
  private var staleElts: Set[ElkGraphElement] = Set()
  private val elementToolTips = mutable.Map[ElkGraphElement, String]()

  override def getZoom = zoomLevel

  override def setZoom(zoom: Float): Unit = {
    zoomLevel = zoom
  }

  def setSelected(elts: Set[ElkGraphElement]): Unit = {
    selected = elts
    validate()
    repaint()
  }

  def setHighlighted(elts: Option[Set[ElkGraphElement]]): Unit = {
    highlighted = elts
    validate()
    repaint()
  }

  def setError(elts: Set[ElkGraphElement]): Unit = {
    errorElts = elts
    validate()
    repaint()
  }

  def setStale(elts: Set[ElkGraphElement]): Unit = {
    staleElts = elts
    validate()
    repaint()
  }

  setGraph(rootNode)

  def getGraph: ElkNode = rootNode

  def setGraph(newGraph: ElkNode): Unit = {
    errorElts = Set()
    staleElts = Set()
    elementToolTips.clear()
    highlighted = None
    selected = Set()
    rootNode = newGraph
    revalidate()
    repaint()
  }

  val EDGE_CLICK_WIDTH = 5.0f // how thick edges are for click detection purposes

  def getElementForLocation(x: Int, y: Int): Option[ElkGraphElement] = {
    def shapeContainsPoint(shape: ElkShape, point: (Double, Double)): Boolean = {
      (shape.getX <= point._1 && point._1 <= shape.getX + shape.getWidth) &&
      (shape.getY <= point._2 && point._2 <= shape.getY + shape.getHeight)
    }

    def lineClosestDist(line1: (Double, Double), line2: (Double, Double), point: (Double, Double)): Double = {
      // Adapted from https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
      val lengthSq = Math.pow(line2._1 - line1._1, 2) +
        Math.pow(line2._2 - line1._2, 2)
      if (lengthSq == 0) { // "line" is a point, return distance to the point
        Math.sqrt(Math.pow(point._1 - line1._1, 2) + Math.pow(point._2 - line1._2, 2)).toFloat
      } else {
        val dot = (point._1 - line1._1) * (line2._1 - line1._1) +
          (point._2 - line1._2) * (line2._2 - line1._2)
        val t = Math.max(0, Math.min(1, dot / lengthSq))
        val proj = (line1._1 + t * (line2._1 - line1._1), line1._2 + t * (line2._2 - line1._2))
        val dist = Math.sqrt(Math.pow(point._1 - proj._1, 2) + Math.pow(point._2 - proj._2, 2))
        dist.toFloat
      }
    }

    def edgeClosestDistance(edge: ElkEdge, point: (Double, Double)): Double = {
      edge.getSections.asScala.map { section =>
        edgeSectionPairs(section).map { case (line1, line2) =>
          lineClosestDist(line1, line2, point)
        }.min
      }.min
    }

    // Tests the clicked point against a node, returning either a sub-node, port, or edge
    def intersectNode(node: ElkNode, point: (Double, Double)): Option[ElkGraphElement] = {
      // Ports can be outside the main shape and can't be gated by the node shape test
      val nodePoint = (point._1 - node.getX, point._2 - node.getY) // transform to node space
      val containedPorts = node.getPorts.asScala.collect {
        case port if shapeContainsPoint(port, nodePoint) => port
      }

      // Test node, and if within node, recurse into children
      val containedNodes = if (shapeContainsPoint(node, point)) {
        val containedChildren = node.getChildren.asScala.flatMap(intersectNode(_, nodePoint))
        val edgeDistances = node.getContainedEdges.asScala
          .map { edge =>
            (edge, edgeClosestDistance(edge, nodePoint) * zoomLevel) // attach distance calculation
          }
          .sortBy(_._2) // sort to get closest to cursor
        val containedEdges = edgeDistances.collect {
          case (edge, dist) if dist <= EDGE_CLICK_WIDTH => edge // filter by maximum click distance
        }

        containedChildren ++ containedEdges ++ Seq(node)
      } else {
        Seq()
      }

      (containedPorts ++ containedNodes).headOption
    }

    val elkPoint =
      (
        (x - ElkNodePainter.margin) / zoomLevel.toDouble,
        (y - ElkNodePainter.margin) / zoomLevel.toDouble
      ) // transform points to elk-space
    intersectNode(rootNode, elkPoint)
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val errorModifier = ElementGraphicsModifier(
      fillGraphics = ElementGraphicsModifier.withColorBlendBackground(Color.RED, 0.25)
    )
    val selectedModifier = ElementGraphicsModifier(
      strokeGraphics = ElementGraphicsModifier.withStroke(new BasicStroke(3 / zoomLevel))
    )
    val elementGraphics = (
      errorElts.map { elt => elt -> errorModifier } ++
        selected.map { elt => elt -> selectedModifier }
      ).toMap
    val painter =
      new ElkNodePainter(rootNode, showTop, zoomLevel, elementGraphics=elementGraphics)
    //      new ModifiedElkNodePainter(rootNode, showTop, zoomLevel, errorElts, staleElts, selected, highlighted)
    painter.paintComponent(paintGraphics, this.getBackground)
  }

  // support for mouse drag: https://docs.oracle.com/javase/tutorial/uiswing/components/scrollpane.html
  setAutoscrolls(true)
  // TODO proper drag support

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      requestFocusInWindow()
    }
  })

  // Tooltip operations
  //
  def setElementToolTip(element: ElkGraphElement, text: String): Unit = {
    elementToolTips.put(element, text)
  }

  override def getToolTipText(e: MouseEvent): String = {
    getElementForLocation(e.getX, e.getY) match {
      case None => null
      case Some(element) =>
        elementToolTips.get(element) match {
          case None => null
          case Some(text) => text
        }
    }
  }

  override def getPreferredScrollableViewportSize: Dimension = getPreferredSize

  // Scrollable APIs
  //
  override def getPreferredSize: Dimension =
    new Dimension(
      (rootNode.getWidth * zoomLevel + 2 * ElkNodePainter.margin).toInt,
      (rootNode.getHeight * zoomLevel + 2 * ElkNodePainter.margin).toInt
    )

  override def getScrollableBlockIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableUnitIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableTracksViewportWidth: Boolean = false

  override def getScrollableTracksViewportHeight: Boolean = false
}
