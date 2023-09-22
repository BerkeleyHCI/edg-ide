package edg_ide.swing.blocks

import com.intellij.ui.JBColor
import edg_ide.swing.{ColorUtil, Zoomable}
import edg_ide.swing.blocks.ElkNodeUtil.edgeSectionPairs
import org.eclipse.elk.graph.{ElkEdge, ElkGraphElement, ElkNode, ElkPort, ElkShape}

import java.awt.event.{MouseAdapter, MouseEvent, MouseMotionAdapter}
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Dimension, Graphics, Graphics2D, Rectangle, TexturePaint}
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
  private var mouseOverElts: Seq[ElkGraphElement] = Seq() // elements that are moused over, maintained internally
  private var selected: Set[ElkGraphElement] = Set() // elements that are selected, set externally
  private var unselectable: Set[ElkGraphElement] = Set() // elements that are greyed and unselectable, set externally
  // if highlight is present, everything else is dimmed, non-selectable, and non-hoverable
  private var highlighted: Option[Set[ElkGraphElement]] = None
  private var portInserts: Set[ElkGraphElement] = Set() // ports to draw insert indicators for
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

  def setUnselectable(elts: Set[ElkGraphElement]): Unit = {
    unselectable = elts
    validate()
    repaint()
  }

  def setHighlighted(elts: Option[Set[ElkGraphElement]]): Unit = {
    highlighted = elts
    validate()
    repaint()
  }

  def setPortInserts(elts: Set[ElkGraphElement]): Unit = {
    portInserts = elts
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
    unselectable = Set()
    resetTransientSelections()
    rootNode = newGraph
    revalidate()
    repaint()
  }

  def resetTransientSelections(): Unit = {
    highlighted = None
    portInserts = Set()
    selected = Set()
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

  private val kDimBlend = 0.25

  private val errorModifier = ElementGraphicsModifier(
    fillGraphics = ElementGraphicsModifier.withColorBlendBackground(JBColor.RED, 0.5)
  )
  private val selectedModifier = ElementGraphicsModifier(
    strokeGraphics = ElementGraphicsModifier.withStroke(new BasicStroke(3 / zoomLevel))
  )
  private val dimGraphics = ElementGraphicsModifier(
    fillGraphics = ElementGraphicsModifier.default.fillGraphics, // needed since this replaces default
    strokeGraphics = ElementGraphicsModifier.withColorBlendBackground(kDimBlend),
    textGraphics = ElementGraphicsModifier.withColorBlendBackground(kDimBlend)
  )

  private def hatchFillTransform(nodeGraphics: Graphics2D): Graphics2D = {
    val hatchRect = new Rectangle2D.Double(0, 0, 16, 16)
    val hatchImage =
      new BufferedImage(hatchRect.width.toInt, hatchRect.height.toInt, BufferedImage.TYPE_INT_ARGB)
    val textureGraphics = hatchImage.createGraphics()
    val hatchTexture = new TexturePaint(hatchImage, hatchRect)
    textureGraphics.setColor(nodeGraphics.getColor) // use existing node color for background
    textureGraphics.fill(hatchRect)
    textureGraphics.setColor(ColorUtil.blendColor(nodeGraphics.getColor, JBColor.YELLOW, 0.25))
    textureGraphics.setStroke(new BasicStroke(2))
    textureGraphics.drawLine(0, 16, 16, 0)

    nodeGraphics.setPaint(hatchTexture)
    nodeGraphics
  }
  private val staleModifier = ElementGraphicsModifier(
    fillGraphics = hatchFillTransform
  )

  private val mouseoverModifier = ElementGraphicsModifier(
    outlineGraphics = Some(ElementGraphicsModifier.withStroke(
      new BasicStroke(9 / zoomLevel, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
    ).compose(
      ElementGraphicsModifier.withColor(ColorUtil.withAlpha(JBColor.BLUE, 127))
    ))
  )

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val elementGraphics = mouseOverElts.map { elt => elt -> mouseoverModifier } ++
      errorElts.map { elt => elt -> errorModifier } ++
      selected.map { elt => elt -> selectedModifier } ++
      staleElts.map { elt => elt -> staleModifier }
    // unselectable handled separately - when highlighting is active, it is treated as dimmed with the rest

    val backgroundPaintGraphics = paintGraphics.create().asInstanceOf[Graphics2D]
    backgroundPaintGraphics.setBackground(this.getBackground)
    val painter = highlighted match {
      case None => // normal rendering
        val innerElementGraphics = elementGraphics ++
          unselectable.map { elt => elt -> dimGraphics }
        new EdgElkNodePainter(
          rootNode,
          showTop,
          zoomLevel,
          elementGraphics = innerElementGraphics,
          portInserts = portInserts
        )
      case Some(highlighted) => // default dim rendering
        val highlightedElementGraphics = (highlighted -- unselectable).toSeq.map { elt =>
          elt -> ElementGraphicsModifier( // undo the dim rendering for highlighted
            strokeGraphics = ElementGraphicsModifier.withColor(getForeground),
            textGraphics = ElementGraphicsModifier.withColor(getForeground)
          )
        } ++ elementGraphics
        new EdgElkNodePainter(
          rootNode,
          showTop,
          zoomLevel,
          defaultGraphics = dimGraphics,
          elementGraphics = highlightedElementGraphics,
          portInserts = portInserts
        )
    }
    painter.paintComponent(backgroundPaintGraphics)
  }

  // support for mouse drag: https://docs.oracle.com/javase/tutorial/uiswing/components/scrollpane.html
  setAutoscrolls(true)
  // TODO proper drag support

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      requestFocusInWindow()
    }
  })

  addMouseMotionListener(new MouseMotionAdapter {
    private def mouseoverUpdated(newElts: Seq[ElkGraphElement]): Unit = {
      if (mouseOverElts != newElts) {
        mouseOverElts = newElts
        validate()
        repaint()
      }
    }

    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)

      val mouseoverElts = getElementForLocation(e.getX, e.getY).toSeq
        .filter(!unselectable.contains(_))
        .filter(highlighted match {
          case None => _ => true
          case Some(highlighted) => highlighted.contains(_) // dimmed items non-interactable
        })
      mouseoverUpdated(mouseoverElts)
    }
  })

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      onClick(e, mouseOverElts)
    }
  })

  // User hooks - can be overridden
  def onClick(e: MouseEvent, elts: Seq[ElkGraphElement]): Unit = {}

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
