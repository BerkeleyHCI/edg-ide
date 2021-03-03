package edg_ide.swing

import com.intellij.util.ui.UIUtil
import org.eclipse.elk.graph._

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.geom.AffineTransform
import java.awt._
import javax.swing.{JComponent, Scrollable, SwingUtilities}
import scala.collection.JavaConverters._


class JElkGraph(var rootNode: ElkNode, var showTop: Boolean = false)
    extends JComponent with Scrollable with Zoomable {
  var zoomLevel: Float = 1.0f

  override def setZoom(zoom: Float): Unit = {
    zoomLevel = zoom
  }
  override def getZoom = zoomLevel

  // Utility functions
  def edgeSectionPairs[T](section: ElkEdgeSection): Seq[((Double, Double), (Double, Double))] = {
    val start = (section.getStartX, section.getStartY)
    val end = (section.getEndX, section.getEndY)
    val bends = section.getBendPoints.asScala.map { elkBendPoint =>
      (elkBendPoint.getX, elkBendPoint.getY)
    }
    val allPoints = Seq(start) ++ bends ++ Seq(end)

    allPoints.sliding(2).map { case point1 :: point2 :: Nil =>
      (point1, point2)
    }.toSeq
  }


  setGraph(rootNode)

  def setGraph(newGraph: ElkNode): Unit = {
    rootNode = newGraph
    revalidate()
    repaint()
  }

  def getGraph: ElkNode = rootNode

  def paintEdge(g: Graphics2D, edge: ElkEdge, parentX: Int, parentY: Int): Unit = {
    edge.getSections.asScala.foreach { section =>
      edgeSectionPairs(section).foreach { case (line1, line2) =>
        g.drawLine(line1._1.toInt + parentX, line1._2.toInt + parentY,
          line2._1.toInt + parentX, line2._2.toInt + parentY
        )
      }
    }
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    val scaling = new AffineTransform()
    scaling.scale(zoomLevel, zoomLevel)
    val g = paintGraphics.create().asInstanceOf[Graphics2D]
    g.transform(scaling)
    g.setStroke(new BasicStroke(1/zoomLevel))  // keep stroke at 1px

    // Keep the real font size constant, regardless of zoom
    val currentFont = g.getFont
    val newFont = currentFont.deriveFont(currentFont.getSize / zoomLevel)
    g.setFont(newFont)

    val fontMetrics = g.getFontMetrics(g.getFont)

    def paintBlock(node: ElkNode, parentX: Int, parentY: Int): Unit = {
      val nodeX = parentX + node.getX.toInt
      val nodeY = parentY + node.getY.toInt

      val rectG = g.create()
      if (selected.contains(node)) {  // emphasis for selected element
        // TODO different color?
      }
      rectG.setColor(UIUtil.shade(rectG.getColor, 1, 0.15))
      rectG.fillRect(nodeX, nodeY,
        node.getWidth.toInt, node.getHeight.toInt)

      val rectStrokeG = g.create().asInstanceOf[Graphics2D]
      if (selected.contains(node)) {  // emphasis for selected element
        rectStrokeG.setStroke(new BasicStroke(3/zoomLevel))  // TODO: maybe should be based off the current stroke?
      }
      rectStrokeG.drawRect(nodeX, nodeY,
        node.getWidth.toInt, node.getHeight.toInt)

      node.getLabels.asScala.foreach { label =>
        // convert the center x, y to top left aligned coordinates
        val labelX = (label.getX + label.getWidth / 2).toInt - fontMetrics.stringWidth(label.getText) / 2
        val labelY = (label.getY + label.getHeight / 2).toInt + fontMetrics.getHeight / 2

        g.drawString(label.getText, labelX + nodeX, labelY + nodeY)
      }

      node.getPorts.asScala.foreach { port =>
        val portStrokeG = g.create().asInstanceOf[Graphics2D]
        if (selected.contains(port)) {  // emphasis for selected element
          portStrokeG.setStroke(new BasicStroke(3))
        }
        portStrokeG.drawRect(nodeX + port.getX.toInt, nodeY + port.getY.toInt,
          port.getWidth.toInt, port.getHeight.toInt)

        port.getLabels.asScala.foreach { label =>
          // convert the center x, y to top left aligned coordinates
          val labelX = (label.getX + label.getWidth / 2).toInt - fontMetrics.stringWidth(label.getText) / 2
          val labelY = (label.getY + label.getHeight / 2).toInt + fontMetrics.getHeight / 2

          g.drawString(label.getText, labelX + nodeX + port.getX.toInt, labelY + nodeY + port.getY.toInt)
        }
      }
      paintBlockContents(node, parentX, parentY)
    }

    def paintBlockContents(node: ElkNode, parentX: Int, parentY: Int): Unit = {
      val nodeX = parentX + node.getX.toInt
      val nodeY = parentY + node.getY.toInt

      node.getChildren.asScala.foreach { childNode =>
        paintBlock(childNode, nodeX, nodeY)
      }

      node.getContainedEdges.asScala.foreach { edge =>
        val edgeStrokeG = g.create().asInstanceOf[Graphics2D]
        if (selected.contains(edge)) {  // emphasis for selected element
          edgeStrokeG.setStroke(new BasicStroke(3))
        }
        paintEdge(edgeStrokeG, edge, nodeX, nodeY)
      }
    }

    if (showTop) {
      paintBlock(rootNode, 0, 0)
    } else {
      paintBlockContents(rootNode, 0, 0)
    }
  }

  // support for mouse drag: https://docs.oracle.com/javase/tutorial/uiswing/components/scrollpane.html
  setAutoscrolls(true)
  // TODO proper drag support

  val EDGE_CLICK_WIDTH = 3.0f  // how thick edges are for click detection purposes

  def getElementForLocation(x: Int, y: Int): Option[ElkGraphElement] = {
    def shapeContainsPoint(shape: ElkShape, point: (Double, Double)): Boolean = {
      (shape.getX <= point._1 && point._1 <= shape.getX + shape.getWidth) &&
          (shape.getY <= point._2 && point._2 <= shape.getY + shape.getHeight)
    }

    def lineClosestDist(line1: (Double, Double), line2: (Double, Double), point: (Double, Double)): Double = {
      // Adapted from https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
      val lengthSq = Math.pow(line2._1 - line1._1, 2) +
          Math.pow(line2._2 - line1._2, 2)
      if (lengthSq == 0) {  // "line" is a point, return distance to the point
        Math.sqrt(Math.pow(point._1 - line1._1, 2) + Math.pow(point._2 - line1._2, 2)).toFloat
      } else {
        val dot = (point._1 - line1._1) * (line2._1 - line1._1) +
            (point._2 - line1._2) * (line2._2 - line1._2)
        val t = Math.max(0, Math.min(1, dot / lengthSq))
        val proj = (line1._1 + t * (line2._1 - line1._1),
            line1._2 + t * (line2._2 - line1._2))
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
      val nodePoint = (point._1 - node.getX, point._2 - node.getY)  // transform to node space
      val containedPorts = node.getPorts.asScala.collect {
        case port if shapeContainsPoint(port, nodePoint) => port
      }

      // Test node, and if within node, recurse into children
      val containedNodes = if (shapeContainsPoint(node, point)) {
        val containedChildren = node.getChildren.asScala.flatMap(intersectNode(_, nodePoint))
        val edgeDistances = node.getContainedEdges.asScala.map { edge =>
          (edge, edgeClosestDistance(edge, nodePoint) * zoomLevel)  // attach distance calculation
        } .sortBy(_._2)  // sort to get closest to cursor
        val containedEdges = edgeDistances.collect { case (edge, dist)
          if dist <= EDGE_CLICK_WIDTH => edge  // filter by maximum click distance
        }

        containedChildren ++ containedEdges ++ Seq(node)
      } else {
        Seq()
      }

      (containedPorts ++ containedNodes).headOption
    }

    val elkPoint = (x / zoomLevel.toDouble, y / zoomLevel.toDouble)  // transform points to elk-space
    intersectNode(rootNode, elkPoint)
  }

  // Selection operations
  //
  private var selected: Set[ElkGraphElement] = Set()
  def setSelected(elts: Set[ElkGraphElement]): Unit = {
    selected = elts
    validate()
    repaint()
  }

  private var highlighted: Option[Set[ElkGraphElement]] = None
  def setHighlighted(elts: Option[Set[ElkGraphElement]]): Unit = {
    highlighted = elts
    validate()
    repaint()
  }

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      requestFocusInWindow()
    }
  })

  // Scrollable APIs
  //
  override def getPreferredSize: Dimension =
    new Dimension((rootNode.getWidth * zoomLevel).toInt, (rootNode.getHeight * zoomLevel).toInt)

  override def getPreferredScrollableViewportSize: Dimension = getPreferredSize

  override def getScrollableBlockIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1
  override def getScrollableUnitIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableTracksViewportWidth: Boolean = false
  override def getScrollableTracksViewportHeight: Boolean = false
}
