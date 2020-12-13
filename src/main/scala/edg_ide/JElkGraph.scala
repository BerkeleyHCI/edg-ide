package edg_ide

import scala.collection.JavaConverters._
import java.awt.{BasicStroke, Dimension, Graphics, Graphics2D, Rectangle}
import java.awt.event.{MouseAdapter, MouseEvent}
import com.intellij.util.ui.UIUtil

import javax.swing.{JComponent, Scrollable}
import org.eclipse.elk.graph._

import java.awt.geom.AffineTransform


class JElkGraph(var rootNode: ElkNode) extends JComponent with Scrollable with Zoomable {
  var zoomLevel: Float = 1.0f

  override def setZoom(zoom: Float): Unit = {
    zoomLevel = zoom
  }
  override def getZoom = zoomLevel


  setGraph(rootNode)

  def setGraph(newGraph: ElkNode): Unit = {
    rootNode = newGraph
    validate()
    repaint()
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
      rectG.setColor(UIUtil.shade(rectG.getColor, 1, 0.15))
      rectG.fillRect(nodeX, nodeY,
        node.getWidth.toInt, node.getHeight.toInt)

      g.drawRect(nodeX, nodeY,
        node.getWidth.toInt, node.getHeight.toInt)

      node.getLabels.asScala.foreach { label =>
        // convert the center x, y to top left aligned coordinates
        val labelX = (label.getX + label.getWidth / 2).toInt - fontMetrics.stringWidth(label.getText) / 2
        val labelY = (label.getY + label.getHeight / 2).toInt + fontMetrics.getHeight / 2

        g.drawString(label.getText, labelX + nodeX, labelY + nodeY)
      }

      node.getPorts.asScala.foreach { port =>
        g.drawRect(nodeX + port.getX.toInt, nodeY + port.getY.toInt,
          port.getWidth.toInt, port.getHeight.toInt)

        port.getLabels.asScala.foreach { label =>
          // convert the center x, y to top left aligned coordinates
          val labelX = (label.getX + label.getWidth / 2).toInt - fontMetrics.stringWidth(label.getText) / 2
          val labelY = (label.getY + label.getHeight / 2).toInt + fontMetrics.getHeight / 2

          g.drawString(label.getText, labelX + nodeX + port.getX.toInt, labelY + nodeY + port.getY.toInt)
        }
      }

      node.getChildren.asScala.foreach { childNode =>
        paintBlock(childNode, nodeX, nodeY)
      }

      node.getContainedEdges.asScala.foreach { edge =>
        paintEdge(edge, nodeX, nodeY)
      }
    }

    def paintEdge(edge: ElkEdge, parentX: Int, parentY: Int): Unit = {
      edge.getSections.asScala.foreach { section =>
        // these are still in parent-relative coordinates
        val start = (section.getStartX.toInt, section.getStartY.toInt)
        val end = (section.getEndX.toInt, section.getEndY.toInt)
        val bends = section.getBendPoints.asScala.map { elkBendPoint =>
          (elkBendPoint.getX.toInt, elkBendPoint.getY.toInt)
        }
        val allPoints = Seq(start) ++ bends ++ Seq(end)

        allPoints.sliding(2).foreach { case point1 :: point2 :: Nil =>
          g.drawLine(point1._1 + parentX, point1._2 + parentY,
            point2._1 + parentX, point2._2 + parentY
          )
        }
      }
    }

    // we don't just call paintBlock on the root because we don't want the containing frame
    rootNode.getChildren.asScala.foreach { childNode =>
      paintBlock(childNode, rootNode.getX.toInt, rootNode.getY.toInt)
    }
    rootNode.getContainedEdges.asScala.foreach{ edge =>
      paintEdge(edge, rootNode.getX.toInt, rootNode.getY.toInt)
    }
  }

  // support for mouse drag: https://docs.oracle.com/javase/tutorial/uiswing/components/scrollpane.html
  setAutoscrolls(true)
  // TODO proper drag support

  addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent) {
      def shapeContainsPoint(shape: ElkShape, point: (Float, Float)): Boolean = {
        (shape.getX <= point._1 && point._1 <= shape.getX + shape.getWidth) &&
          (shape.getY <= point._2 && point._2 <= shape.getY + shape.getHeight)
      }

      // Tests the clicked point against a node, returning either a sub-node, port, or edge
      def intersectNode(node: ElkNode, point: (Float, Float)): Option[ElkGraphElement] = {
        // Ports can be outside the main shape and can't be gated by the node shape test
        val nodePoint = (point._1 - node.getX.toFloat, point._2 - node.getY.toFloat)  // transform to node space
        val containedPorts = node.getPorts.asScala.collect {
          case port if shapeContainsPoint(port, nodePoint) => port
        }

        // Test node, and if within node, recurse into children
        val containedNodes = if (shapeContainsPoint(node, point)) {
          val containedNodes = node.getChildren.asScala.flatMap(intersectNode(_, nodePoint))
          // TODO handle edges

          containedNodes ++ Seq(node)
        } else {
          Seq()
        }

        (containedPorts ++ containedNodes).headOption
      }

      val elkPoint = (e.getX / zoomLevel, e.getY / zoomLevel)  // transform points to elk-space
      val clickedNode = intersectNode(rootNode, elkPoint)

      clickedNode.foreach { onNodeSelected }

      println(s"${e.getPoint.toString}  $clickedNode")
    }
  })

  def onNodeSelected(node: ElkGraphElement): Unit = {

  }

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
