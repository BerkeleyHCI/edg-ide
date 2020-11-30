package edg_ide

import scala.collection.JavaConverters._

import java.awt.{Dimension, FontMetrics, Graphics}

import javax.swing.JComponent
import org.eclipse.elk.graph._


class JElkGraph(var rootNode: ElkNode) extends JComponent {
  setGraph(rootNode)

  def setGraph(newGraph: ElkNode): Unit = {
    rootNode = newGraph
    setMinimumSize(new Dimension(rootNode.getWidth.toInt, rootNode.getHeight.toInt))
    validate()
    repaint()
  }

  override def paintComponent(g: Graphics): Unit = {
    val fontMetrics = g.getFontMetrics(g.getFont)

    def paintBlock(node: ElkNode, parentX: Int, parentY: Int): Unit = {
      val nodeX = parentX + node.getX.toInt
      val nodeY = parentY + node.getY.toInt

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
        paintEdge(edge, parentX, parentY)
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
}
