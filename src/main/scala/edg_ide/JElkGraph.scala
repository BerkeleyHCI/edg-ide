package edg_ide

import scala.collection.JavaConverters._

import java.awt.{Dimension, Graphics}

import javax.swing.JComponent
import org.eclipse.elk.graph._


class JElkGraph(var graph: ElkNode) extends JComponent {
  setGraph(graph)

  def setGraph(newGraph: ElkNode): Unit = {
    graph = newGraph
    setMinimumSize(new Dimension(graph.getWidth.toInt, graph.getHeight.toInt))
    validate()
    repaint()
  }

  override def paintComponent(g: Graphics): Unit = {
    graph.getChildren.asScala.foreach { node =>
        g.drawRect(node.getX.toInt, node.getY.toInt,
          node.getWidth.toInt, node.getHeight.toInt)

      node.getPorts.asScala.foreach { port =>
        g.drawRect(port.getX.toInt, port.getY.toInt,
          port.getWidth.toInt, port.getHeight.toInt)
      }
    }
  }
}
