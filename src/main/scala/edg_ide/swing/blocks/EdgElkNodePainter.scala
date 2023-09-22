package edg_ide.swing.blocks

import edg.wir.DesignPath
import edg_ide.edgir_graph.ElkEdgirGraphUtils
import edg_ide.swing.blocks.ElementGraphicsModifier.withColorBlendBackground
import edg_ide.swing.{ColorUtil, DrawAnchored}
import org.eclipse.elk.graph._

import java.awt._
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import scala.jdk.CollectionConverters.ListHasAsScala

// ELK h-block graph painter with modifications for EDG graphs:
// - support stub edges (edges to self), to emulate tunnels in schematics
// - support array ports, drawn as stacked ports
// - support array port request / insert indicator, drawn as arrow into the port
class EdgElkNodePainter(
    rootNode: ElkNode,
    showTop: Boolean = false,
    zoomLevel: Float = 1.0f,
    defaultGraphics: ElementGraphicsModifier = ElementGraphicsModifier.default,
    elementGraphics: Seq[(ElkGraphElement, ElementGraphicsModifier)] = Seq()
) extends ElkNodePainter(rootNode, showTop, zoomLevel, defaultGraphics, elementGraphics) {
  override protected def paintEdge(parentG: Graphics2D, blockG: Graphics2D, edge: ElkEdge): Unit = {
    super.paintEdge(parentG, blockG, edge)

    val thisG = if (edge.getSources == edge.getTargets) {
      val edgeTargetBlockOption = edge.getSources.asScala.headOption.collect { case sourcePort: ElkPort =>
        sourcePort.getParent
      }
      if (edgeTargetBlockOption == Some(edge.getContainingNode)) {
        parentG
      } else {
        blockG
      }
    } else {
      blockG
    }

    if (edge.getSources == edge.getTargets) { // degenerate, "tunnel" (by heuristic / transform) edge
      val label = edge.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property) match {
        case DesignPath(steps) => steps.lastOption.getOrElse("")
        case _ => ""
      }

      val targetPointOpt = edge.getSections.asScala.headOption.map { section =>
        val bend = section.getBendPoints.asScala.head
        (bend.getX, bend.getY, section.getStartX, section.getStartY)
      }

      val textG = textGraphics(thisG, edge)
      targetPointOpt match {
        case Some((x, y, x1, y1)) if (x1 == x) && (y > y1) =>
          DrawAnchored.drawLabel(textG, label, (x, y), DrawAnchored.Top)
        case Some((x, y, x1, y1)) if (x1 == x) && (y < y1) =>
          DrawAnchored.drawLabel(textG, label, (x, y), DrawAnchored.Bottom)
        case Some((x, y, x1, y1)) if (y1 == y) && (x > x1) =>
          DrawAnchored.drawLabel(textG, label, (x, y), DrawAnchored.Left)
        case Some((x, y, x1, y1)) if (y1 == y) && (x < x1) =>
          DrawAnchored.drawLabel(textG, label, (x, y), DrawAnchored.Right)
        case Some((x, y, _, _)) =>
          DrawAnchored.drawLabel(textG, label, (x, y), DrawAnchored.Center)
        case None =>
      }
    }
  }

  override protected def paintPort(g: Graphics2D, port: ElkPort): Unit = {
    // if an array, render the array ports under it
    if (port.getProperty(ElkEdgirGraphUtils.PortArrayMapper.property)) {
      val portX = port.getX.toInt
      val portY = port.getY.toInt

      fillGraphics(g, port).fillRect(portX + 4, portY + 4, port.getWidth.toInt, port.getHeight.toInt)
      withColorBlendBackground(0.5)(strokeGraphics(g, port))
        .drawRect(portX + 4, portY + 4, port.getWidth.toInt, port.getHeight.toInt)

      fillGraphics(g, port).fillRect(portX + 2, portY + 2, port.getWidth.toInt, port.getHeight.toInt)
      withColorBlendBackground(0.75)(strokeGraphics(g, port))
        .drawRect(portX + 2, portY + 2, port.getWidth.toInt, port.getHeight.toInt)
    }
    super.paintPort(g, port)
  }
}
