package edg_ide.swing.blocks

import com.intellij.ui.JBColor
import edg.wir.DesignPath
import edg_ide.edgir_graph.ElkEdgirGraphUtils
import edg_ide.swing.blocks.ElementGraphicsModifier.withColorBlendBackground
import edg_ide.swing.{ColorUtil, DrawAnchored}
import org.eclipse.elk.core.options.{CoreOptions, PortSide}
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
    elementGraphics: Seq[(ElkGraphElement, ElementGraphicsModifier)] = Seq(),
    portInserts: Set[ElkGraphElement] = Set() // ports to draw insert indicators for
) extends ElkNodePainter(rootNode, showTop, zoomLevel, defaultGraphics, elementGraphics) {
  protected val kWireColorBlendFactor = 0.67

  override protected def paintEdge(
      parentG: Graphics2D,
      blockG: Graphics2D,
      edge: ElkEdge,
      isOutline: Boolean,
      strokeModifier: Graphics2D => Graphics2D = identity
  ): Unit = {
    val colorStrokeModifier = edge.getProperty(ElkEdgirGraphUtils.WireColorMapper.WireColorProperty) match {
      case Some(color) => ElementGraphicsModifier.withColor(color, kWireColorBlendFactor).andThen(strokeModifier)
      case None => strokeModifier
    }
    super.paintEdge(parentG, blockG, edge, isOutline, colorStrokeModifier)

    if (isOutline) return
    val baseG = getFixedEdgeBaseG(parentG, blockG, edge)
    if (edge.getSources == edge.getTargets) { // degenerate, "tunnel" (by heuristic / transform) edge
      val label = edge.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property) match {
        case DesignPath(steps) => steps.lastOption.getOrElse("")
        case _ => ""
      }

      val targetPointOpt = edge.getSections.asScala.headOption.map { section =>
        val bend = section.getBendPoints.asScala.head
        (bend.getX, bend.getY, section.getStartX, section.getStartY)
      }

      val textG = textGraphics(colorStrokeModifier(baseG), edge)
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

  // get the polygon points for the array insertion arrow, structured to be passed into g.drawPolygon
  protected def insertArrowPoints(port: ElkPort): (Array[Int], Array[Int]) = {
    val portCenterX = port.getX.toInt + port.getWidth.toInt / 2
    val portCenterY = port.getY.toInt + port.getHeight.toInt / 2
    val arrowLength = math.max(port.getWidth, port.getHeight).toInt
    val arrowHalfWidth = math.min(port.getWidth, port.getHeight).toInt / 2

    // points ordered as center, base1, base2
    port.getProperty(CoreOptions.PORT_SIDE) match {
      case PortSide.NORTH =>
        val portEdgeY = portCenterY - port.getHeight.toInt / 2
        (
          Array(portCenterX, portCenterX - arrowHalfWidth, portCenterX + arrowHalfWidth),
          Array(portEdgeY, portEdgeY - arrowLength, portEdgeY - arrowLength)
        )
      case PortSide.SOUTH =>
        val portEdgeY = portCenterY + port.getHeight.toInt / 2
        (
          Array(portCenterX, portCenterX - arrowHalfWidth, portCenterX + arrowHalfWidth),
          Array(portEdgeY, portEdgeY + arrowLength, portEdgeY + arrowLength)
        )
      case PortSide.EAST =>
        val portEdgeX = portCenterX + port.getWidth.toInt / 2
        (
          Array(portEdgeX, portEdgeX + arrowLength, portEdgeX + arrowLength),
          Array(portCenterY, portCenterY - arrowHalfWidth, portCenterY + arrowHalfWidth)
        )
      case PortSide.WEST | _ =>
        val portEdgeX = portCenterX - port.getWidth.toInt / 2
        (
          Array(portEdgeX, portEdgeX - arrowLength, portEdgeX - arrowLength),
          Array(portCenterY, portCenterY - arrowHalfWidth, portCenterY + arrowHalfWidth)
        )
    }
  }

  override protected def paintPort(
      g: Graphics2D,
      port: ElkPort
  ): Unit = {
    if (portInserts.contains(port)) { // if insert is specified, draw the arrow outline
      val (xPts, yPts) = insertArrowPoints(port)
      outlineGraphics(g, port).foreach { g => g.drawPolygon(xPts, yPts, 3) }
    }

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

    if (portInserts.contains(port)) { // if insert is specified, draw the arrow
      val (xPts, yPts) = insertArrowPoints(port)
      strokeGraphics(g, port).fillPolygon(xPts, yPts, 3)
    }
  }
}
