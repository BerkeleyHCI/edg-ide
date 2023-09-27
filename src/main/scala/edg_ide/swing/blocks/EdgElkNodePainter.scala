package edg_ide.swing.blocks

import edg.wir.DesignPath
import edg_ide.edgir_graph.ElkEdgirGraphUtils
import edg_ide.swing.DrawAnchored
import edg_ide.swing.blocks.ElementGraphicsModifier.withColorBlendBackground
import org.eclipse.elk.core.options.{CoreOptions, PortSide}
import org.eclipse.elk.graph._

import java.awt._
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
  protected val kWireColorBlendFactor = 0.75

  protected def drawGround(g: Graphics2D, x: Int, y: Int, anchor: DrawAnchored, size: Int): Unit = {
    val halfsize = size / 2
    require(halfsize > 0)
    anchor match {
      case DrawAnchored.Top | DrawAnchored.Bottom =>
        val dy = anchor match {
          case DrawAnchored.Bottom => -halfsize / 2
          case DrawAnchored.Top | _ => halfsize / 2
        }
        g.drawLine(x - halfsize, y, x + halfsize, y)
        g.drawLine(x - halfsize / 2, y + dy, x + halfsize / 2, y + dy)
        g.drawLine(x - halfsize / 3, y + dy * 2, x + halfsize / 3, y + dy * 2)
      case DrawAnchored.Left | DrawAnchored.Right =>
        val dx = anchor match {
          case DrawAnchored.Right => -halfsize / 2
          case DrawAnchored.Left | _ => halfsize / 2
        }
        g.drawLine(x, y - halfsize, x, y + halfsize)
        g.drawLine(x + dx, y - halfsize / 2, x + dx, y + halfsize / 2)
        g.drawLine(x + dx * 2, y - halfsize / 3, x + dx * 2, y + halfsize / 3)
      case _ => // ignored, invalid
    }
  }

  protected def drawRail(g: Graphics2D, x: Int, y: Int, anchor: DrawAnchored, size: Int): Unit = {
    val halfsize = size / 2
    anchor match {
      case DrawAnchored.Top | DrawAnchored.Bottom => g.drawLine(x - halfsize, y, x + halfsize, y)
      case DrawAnchored.Left | DrawAnchored.Right => g.drawLine(x, y - halfsize, x, y + halfsize)
      case _ => // ignored, invalid
    }
  }

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
      val targetPointOpt = edge.getSections.asScala.headOption.map { section =>
        val bend = section.getBendPoints.asScala.head
        (bend.getX, bend.getY, section.getStartX, section.getStartY)
      }

      val textG = textGraphics(colorStrokeModifier(baseG), edge)
      val anchorXYSizeOpt = targetPointOpt.collect {
        case (x, y, x1, y1) if (x1 == x) && (y >= y1) => (DrawAnchored.Top, x, y, y - y1)
        case (x, y, x1, y1) if (x1 == x) && (y < y1) => (DrawAnchored.Bottom, x, y, y1 - y)
        case (x, y, x1, y1) if (y1 == y) && (x >= x1) => (DrawAnchored.Left, x, y, x - x1)
        case (x, y, x1, y1) if (y1 == y) && (x < x1) => (DrawAnchored.Right, x, y, x1 - x)
      }

      anchorXYSizeOpt.foreach { case (anchor, x, y, size) =>
        val lastPath = edge.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property) match {
          case DesignPath(steps) => steps.lastOption.getOrElse("")
          case _ => ""
        }
        val propLabel = edge.getProperty(ElkEdgirGraphUtils.WireLabelMapper.WireLabelProperty)
        val label = if (lastPath.isEmpty || lastPath.startsWith("_") && propLabel.nonEmpty) {
          if (propLabel != "GND") {
            drawRail(textG, x.toInt, y.toInt, anchor, size.toInt)
            propLabel // use mapper-defined labels for anon / unnamed
          } else {
            drawGround(textG, x.toInt, y.toInt, anchor, size.toInt)
            "" // don't draw GND, which is handled by the symbol
          }
        } else {
          lastPath
        }
        DrawAnchored.drawLabel(detailLabelModifier(textG), label, (x, y), anchor)
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
      val (dx, dy) = port.getProperty(CoreOptions.PORT_SIDE) match { // have array extending inwards
        case PortSide.NORTH => (2, -2)
        case PortSide.SOUTH => (2, -2)
        case PortSide.EAST => (-2, 2)
        case PortSide.WEST | _ => (2, 2)
      }

      fillGraphics(g, port).fillRect(portX + dx * 2, portY + dy * 2, port.getWidth.toInt, port.getHeight.toInt)
      withColorBlendBackground(0.5)(strokeGraphics(g, port))
        .drawRect(portX + dx * 2, portY + dy * 2, port.getWidth.toInt, port.getHeight.toInt)

      fillGraphics(g, port).fillRect(portX + dx, portY + dy, port.getWidth.toInt, port.getHeight.toInt)
      withColorBlendBackground(0.75)(strokeGraphics(g, port))
        .drawRect(portX + dx, portY + dy, port.getWidth.toInt, port.getHeight.toInt)
    }
    super.paintPort(g, port)

    if (portInserts.contains(port)) { // if insert is specified, draw the arrow
      val (xPts, yPts) = insertArrowPoints(port)
      strokeGraphics(g, port).fillPolygon(xPts, yPts, 3)
    }
  }
}
