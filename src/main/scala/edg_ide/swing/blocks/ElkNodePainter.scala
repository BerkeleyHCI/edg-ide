package edg_ide.swing.blocks

import edg_ide.swing.ColorUtil
import edg_ide.swing.blocks.ElkNodeUtil.edgeSectionPairs
import org.eclipse.elk.core.options._
import org.eclipse.elk.graph._

import java.awt._
import java.awt.geom.AffineTransform
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

object ElkNodePainter {
  val margin: Int = 32 // margin in pixels, regardless of zoom level, so tunnel labels aren't cut off
}

class ElkNodePainter(rootNode: ElkNode, showTop: Boolean = false, zoomLevel: Float = 1.0f) {
  // Modify the base graphics for filling some element, eg by highlighted status
  protected def fillGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    if (element == rootNode && !showTop) { // completely transparent for root if not showing top
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(new Color(0, 0, 0, 0))
      newGraphics
    } else { // computation is handled by the background passed in
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(background)
      newGraphics
    }
  }

  protected def strokeGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    if (element == rootNode && !showTop) { // completely transparent for root if not showing top
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(new Color(0, 0, 0, 0))
      newGraphics
    } else {
      base
    }
  }

  protected def textGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    // Main difference is stroke isn't bolded
    if (element == rootNode && !showTop) { // completely transparent for root if not showing top
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(new Color(0, 0, 0, 0))
      newGraphics
    } else {
      base
    }
  }

  // Given a ElkLabel and placement (anchoring) constraints, return the x and y coordinates for where the
  // text should be drawn.
  def transformLabelCoords(
      g: Graphics2D,
      label: ElkLabel,
      placement: Set[NodeLabelPlacement]
  ): (Double, Double) = {
    val fontMetrics = g.getFontMetrics(g.getFont)

    val textWidth = fontMetrics.stringWidth(label.getText)
    val textHeight = fontMetrics.getMaxAscent

    if (Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_TOP).subsetOf(placement)) {
      (
        label.getX + label.getWidth / 2 - textWidth / 2, // shift X to centerline
        label.getY + textHeight
      )
    } else if (
      Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_TOP, NodeLabelPlacement.OUTSIDE).subsetOf(placement)
    ) { // inside means bottom-anchored
      (label.getX, label.getY + label.getHeight)
    } else if (Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_BOTTOM).subsetOf(placement)) {
      (label.getX + label.getWidth / 2 - textWidth / 2, label.getY + label.getHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_TOP).subsetOf(placement)) {
      (label.getX, label.getY + textHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_CENTER).subsetOf(placement)) {
      (label.getX, label.getY + label.getHeight / 2 + textHeight / 2)
    } else if (Set(NodeLabelPlacement.H_RIGHT, NodeLabelPlacement.V_CENTER).subsetOf(placement)) {
      (label.getX + label.getWidth - textWidth, label.getY + label.getHeight / 2 + textHeight / 2)
    } else { // fallback: center anchored
      (label.getX + label.getWidth / 2 - textWidth / 2, label.getY + label.getHeight / 2 + textHeight / 2)
    }
  }

  // Render an edge, including all its sections
  def paintEdge(parentG: Graphics2D, blockG: Graphics2D, nodeBackground: Color, edge: ElkEdge): Unit = {
    // HACK PATCH around a (probable?) ELK bug
    // If a self-edge between parent's ports, use parent's transforms
    // TODO: is this generally correct? it's good enough for what we need though
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

    edge.getSections.asScala.foreach { section =>
      edgeSectionPairs(section).foreach { case (line1, line2) =>
        thisG.drawLine(line1._1.toInt, line1._2.toInt, line2._1.toInt, line2._2.toInt)
      }
    }
  }

  // Render a node, including its labels, but not its ports
  def paintNode(g: Graphics2D, nodeBackground: Color, node: ElkNode): Unit = {
    val nodeX = node.getX.toInt
    val nodeY = node.getY.toInt

    fillGraphics(g, nodeBackground, node).fillRect(nodeX, nodeY, node.getWidth.toInt, node.getHeight.toInt)
    strokeGraphics(g, nodeBackground, node).drawRect(nodeX, nodeY, node.getWidth.toInt, node.getHeight.toInt)

    node.getLabels.asScala.foreach { label =>
      val (labelX, labelY) =
        transformLabelCoords(g, label, label.getProperty(CoreOptions.NODE_LABELS_PLACEMENT).asScala.toSet)
      textGraphics(g, nodeBackground, node).drawString(
        label.getText,
        (labelX + nodeX).toInt,
        (labelY + nodeY).toInt
      )
    }
  }

  // Render a port, including its labels
  def paintPort(g: Graphics2D, nodeBackground: Color, port: ElkPort): Unit = {
    fillGraphics(g, nodeBackground, port).fillRect(
      port.getX.toInt,
      port.getY.toInt,
      port.getWidth.toInt,
      port.getHeight.toInt
    )
    strokeGraphics(g, nodeBackground, port).drawRect(
      port.getX.toInt,
      port.getY.toInt,
      port.getWidth.toInt,
      port.getHeight.toInt
    )

    val labelPlacement = port.getProperty(CoreOptions.PORT_SIDE) match {
      case PortSide.NORTH => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_TOP)
      case PortSide.SOUTH => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_BOTTOM)
      case PortSide.WEST => Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_CENTER)
      case PortSide.EAST => Set(NodeLabelPlacement.H_RIGHT, NodeLabelPlacement.V_CENTER)
      case _ => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_CENTER)
    }

    port.getLabels.asScala.foreach { label =>
      val (labelX, labelY) = transformLabelCoords(g, label, labelPlacement)
      textGraphics(g, nodeBackground, port).drawString(
        label.getText,
        (labelX + port.getX).toInt,
        (labelY + port.getY).toInt
      )
    }
  }

  protected def getNodeBackground(
      containingG: Graphics2D,
      containingBackground: Color,
      node: ElkNode
  ): Color = {
    val nodeBackground = ColorUtil.blendColor(containingBackground, containingG.getColor, 0.15)
    nodeBackground
  }

  def paintBlock(containingG: Graphics2D, containingBackground: Color, node: ElkNode): Unit = {
    val nodeBackground = getNodeBackground(containingG, containingBackground, node)
    paintNode(containingG, nodeBackground, node)

    val nodeG = containingG.create().asInstanceOf[Graphics2D]
    nodeG.translate(node.getX, node.getY)

    node.getPorts.asScala.foreach { port =>
      paintPort(nodeG, nodeBackground, port)
    }

    node.getChildren.asScala.foreach { childNode =>
      paintBlock(nodeG, nodeBackground, childNode)
    }

    node.getContainedEdges.asScala.foreach { edge =>
      // containing is passed in here as a hack around Elk not using container coordinates
      // for self edges
      paintEdge(
        strokeGraphics(containingG, nodeBackground, edge),
        strokeGraphics(nodeG, nodeBackground, edge),
        nodeBackground,
        edge
      )
    }
  }

  def paintComponent(paintGraphics: Graphics, backGround: Color): Unit = {
    val scaling = new AffineTransform()
    scaling.scale(zoomLevel, zoomLevel)
    val scaledG = paintGraphics.create().asInstanceOf[Graphics2D]
    scaledG.translate(ElkNodePainter.margin, ElkNodePainter.margin)
    scaledG.transform(scaling)
    scaledG.setStroke(new BasicStroke(1 / zoomLevel)) // keep stroke at 1px

    // Keep the real font size constant, regardless of zoom
    val currentFont = scaledG.getFont
    val newFont = currentFont.deriveFont(currentFont.getSize / zoomLevel)
    scaledG.setFont(newFont)

    paintBlock(scaledG, backGround, rootNode)
  }
}
