package edg_ide.swing.blocks

import edg_ide.swing.ColorUtil
import edg_ide.swing.blocks.ElkNodeUtil.edgeSectionPairs
import org.eclipse.elk.core.options._
import org.eclipse.elk.graph._

import java.awt._
import java.awt.geom.AffineTransform
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

case class ElementGraphicsModifier(
  strokeGraphics: Option[Graphics2D => Graphics2D] = None,  // for the border
  fillGraphics: Option[Graphics2D => Graphics2D] = None,
  textGraphics: Option[Graphics2D => Graphics2D] = None,  // for the label (if any)
  outlineGraphics: Option[Graphics2D => Graphics2D] = None  // optional stroke below other elements, eg for hover highlight
)

object ElementGraphicsModifier {
  val kDefaultFillBlend = 0.15

  def default = ElementGraphicsModifier(
    strokeGraphics = Some(identity),
    fillGraphics = Some(ElementGraphicsModifier.withColorBlendBackground(ElementGraphicsModifier.kDefaultFillBlend)),
    textGraphics = Some(identity))

  // utility functions for creating graphics transformers
  def withColorBlendBackground(color: Color, factor: Double): Graphics2D => Graphics2D = {
    def transform(g: Graphics2D): Graphics2D = {
      val newG = g.create().asInstanceOf[Graphics2D]
      newG.setColor(ColorUtil.blendColor(newG.getBackground, color, factor))
      newG
    }
    transform
  }

  def withColorBlendBackground(factor: Double): Graphics2D => Graphics2D = {  // blends the foreground color
    def transform(g: Graphics2D): Graphics2D = {
      val newG = g.create().asInstanceOf[Graphics2D]
      newG.setColor(ColorUtil.blendColor(newG.getBackground, newG.getColor, factor))
      newG
    }
    transform
  }

  def withColor(color: Color): Graphics2D => Graphics2D = { // blends the foreground color
    def transform(g: Graphics2D): Graphics2D = {
      val newG = g.create().asInstanceOf[Graphics2D]
      newG.setColor(color)
      newG
    }
    transform
  }

  def withStroke(stroke: Stroke): Graphics2D => Graphics2D = {
    def transform(g: Graphics2D): Graphics2D = {
      val newG = g.create().asInstanceOf[Graphics2D]
      newG.setStroke(stroke)
      newG
    }
    transform
  }
}

object ElkNodePainter {
  val margin: Int = 32 // margin in pixels, regardless of zoom level, so tunnel labels aren't cut off
}

class ElkNodePainter(rootNode: ElkNode, showTop: Boolean = false, zoomLevel: Float = 1.0f,
                     defaultGraphics: ElementGraphicsModifier = ElementGraphicsModifier.default,
                     elementGraphics: Map[ElkGraphElement, ElementGraphicsModifier] = Map()) {
  // Modify the base graphics for filling some element, eg by highlighted status
  protected def fillGraphics(base: Graphics2D, element: ElkGraphElement): Graphics2D =
    elementGraphics.get(element).flatMap(_.fillGraphics).orElse(defaultGraphics.fillGraphics)
      .map(_(base)).getOrElse(base)

  protected def strokeGraphics(base: Graphics2D, element: ElkGraphElement): Graphics2D =
    elementGraphics.get(element).flatMap(_.strokeGraphics).orElse(defaultGraphics.strokeGraphics)
      .map(_(base)).getOrElse(base)

  protected def textGraphics(base: Graphics2D, element: ElkGraphElement): Graphics2D =
    elementGraphics.get(element).flatMap(_.textGraphics).orElse(defaultGraphics.textGraphics)
      .map(_(base)).getOrElse(base)

  protected def outlineGraphics(base: Graphics2D, element: ElkGraphElement): Option[Graphics2D] =
    elementGraphics.get(element).flatMap(_.outlineGraphics).orElse(defaultGraphics.outlineGraphics).map(_(base))

  // Given a ElkLabel and placement (anchoring) constraints, return the x and y coordinates for where the
  // text should be drawn.
  protected def transformLabelCoords(
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
  protected def paintEdge(parentG: Graphics2D, blockG: Graphics2D, edge: ElkEdge): Unit = {
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
  // Returns its fill color, which can be used for the background of inner nodes
  protected def paintNode(g: Graphics2D, node: ElkNode): Color = {
    val nodeX = node.getX.toInt
    val nodeY = node.getY.toInt

    outlineGraphics(g, node).foreach { g => g.drawRect(nodeX, nodeY, node.getWidth.toInt, node.getHeight.toInt) }
    val nodeFillGraphics = fillGraphics(g, node)
    nodeFillGraphics.fillRect(nodeX, nodeY, node.getWidth.toInt, node.getHeight.toInt)
    strokeGraphics(g, node).drawRect(nodeX, nodeY, node.getWidth.toInt, node.getHeight.toInt)

    node.getLabels.asScala.foreach { label =>
      val (labelX, labelY) =
        transformLabelCoords(g, label, label.getProperty(CoreOptions.NODE_LABELS_PLACEMENT).asScala.toSet)
      textGraphics(g, node).drawString(
        label.getText,
        (labelX + nodeX).toInt,
        (labelY + nodeY).toInt
      )
    }
    nodeFillGraphics.getColor
  }

  // Render a port, including its labels
  def paintPort(g: Graphics2D, port: ElkPort): Unit = {
    val portX = port.getX.toInt
    val portY = port.getY.toInt

    outlineGraphics(g, port).foreach { g => g.drawRect(portX, portY, port.getWidth.toInt, port.getHeight.toInt) }
    fillGraphics(g, port).fillRect(portX, portY, port.getWidth.toInt, port.getHeight.toInt)
    strokeGraphics(g, port).drawRect(portX, portY, port.getWidth.toInt, port.getHeight.toInt)

    val labelPlacement = port.getProperty(CoreOptions.PORT_SIDE) match {
      case PortSide.NORTH => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_TOP)
      case PortSide.SOUTH => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_BOTTOM)
      case PortSide.WEST => Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_CENTER)
      case PortSide.EAST => Set(NodeLabelPlacement.H_RIGHT, NodeLabelPlacement.V_CENTER)
      case _ => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_CENTER)
    }

    port.getLabels.asScala.foreach { label =>
      val (labelX, labelY) = transformLabelCoords(g, label, labelPlacement)
      textGraphics(g, port).drawString(
        label.getText,
        (labelX + port.getX).toInt,
        (labelY + port.getY).toInt
      )
    }
  }

  // paints the block and its contents
  protected def paintBlock(containingG: Graphics2D, node: ElkNode): Unit = {
    val nodeBackground = paintNode(containingG, node)

    val nodeG = containingG.create().asInstanceOf[Graphics2D]
    nodeG.translate(node.getX, node.getY)

    node.getPorts.asScala.foreach { port =>
      paintPort(nodeG, port)
    }

    nodeG.setBackground(nodeBackground)
    paintBlockContents(containingG, nodeG, node)
  }

  // paints the block's contents only
  protected def paintBlockContents(containingG: Graphics2D, nodeG: Graphics2D, node: ElkNode): Unit = {
    // paint all mouseover outlines below the main layer
    node.getContainedEdges.asScala.foreach { edge =>
      (outlineGraphics(containingG, edge), outlineGraphics(nodeG, edge)) match {
        case (Some(containingG), Some(nodeG)) =>
          paintEdge(
            strokeGraphics(containingG, edge),
            strokeGraphics(nodeG, edge),
            edge
          )
        case _ =>
      }
    }

    node.getContainedEdges.asScala.foreach { edge =>
      // containing is passed in here as a hack around Elk not using container coordinates for self edges
      paintEdge(
        strokeGraphics(containingG, edge),
        strokeGraphics(nodeG, edge),
        edge
      )
    }

    node.getChildren.asScala.foreach { childNode =>
      paintBlock(nodeG, childNode)
    }
  }

  def paintComponent(paintGraphics: Graphics): Unit = {
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

    if (showTop) {
      paintBlock(scaledG, rootNode)
    } else {
      paintBlockContents(scaledG, scaledG, rootNode)
    }
  }
}
