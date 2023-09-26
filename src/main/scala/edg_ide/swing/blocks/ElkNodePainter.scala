package edg_ide.swing.blocks

import edg_ide.swing.ColorUtil
import org.eclipse.elk.core.options._
import org.eclipse.elk.graph._

import java.awt.{Graphics2D, _}
import java.awt.geom.AffineTransform
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

case class ElementGraphicsModifier(
    // each is a graphics transformer, taking the prior one and returning the new one
    // the input will either be the base graphics object, or the output of the prior transformer
    // these may modify the graphics object in-place, or return a fresh one
    strokeGraphics: Graphics2D => Graphics2D = identity, // for the border
    fillGraphics: Graphics2D => Graphics2D = identity,
    textGraphics: Graphics2D => Graphics2D = identity, // for the label (if any)
    outlineGraphics: Option[Graphics2D => Graphics2D] =
      None // optional stroke below other elements, eg for hover highlight
)

object ElementGraphicsModifier {
  val kDefaultFillBlend = 0.15

  def default = ElementGraphicsModifier(
    fillGraphics = ElementGraphicsModifier.withColorBlendBackground(ElementGraphicsModifier.kDefaultFillBlend)
  )

  def makeTransformer(fn: Graphics2D => Unit): Graphics2D => Graphics2D = {
    def transform(g: Graphics2D): Graphics2D = {
      val newG = g.create().asInstanceOf[Graphics2D]
      fn(newG)
      newG
    }
    transform
  }

  // utility functions for creating graphics transformers
  def withColorBlendBackground(color: Color, factor: Double): Graphics2D => Graphics2D = makeTransformer { g =>
    g.setColor(ColorUtil.blendColor(g.getBackground, color, factor))
  }

  def withColorBlendBackground(factor: Double): Graphics2D => Graphics2D = makeTransformer { g =>
    g.setColor(ColorUtil.blendColor(g.getBackground, g.getColor, factor))
  }

  // blends the foreground color
  def withColor(color: Color, factor: Double = 1.0): Graphics2D => Graphics2D = makeTransformer { g =>
    g.setColor(ColorUtil.blendColor(g.getColor, color, factor))
  }

  def withStroke(stroke: Stroke): Graphics2D => Graphics2D = makeTransformer { g =>
    g.setStroke(stroke)
  }
}

object ElkNodePainter {
  val margin: Int = 32 // margin in pixels, regardless of zoom level, so tunnel labels aren't cut off
}

class ElkNodePainter(
    rootNode: ElkNode,
    showTop: Boolean = false,
    zoomLevel: Float = 1.0f,
    defaultGraphics: ElementGraphicsModifier = ElementGraphicsModifier.default,
    elementGraphics: Seq[(ElkGraphElement, ElementGraphicsModifier)] = Seq()
) {
  protected val modifiersByElement = elementGraphics.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  // Modify the base graphics for filling some element, eg by highlighted status
  protected def fillGraphics(base: Graphics2D, element: ElkGraphElement): Graphics2D = {
    val initialGraphics = base.create().asInstanceOf[Graphics2D]
    val modifiers = defaultGraphics +: modifiersByElement.getOrElse(element, Seq())
    modifiers.foldLeft(initialGraphics)((prev, elt) => elt.fillGraphics(prev))
  }

  protected def strokeGraphics(base: Graphics2D, element: ElkGraphElement): Graphics2D = {
    val initialGraphics = base.create().asInstanceOf[Graphics2D]
    val modifiers = defaultGraphics +: modifiersByElement.getOrElse(element, Seq())
    modifiers.foldLeft(initialGraphics)((prev, elt) => elt.strokeGraphics(prev))
  }

  protected def textGraphics(base: Graphics2D, element: ElkGraphElement): Graphics2D = {
    val initialGraphics = base.create().asInstanceOf[Graphics2D]
    val modifiers = defaultGraphics +: modifiersByElement.getOrElse(element, Seq())
    modifiers.foldLeft(initialGraphics)((prev, elt) => elt.textGraphics(prev))
  }

  protected def outlineGraphics(base: Graphics2D, element: ElkGraphElement): Option[Graphics2D] = {
    val modifiers = defaultGraphics +: modifiersByElement.getOrElse(element, Seq())
    val transformers = modifiers.flatMap(_.outlineGraphics)
    transformers match {
      case Seq() => None
      case transformers =>
        val initialGraphics = base.create().asInstanceOf[Graphics2D]
        Some(transformers.foldLeft(initialGraphics)((prev, elt) => elt(prev)))
    }
  }

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

  // HACK PATCH around a (probable?) ELK bug
  // If a self-edge between parent's ports, use parent's transforms
  // TODO: is this generally correct? it's good enough for what we need though
  protected def getFixedEdgeBaseG(parentG: Graphics2D, blockG: Graphics2D, edge: ElkEdge): Graphics2D = {
    if (edge.getSources == edge.getTargets) {
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
  }

  // Render an edge, including all its sections
  // containing is passed in here as a hack around Elk not using container coordinates for self edges
  protected def paintEdge(
      parentG: Graphics2D,
      blockG: Graphics2D,
      edge: ElkEdge,
      isOutline: Boolean,
      strokeModifier: Graphics2D => Graphics2D = identity
  ): Unit = {
    val modifierG: Graphics2D => Graphics2D = g =>
      if (isOutline) {
        outlineGraphics(g, edge).getOrElse {
          return
        }
      } else {
        strokeGraphics(g, edge)
      }
    val thisG = strokeModifier(modifierG(getFixedEdgeBaseG(parentG, blockG, edge)))

    edge.getSections.asScala.foreach { section =>
      val (pointsX, pointsY) = ElkNodeUtil.allPoints(section).unzip
      thisG.drawPolyline(pointsX.map(_.toInt).toArray, pointsY.map(_.toInt).toArray, pointsX.length)
    }
  }

  // Render a node, including its labels and its ports' labels (since they are spatially in the node)
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

    node.getPorts.asScala.foreach { port =>
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
          (labelX + port.getX + node.getX).toInt, // ports in node's coordinates
          (labelY + port.getY + node.getY).toInt
        )
      }
    }

    nodeFillGraphics.getColor
  }

  // Render a port, including its labels
  protected def paintPort(g: Graphics2D, port: ElkPort): Unit = {
    val portX = port.getX.toInt
    val portY = port.getY.toInt

    outlineGraphics(g, port).foreach { g => g.drawRect(portX, portY, port.getWidth.toInt, port.getHeight.toInt) }
    fillGraphics(g, port).fillRect(portX, portY, port.getWidth.toInt, port.getHeight.toInt)
    strokeGraphics(g, port).drawRect(portX, portY, port.getWidth.toInt, port.getHeight.toInt)
  }

  // paints the block and its contents
  protected def paintBlock(containingG: Graphics2D, node: ElkNode): Unit = {
    val nodeG = containingG.create().asInstanceOf[Graphics2D]
    nodeG.translate(node.getX, node.getY)

    node.getPorts.asScala.foreach { port =>
      paintPort(nodeG, port)
    }

    val nodeBackground = paintNode(containingG, node)
    nodeG.setBackground(nodeBackground)
    paintBlockContents(containingG, nodeG, node)
  }

  // paints the block's contents only
  protected def paintBlockContents(containingG: Graphics2D, nodeG: Graphics2D, node: ElkNode): Unit = {
    // paint all mouseover outlines below the main layer
    node.getContainedEdges.asScala.foreach { edge =>
      paintEdge(containingG, nodeG, edge, true)
    }
    node.getContainedEdges.asScala.foreach { edge =>
      paintEdge(containingG, nodeG, edge, false)
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
