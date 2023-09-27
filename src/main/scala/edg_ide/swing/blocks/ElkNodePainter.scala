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
  // label transparency by zoom scaling apply only to ports names and block types
  protected val kLabelDimStartZoom = 0.9f // zoom level at which labels start to become transparent
  protected val kLabelDimEndZoom = 0.5f // zoom level at which labels are at full transparency
  protected val kLabelDimTransparency = 32 // transparency at labelDimEndZoom
  protected val detailLabelModifier = {
    val factor =
      math.min(math.max((zoomLevel - kLabelDimEndZoom) / (kLabelDimStartZoom - kLabelDimEndZoom), 0), 1)

    ElementGraphicsModifier.makeTransformer(g =>
      g.setColor(ColorUtil.withAlpha(
        g.getColor,
        (factor * g.getColor.getAlpha + (1 - factor) * kLabelDimTransparency).toInt
      ))
    )
  }

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
  // If a self-edge between parent's ports, it needs to be in its parents coordinate space
  protected def getFixedEdgeOffset(edge: ElkEdge): (Double, Double) = {
    if (edge.getSources == edge.getTargets) {
      val edgeTargetBlockOption = edge.getSources.asScala.headOption.collect { case sourcePort: ElkPort =>
        sourcePort.getParent
      }
      if (edgeTargetBlockOption == Some(edge.getContainingNode)) {
        (-edge.getContainingNode.getX, -edge.getContainingNode.getY)
      } else {
        (0, 0)
      }
    } else {
      (0, 0)
    }
  }

  // Render an edge, including all its sections
  protected def paintEdge(
      g: Graphics2D,
      edge: ElkEdge,
      isOutline: Boolean,
      strokeModifier: Graphics2D => Graphics2D = identity
  ): Unit = {
    val edgeG = if (isOutline) {
      outlineGraphics(strokeModifier(g), edge).getOrElse {
        return
      }
    } else {
      strokeGraphics(strokeModifier(g), edge)
    }
    val (edgeOffX, edgeOffY) = getFixedEdgeOffset(edge)
    edge.getSections.asScala.foreach { section =>
      val (pointsX, pointsY) = ElkNodeUtil.allPoints(section).unzip
      edgeG.drawPolyline(
        pointsX.map(x => (x + edgeOffX).toInt).toArray,
        pointsY.map(y => (y + edgeOffY).toInt).toArray,
        pointsX.length
      )
    }
  }

  // Render a node, including its labels and its ports' labels (since they are spatially in the node)
  // Returns its fill color, which can be used for the background of inner nodes
  protected def paintNode(g: Graphics2D, node: ElkNode): Color = {
    outlineGraphics(g, node).foreach { g => g.drawRect(0, 0, node.getWidth.toInt, node.getHeight.toInt) }
    val nodeFillGraphics = fillGraphics(g, node)
    nodeFillGraphics.fillRect(0, 0, node.getWidth.toInt, node.getHeight.toInt)
    strokeGraphics(g, node).drawRect(0, 0, node.getWidth.toInt, node.getHeight.toInt)

    node.getLabels.asScala.zipWithIndex.foreach { case (label, i) =>
      val (labelX, labelY) =
        transformLabelCoords(g, label, label.getProperty(CoreOptions.NODE_LABELS_PLACEMENT).asScala.toSet)
      val textG = if (i == 0) { // first label is important, rest are detail
        textGraphics(g, node)
      } else {
        detailLabelModifier(textGraphics(g, node))
      }
      textG.drawString(label.getText, labelX.toInt, labelY.toInt)
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
        detailLabelModifier(textGraphics(g, port)).drawString(
          label.getText,
          (labelX + port.getX).toInt, // ports in node's coordinates
          (labelY + port.getY).toInt
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

    val nodeBackground = paintNode(nodeG, node)
    nodeG.setBackground(nodeBackground)
    paintBlockContents(nodeG, node)
  }

  // paints the block's contents only
  protected def paintBlockContents(g: Graphics2D, node: ElkNode): Unit = {
    // paint all mouseover outlines below the main layer
    node.getContainedEdges.asScala.foreach { edge =>
      paintEdge(g, edge, true)
    }
    node.getContainedEdges.asScala.foreach { edge =>
      paintEdge(g, edge, false)
    }

    node.getChildren.asScala.foreach { childNode =>
      paintBlock(g, childNode)
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
      paintBlockContents(scaledG, rootNode)
    }
  }
}
