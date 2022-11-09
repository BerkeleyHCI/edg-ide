package edg_ide.swing

import org.eclipse.elk.core.options._
import org.eclipse.elk.graph._

import java.awt._
import java.awt.geom.AffineTransform
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}


class ElkNodePainter(var rootNode: ElkNode, var showTop: Boolean = false) {
  private var zoomLevel: Float = 1.0f
  private val margin: Int = 32
  private var selected: Set[ElkGraphElement] = Set()
  private var highlighted: Option[Set[ElkGraphElement]] = None

  def setZoom(zoom: Float): Unit = {
    zoomLevel = zoom
  }

  def setSelected(elts: Set[ElkGraphElement]): Unit = {
    selected = elts
  }

  def setHighlighted(elts: Option[Set[ElkGraphElement]]): Unit = {
    highlighted = elts
  }

  def setRootNode(node: ElkNode): Unit ={
    rootNode = node
  }

  def blendColor(baseColor: Color, topColor: Color, factor: Double): Color = {
    new Color(
      (baseColor.getRed * (1 - factor) + topColor.getRed * factor).toInt,
      (baseColor.getGreen * (1 - factor) + topColor.getGreen * factor).toInt,
      (baseColor.getBlue * (1 - factor) + topColor.getBlue * factor).toInt,
    )
  }

  // Modify the base graphics for filling some element, eg by highlighted status
  def fillGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
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


  // Modify the base graphics for drawing the outline (stroke) of some element, eg by highlighted status
  protected def strokeGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    if (element == rootNode && !showTop) { // completely transparent for root if not showing top
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(new Color(0, 0, 0, 0))
      newGraphics
    } else if (selected.contains(element)) { // emphasis for selected
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setStroke(new BasicStroke(3 / zoomLevel))
      newGraphics
    } else if (highlighted.isDefined && !highlighted.get.contains(element)) { // dimmed out if not highlighted
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(blendColor(background, newGraphics.getColor, 0.25))
      newGraphics
    } else {
      base
    }
  }


  // Modify the base graphics for drawing some text, eg by highlighted status
  def textGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    // Main difference is stroke isn't bolded
    if (element == rootNode && !showTop) { // completely transparent for root if not showing top
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(new Color(0, 0, 0, 0))
      newGraphics
    } else if (!selected.contains(element) &&
      highlighted.isDefined && !highlighted.get.contains(element)) { // dimmed out if not highlighted
      val newGraphics = base.create().asInstanceOf[Graphics2D]
      newGraphics.setColor(blendColor(background, newGraphics.getColor, 0.25))
      newGraphics
    } else {
      base
    }
  }

  // Utility functions
  def edgeSectionPairs[T](section: ElkEdgeSection): Seq[((Double, Double), (Double, Double))] = {
    val start = (section.getStartX, section.getStartY)
    val end = (section.getEndX, section.getEndY)
    val bends = section.getBendPoints.asScala.map { elkBendPoint =>
      (elkBendPoint.getX, elkBendPoint.getY)
    }
    val allPoints = Seq(start) ++ bends ++ Seq(end)

    allPoints.sliding(2).map { case point1 :: point2 :: Nil =>
      (point1, point2)
    }.toSeq
  }

  val EDGE_CLICK_WIDTH = 5.0f // how thick edges are for click detection purposes

  def getElementForLocation(x: Int, y: Int): Option[ElkGraphElement] = {
    def shapeContainsPoint(shape: ElkShape, point: (Double, Double)): Boolean = {
      (shape.getX <= point._1 && point._1 <= shape.getX + shape.getWidth) &&
        (shape.getY <= point._2 && point._2 <= shape.getY + shape.getHeight)
    }

    def lineClosestDist(line1: (Double, Double), line2: (Double, Double), point: (Double, Double)): Double = {
      // Adapted from https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
      val lengthSq = Math.pow(line2._1 - line1._1, 2) +
        Math.pow(line2._2 - line1._2, 2)
      if (lengthSq == 0) { // "line" is a point, return distance to the point
        Math.sqrt(Math.pow(point._1 - line1._1, 2) + Math.pow(point._2 - line1._2, 2)).toFloat
      } else {
        val dot = (point._1 - line1._1) * (line2._1 - line1._1) +
          (point._2 - line1._2) * (line2._2 - line1._2)
        val t = Math.max(0, Math.min(1, dot / lengthSq))
        val proj = (line1._1 + t * (line2._1 - line1._1),
          line1._2 + t * (line2._2 - line1._2))
        val dist = Math.sqrt(Math.pow(point._1 - proj._1, 2) + Math.pow(point._2 - proj._2, 2))
        dist.toFloat
      }
    }

    def edgeClosestDistance(edge: ElkEdge, point: (Double, Double)): Double = {
      edge.getSections.asScala.map { section =>
        edgeSectionPairs(section).map { case (line1, line2) =>
          lineClosestDist(line1, line2, point)
        }.min
      }.min
    }

    // Tests the clicked point against a node, returning either a sub-node, port, or edge
    def intersectNode(node: ElkNode, point: (Double, Double)): Option[ElkGraphElement] = {
      // Ports can be outside the main shape and can't be gated by the node shape test
      val nodePoint = (point._1 - node.getX, point._2 - node.getY) // transform to node space
      val containedPorts = node.getPorts.asScala.collect {
        case port if shapeContainsPoint(port, nodePoint) => port
      }

      // Test node, and if within node, recurse into children
      val containedNodes = if (shapeContainsPoint(node, point)) {
        val containedChildren = node.getChildren.asScala.flatMap(intersectNode(_, nodePoint))
        val edgeDistances = node.getContainedEdges.asScala.map { edge =>
          (edge, edgeClosestDistance(edge, nodePoint) * zoomLevel) // attach distance calculation
        }.sortBy(_._2) // sort to get closest to cursor
        val containedEdges = edgeDistances.collect { case (edge, dist)
          if dist <= EDGE_CLICK_WIDTH => edge // filter by maximum click distance
        }

        containedChildren ++ containedEdges ++ Seq(node)
      } else {
        Seq()
      }

      (containedPorts ++ containedNodes).headOption
    }

    val elkPoint = ((x - margin) / zoomLevel.toDouble, (y - margin) / zoomLevel.toDouble) // transform points to elk-space
    intersectNode(rootNode, elkPoint)
  }


  // Given a ElkLabel and placement (anchoring) constraints, return the x and y coordinates for where the
  // text should be drawn.
  def transformLabelCoords(g: Graphics2D, label: ElkLabel, placement: Set[NodeLabelPlacement]): (Double, Double) = {
    val fontMetrics = g.getFontMetrics(g.getFont)

    val textWidth = fontMetrics.stringWidth(label.getText)
    val textHeight = fontMetrics.getMaxAscent

    if (Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_TOP).subsetOf(placement)) {
      (label.getX + label.getWidth / 2 - textWidth / 2, // shift X to centerline
        label.getY + textHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_TOP,
      NodeLabelPlacement.OUTSIDE).subsetOf(placement)) { // inside means bottom-anchored
      (label.getX,
        label.getY + label.getHeight)
    } else if (Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_BOTTOM).subsetOf(placement)) {
      (label.getX + label.getWidth / 2 - textWidth / 2,
        label.getY + label.getHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_TOP).subsetOf(placement)) {
      (label.getX,
        label.getY + textHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_CENTER).subsetOf(placement)) {
      (label.getX,
        label.getY + label.getHeight / 2 + textHeight / 2)
    } else if (Set(NodeLabelPlacement.H_RIGHT, NodeLabelPlacement.V_CENTER).subsetOf(placement)) {
      (label.getX + label.getWidth - textWidth,
        label.getY + label.getHeight / 2 + textHeight / 2)
    } else { // fallback: center anchored
      (label.getX + label.getWidth / 2 - textWidth / 2,
        label.getY + label.getHeight / 2 + textHeight / 2)
    }
  }


  // Render an edge, including all its sections
  def paintEdge(parentG: Graphics2D, blockG: Graphics2D, nodeBackground: Color, edge: ElkEdge): Unit = {
    // HACK PATCH around a (probable?) ELK bug
    // If a self-edge between parent's ports, use parent's transforms
    // TODO: is this generally correct? it's good enough for what we need though
    val thisG = if (edge.getSources == edge.getTargets) {
      val edgeTargetBlockOption = edge.getSources.asScala.headOption.collect {
        case sourcePort: ElkPort => sourcePort.getParent
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
        thisG.drawLine(line1._1.toInt, line1._2.toInt,
          line2._1.toInt, line2._2.toInt
        )
      }
    }
  }


  // Render a node, including its labels, but not its ports
  def paintNode(g: Graphics2D, nodeBackground: Color, node: ElkNode): Unit = {
    val nodeX = node.getX.toInt
    val nodeY = node.getY.toInt

    fillGraphics(g, nodeBackground, node).fillRect(nodeX, nodeY,
      node.getWidth.toInt, node.getHeight.toInt)
    strokeGraphics(g, nodeBackground, node).drawRect(nodeX, nodeY,
      node.getWidth.toInt, node.getHeight.toInt)

    node.getLabels.asScala.foreach { label =>
      val (labelX, labelY) = transformLabelCoords(g, label,
        label.getProperty(CoreOptions.NODE_LABELS_PLACEMENT).asScala.toSet)
      textGraphics(g, nodeBackground, node).drawString(label.getText, (labelX + nodeX).toInt, (labelY + nodeY).toInt)
    }
  }


  // Render a port, including its labels
  def paintPort(g: Graphics2D, nodeBackground: Color, port: ElkPort): Unit = {
    fillGraphics(g, nodeBackground, port).fillRect(port.getX.toInt, port.getY.toInt,
      port.getWidth.toInt, port.getHeight.toInt)
    strokeGraphics(g, nodeBackground, port).drawRect(port.getX.toInt, port.getY.toInt,
      port.getWidth.toInt, port.getHeight.toInt)

    val labelPlacement = port.getProperty(CoreOptions.PORT_SIDE) match {
      case PortSide.NORTH => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_TOP)
      case PortSide.SOUTH => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_BOTTOM)
      case PortSide.WEST => Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_CENTER)
      case PortSide.EAST => Set(NodeLabelPlacement.H_RIGHT, NodeLabelPlacement.V_CENTER)
      case _ => Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_CENTER)
    }

    port.getLabels.asScala.foreach { label =>
      val (labelX, labelY) = transformLabelCoords(g, label, labelPlacement)
      textGraphics(g, nodeBackground, port).drawString(label.getText,
        (labelX + port.getX).toInt,
        (labelY + port.getY).toInt)
    }
  }


  def paintComponent(paintGraphics: Graphics, backGround: Color): Unit = {
    val scaling = new AffineTransform()
    scaling.scale(zoomLevel, zoomLevel)
    val scaledG = paintGraphics.create().asInstanceOf[Graphics2D]
    scaledG.translate(margin, margin)
    scaledG.transform(scaling)
    scaledG.setStroke(new BasicStroke(1 / zoomLevel)) // keep stroke at 1px

    // Keep the real font size constant, regardless of zoom
    val currentFont = scaledG.getFont
    val newFont = currentFont.deriveFont(currentFont.getSize / zoomLevel)
    scaledG.setFont(newFont)

    def paintBlock(containingG: Graphics2D, containingBackground: Color, node: ElkNode): Unit = {
      val nodeBackground = if (highlighted.isDefined && !highlighted.get.contains(node)) { // dimmed out
        blendColor(containingBackground, containingG.getColor, 0.375)
      } else {
        blendColor(containingBackground, containingG.getColor, 0.15)
      }

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
        paintEdge(strokeGraphics(containingG, nodeBackground, edge), strokeGraphics(nodeG, nodeBackground, edge),
          nodeBackground, edge)
      }
    }

    paintBlock(scaledG, backGround, rootNode)
  }
}
