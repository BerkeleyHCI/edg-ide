package edg_ide.swing.blocks

import edg.wir.DesignPath
import edg_ide.edgir_graph.ElkEdgirGraphUtils
import edg_ide.swing.{ColorUtil, DrawAnchored}
import org.eclipse.elk.graph._

import java.awt._
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import scala.jdk.CollectionConverters.ListHasAsScala

class ModifiedElkNodePainter(
    rootNode: ElkNode,
    showTop: Boolean = false,
    zoomLevel: Float = 1.0f,
    errorElts: Set[ElkGraphElement] = Set(),
    staleElts: Set[ElkGraphElement] = Set(),
    selected: Set[ElkGraphElement] = Set(),
    highlighted: Option[Set[ElkGraphElement]] = None
) extends ElkNodePainter(rootNode, showTop, zoomLevel) {
  private val hatchRect = new Rectangle2D.Double(0, 0, 16, 16)

  def makeHatchTexture(backgroundColor: Color, foregroundColor: Color): TexturePaint = {
    val hatchImage =
      new BufferedImage(hatchRect.width.toInt, hatchRect.height.toInt, BufferedImage.TYPE_INT_ARGB)
    val hatchGraphics = hatchImage.createGraphics()
    val hatchTexture = new TexturePaint(hatchImage, hatchRect)
    hatchGraphics.setColor(backgroundColor)
    hatchGraphics.fill(hatchRect)
    hatchGraphics.setColor(foregroundColor)
    hatchGraphics.setStroke(new BasicStroke(2))
    hatchGraphics.drawLine(0, 16, 16, 0)
    hatchTexture
  }

//  // Modify the base graphics for drawing some text, eg by highlighted status
//  override def textGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
//    // Main difference is stroke isn't bolded
//    if (element == rootNode && !showTop) { // completely transparent for root if not showing top
//      val newGraphics = base.create().asInstanceOf[Graphics2D]
//      newGraphics.setColor(new Color(0, 0, 0, 0))
//      newGraphics
//    } else if (
//      !selected.contains(element) &&
//      highlighted.isDefined && !highlighted.get.contains(element)
//    ) { // dimmed out if not highlighted
//      val newGraphics = base.create().asInstanceOf[Graphics2D]
//      newGraphics.setColor(ColorUtil.blendColor(background, newGraphics.getColor, 0.25))
//      newGraphics
//    } else {
//      base
//    }
//  }
//
//  // Modify the base graphics for filling some element, eg by highlighted status
//  override def fillGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
//    if (errorElts.contains(element)) {
////      val newBase = base.create().asInstanceOf[Graphics2D]
////      newBase.setColor(ColorUtil.blendColor(background, Color.RED, 0.25))
////      newBase // explicitly ignores showTop invisibility if it's an error
//      base
//    } else if (staleElts.contains(element)) {
//      val newBase = base.create().asInstanceOf[Graphics2D]
//      if (highlighted.isDefined && !highlighted.get.contains(element)) { // dimmed out if not highlighted
//        newBase.setPaint(
//          makeHatchTexture(background, ColorUtil.blendColor(background, base.getColor, 0.0375))
//        )
//      } else {
//        newBase.setPaint(makeHatchTexture(background, ColorUtil.blendColor(background, base.getColor, 0.15)))
//      }
//      newBase
//    } else {
//      if (element == rootNode && !showTop) { // completely transparent for root if not showing top
//        val newGraphics = base.create().asInstanceOf[Graphics2D]
//        newGraphics.setColor(new Color(0, 0, 0, 0))
//        newGraphics
//      } else { // computation is handled by the background passed in
//        val newGraphics = base.create().asInstanceOf[Graphics2D]
//        newGraphics.setColor(background)
//        newGraphics
//      }
//    }
//  }
//
//  // Modify the base graphics for drawing the outline (stroke) of some element, eg by highlighted status
//  override def strokeGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
//    if (element == rootNode && !showTop) { // completely transparent for root if not showing top
//      val newGraphics = base.create().asInstanceOf[Graphics2D]
//      newGraphics.setColor(new Color(0, 0, 0, 0))
//      newGraphics
//    } else if (selected.contains(element)) { // emphasis for selected
////      val newGraphics = base.create().asInstanceOf[Graphics2D]
////      newGraphics.setStroke(new BasicStroke(3 / zoomLevel))
////      newGraphics
//      base
//    } else if (highlighted.isDefined && !highlighted.get.contains(element)) { // dimmed out if not highlighted
//      val newGraphics = base.create().asInstanceOf[Graphics2D]
//      newGraphics.setColor(ColorUtil.blendColor(background, newGraphics.getColor, 0.25))
//      newGraphics
//    } else {
//      base
//    }
//  }

  override def paintEdge(parentG: Graphics2D, blockG: Graphics2D, edge: ElkEdge): Unit = {
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

//  override def getNodeBackground(
//      containingG: Graphics2D,
//      containingBackground: Color,
//      node: ElkNode
//  ): Color = {
//    val nodeBackground = if (highlighted.isDefined && !highlighted.get.contains(node)) { // dimmed out
//      ColorUtil.blendColor(containingBackground, containingG.getColor, 0.375)
//    } else {
//      ColorUtil.blendColor(containingBackground, containingG.getColor, 0.15)
//    }
//
//    nodeBackground
//  }
}
