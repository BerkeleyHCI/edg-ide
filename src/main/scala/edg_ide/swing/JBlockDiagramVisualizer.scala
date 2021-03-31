package edg_ide.swing

import com.intellij.util.ui.UIUtil
import edg.wir.DesignPath
import edg_ide.edgir_graph.ElkEdgirGraphUtils
import org.eclipse.elk.graph.{ElkEdge, ElkGraphElement, ElkNode, ElkPort}

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D, TexturePaint}
import scala.jdk.CollectionConverters.CollectionHasAsScala


/** Block diagram visualizer that customizes the rendering in JElkGraph with options specific to
  * design block diagrams:
  * - tunnel link names by heuristic matching (degenerate self-to-self links)
  * - additional setError(elts) to render elts as filled in red
  * - additional setStable(elts) to render elts as stale (???)
  */
class JBlockDiagramVisualizer(rootNode: ElkNode, showTop: Boolean = false) extends
    JElkGraph(rootNode, showTop) {
  protected val hatchRect = new Rectangle2D.Double(0, 0, 16, 16)
  def makeHatchTexture(backgroundColor: Color, foregroundColor: Color): TexturePaint = {
    val hatchImage = new BufferedImage(hatchRect.width.toInt, hatchRect.height.toInt, BufferedImage.TYPE_INT_ARGB)
    val hatchGraphics = hatchImage.createGraphics()
    val hatchTexture = new TexturePaint(hatchImage, hatchRect)
    hatchGraphics.setColor(backgroundColor)
    hatchGraphics.fill(hatchRect)
    hatchGraphics.setColor(foregroundColor)
    hatchGraphics.setStroke(new BasicStroke(2))
    hatchGraphics.drawLine(0, 16, 16, 0)
    hatchTexture
  }


  protected var errorElts: Set[ElkGraphElement] = Set()
  def setError(elts: Set[ElkGraphElement]): Unit = {
    errorElts = elts
    validate()
    repaint()
  }

  protected var staleElts: Set[ElkGraphElement] = Set()
  def setStale(elts: Set[ElkGraphElement]): Unit = {
    staleElts = elts
    validate()
    repaint()
  }

  override def setGraph(newGraph: ElkNode): Unit = {
    errorElts = Set()
    staleElts = Set()
    super.setGraph(newGraph)
  }

  override def fillGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    if (errorElts.contains(element)) {
      val newBase = base.create().asInstanceOf[Graphics2D]
      newBase.setColor(blendColor(background, Color.RED, 0.25))
      newBase  // explicitly ignores showTop invisibility if it's an error
    } else if (staleElts.contains(element)) {
      val newBase = base.create().asInstanceOf[Graphics2D]
      if (highlighted.isDefined && !highlighted.get.contains(element)) { // dimmed out if not highlighted
        newBase.setPaint(makeHatchTexture(background, blendColor(background, base.getColor, 0.0375)))
      } else {
        newBase.setPaint(makeHatchTexture(background, blendColor(background, base.getColor, 0.15)))
      }
      newBase
    } else {
      super.fillGraphics(base, background, element)
    }
  }



  override def paintEdge(parentG: Graphics2D, blockG: Graphics2D, background: Color, edge: ElkEdge): Unit = {
    super.paintEdge(parentG, blockG, background, edge)

    // TODO dedup from JElkGraph
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

    if (edge.getSources == edge.getTargets) {  // degenerate, "tunnel" (by heuristic / transform) edge
      val label = edge.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property) match {
        case DesignPath(steps) => steps.lastOption.getOrElse("")
        case _ => ""
      }

      val targetPointOpt = edge.getSections.asScala.headOption.map { section =>
        val bend = section.getBendPoints.asScala.head
        (bend.getX, bend.getY, section.getStartX, section.getStartY)
      }

      val textG = textGraphics(thisG, background, edge)
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
}
