package edg_ide.swing

import org.eclipse.elk.core.options._
import org.eclipse.elk.graph._

import java.awt._
import java.awt.geom.AffineTransform
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}


class ModifiedElkNodePainter(rootNode: ElkNode, showTop: Boolean = false,
                             zoomLevel: Float = 1.0f)
extends ElkNodePainter(rootNode, showTop, zoomLevel) {
  private var selected: Set[ElkGraphElement] = Set()
  private var highlighted: Option[Set[ElkGraphElement]] = None

//  def setZoom(zoom: Float): Unit = {
//    zoomLevel = zoom
//  }
//
  def setSelected(elts: Set[ElkGraphElement]): Unit = {
    selected = elts
  }

  def setHighlighted(elts: Option[Set[ElkGraphElement]]): Unit = {
    highlighted = elts
  }
//
//  def setRootNode(node: ElkNode): Unit ={
//    rootNode = node
//  }


  // Modify the base graphics for filling some element, eg by highlighted status
  override def fillGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
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
  override def strokeGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
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
  override def textGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
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


  override def paintComponent(paintGraphics: Graphics, backGround: Color): Unit = {
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
