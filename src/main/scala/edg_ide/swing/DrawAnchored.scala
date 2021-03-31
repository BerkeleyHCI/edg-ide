package edg_ide.swing

import java.awt.Graphics

sealed trait DrawAnchored  // position on the "label" where the drawing point is
object DrawAnchored {
  object Bottom extends DrawAnchored
  object Top extends DrawAnchored
  object Left extends DrawAnchored
  object Right extends DrawAnchored
  object Center extends DrawAnchored

  def drawLabel(g: Graphics, text: String, point: (Double, Double), anchor: DrawAnchored): Unit = {
    val fontMetrics = g.getFontMetrics(g.getFont)

    val textWidth = fontMetrics.stringWidth(text)
    val textHeight = fontMetrics.getMaxAscent

    val (pointX, pointY) = point
    val (drawX, drawY) = anchor match {
      case Bottom => (pointX - textWidth / 2, pointY)
      case Top => (pointX - textWidth / 2, pointY + textHeight)
      case Left => (pointX, pointY + textHeight / 2)
      case Right => (pointX - textWidth, pointY + textHeight / 2)
      case Center => (pointX - textWidth / 2, pointY + textHeight / 2)
    }
    g.drawString(text, drawX.toInt, drawY.toInt)
  }
}
