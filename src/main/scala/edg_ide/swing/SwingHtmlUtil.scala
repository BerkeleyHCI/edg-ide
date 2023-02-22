package edg_ide.swing

import java.awt.{Color, Font}

object SwingHtmlUtil {
  def wrapInHtml(text: String, font: Font): String = {
    val htmlBody = s"""<body style="font-family:${font.getFamily()}; font-size:${font.getSize}">"""
    s"<html>$htmlBody${text.replaceAll("\n", "<br/>")}</html>"
  }

  // how the heck this isn't a standard library function is beyond me, especially since there's Color.decode
  def colorToHtml(color: Color): String = {
    val hex = Integer.toHexString(color.getRGB & 0xffffff)
    "#" + ("0" * (6 - hex.length)) + hex
  }
}
