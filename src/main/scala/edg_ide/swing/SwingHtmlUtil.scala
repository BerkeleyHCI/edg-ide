package edg_ide.swing

import java.awt.Font

object SwingHtmlUtil {
  def wrapInHtml(text: String, font: Font): String = {
    val htmlBody = s"""<body style="font-family:${font.getFamily()}; font-size:${font.getSize}">"""
    s"<html>$htmlBody${text.replaceAll("\n", "<br/>")}</html>"
  }
}
