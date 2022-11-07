package edg_ide.runner

import com.lowagie.text.{Document, Rectangle}
import com.lowagie.text.pdf.PdfWriter
import edg_ide.swing.GraphicsPaintingUtil

import java.io.FileOutputStream


object PDFGeneratorUtil{

  private var width: Double = 100
  private var height: Double = 100

  def setDimensions(w: Double, h: Double): Unit ={
    width = w
    height = h
  }

  def generate(fileName: String): Unit ={
    val document = new Document(new Rectangle(width.toFloat, height.toFloat))
    val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
    document.open()
    val cb = writer.getDirectContent
    val graphics = cb.createGraphics(width.toFloat, height.toFloat)

    GraphicsPaintingUtil.paintComponent(graphics)

    graphics.dispose()
    document.close()
  }
}
