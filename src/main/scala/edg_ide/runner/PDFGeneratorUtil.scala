package edg_ide.runner

import com.lowagie.text.{Document, Rectangle}
import com.lowagie.text.pdf.PdfWriter
import edg_ide.swing.GraphicsPaintingUtil

import java.awt.Color
import java.io.FileOutputStream


object PDFGeneratorUtil{

  private var width: Double = 1000.0
  private var height: Double = 1000.0

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

    println("In PDF Util, painting")
    GraphicsPaintingUtil.paintComponent(graphics, Color.white)

    graphics.dispose()
    document.close()
  }
}
