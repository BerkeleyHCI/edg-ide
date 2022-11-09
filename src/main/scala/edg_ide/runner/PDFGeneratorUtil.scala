package edg_ide.runner

import com.lowagie.text.{Document, Rectangle}
import com.lowagie.text.pdf.PdfWriter
import edg_ide.swing.ElkNodePainter
import org.eclipse.elk.graph.ElkNode

import java.awt.Color
import java.io.FileOutputStream


object PDFGeneratorUtil{

  def generate(rootNode: ElkNode, fileName: String): Unit ={
    val width = rootNode.getWidth.toFloat
    val height = rootNode.getHeight.toFloat
    val document = new Document(new Rectangle(width, height))
    val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
    document.open()
    val cb = writer.getDirectContent
    val graphics = cb.createGraphics(width, height)

    println("In PDF Util, painting")
    val painter = new ElkNodePainter(rootNode)
    painter.paintComponent(graphics, Color.white)

    graphics.dispose()
    document.close()
  }
}
