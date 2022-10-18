package edg_ide.ui
import com.lowagie.text.Rectangle
import com.lowagie.text.pdf.PdfWriter
import com.lowagie.text.Document

import java.awt.Graphics2D
import java.io.FileOutputStream


class PDFGeneratorUtil (width: Double, height: Double, fileName: String){

  protected val document = new Document(new Rectangle(width.toFloat, height.toFloat))
  protected val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
  document.open()
  protected val cb = writer.getDirectContent
  val graphics = cb.createGraphics(width.toFloat, height.toFloat)

  def close(): Unit ={
    graphics.dispose()
    document.close()
  }
}
