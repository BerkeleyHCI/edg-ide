package edg_ide.ui
import com.lowagie.text.Rectangle
import com.lowagie.text.pdf.PdfWriter
import com.lowagie.text.Document

import java.awt.Graphics2D
import java.io.FileOutputStream


class PDFGeneratorUtil {

  protected val w = 500
  protected val h = 500
  protected val document = new Document(new Rectangle(w, h))
  protected val writer = PdfWriter.getInstance(document, new FileOutputStream("sun_tutorial.pdf"))
  document.open()
  protected val cb = writer.getDirectContent
  val graphics = cb.createGraphics(w, h)

  def close(): Unit ={
    graphics.dispose()
    document.close()
  }
}
