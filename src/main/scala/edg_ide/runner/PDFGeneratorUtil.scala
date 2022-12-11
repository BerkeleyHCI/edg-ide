package edg_ide.runner

import com.lowagie.text.{Document, Element, HeaderFooter, Rectangle}
import com.lowagie.text.pdf.PdfWriter
import edg.wir.ProtoUtil.BlockProtoToSeqMap
import edg_ide.swing.ElkNodePainter
import edgir.elem.elem.{BlockLike, HierarchyBlock}
import org.eclipse.elk.graph.ElkNode
import edg.wir.{DesignPath, ProtoUtil}
import edg_ide.edgir_graph.HierarchyGraphElk

import java.awt.Color
import java.io.{FileNotFoundException, FileOutputStream, IOException}


object PDFGeneratorUtil{

  private def generatePageSize(node: ElkNode): (Float, Float) = {
    val width = node.getWidth.toFloat + 2 * ElkNodePainter.margin.toFloat
    val height = node.getHeight.toFloat + 2 * ElkNodePainter.margin.toFloat
    (width, height)
  }

  def generate(content: HierarchyBlock, fileName: String): Unit = {
    try {
      val document = new Document()
      val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
      val footer = new HeaderFooter(true)
      footer.setBorder(Rectangle.NO_BORDER)
      footer.setAlignment(Element.ALIGN_RIGHT)
      document.setFooter(footer)

      def printNode(node: ElkNode): Unit = {
        val (width, height) = generatePageSize(node)
        document.setPageSize(new Rectangle(width, height))

        /*
        Metadata for the Footer does not align the page number correctly if
        document.open() is called before document.setPageSize() was executed
         */
        if (!document.isOpen) {
          document.open()
        } else {
          document.newPage
        }

        val cb = writer.getDirectContent
        val graphics = cb.createGraphics(width, height)
        val painter = new ElkNodePainter(node)
        painter.paintComponent(graphics, Color.white)
        graphics.dispose()
      }

      def printNextHierarchyLevel(block: HierarchyBlock, path: DesignPath = DesignPath()): Unit = {
        val node = HierarchyGraphElk.HBlockToElkNode(block, path)
        printNode(node)

        block.blocks.asPairs.map {
          case (name, subblock) => (name, subblock.`type`)
        }
          .collect {
            case (name, BlockLike.Type.Hierarchy(subblock)) if subblock.blocks.nonEmpty => (path + name, subblock)
          }.toMap.foreach {
          case (path, subblock) => printNextHierarchyLevel(subblock, path)
        }
      }

      printNextHierarchyLevel(content)
      document.close()
    } catch {
      case e: FileNotFoundException => println(s"Couldn't find ${fileName}.")
      case e: SecurityException => println(s"Access to ${fileName} denied.")
      case e: IOException => println(s"Had an IOException trying to read ${fileName}")
    }
  }
}
