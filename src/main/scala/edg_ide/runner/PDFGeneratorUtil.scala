package edg_ide.runner

import com.lowagie.text.{Document, Element, HeaderFooter, Rectangle}
import com.lowagie.text.pdf.PdfWriter
import edg_ide.swing.ElkNodePainter
import edgir.elem.elem.{BlockLike, HierarchyBlock}
import org.eclipse.elk.graph.ElkNode
import edg.util.SeqMapSortableFrom._
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
      val header = new HeaderFooter(true)
      header.setBorder(Rectangle.NO_BORDER)
      header.setAlignment(Element.ALIGN_RIGHT)
      document.setFooter(header)

      def printNode(node: ElkNode): Unit = {
        val (width, height) = generatePageSize(node)
        document.setPageSize(new Rectangle(width, height))

        if (document.isOpen) {
          document.newPage
        }
        else {
          document.open()
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

        val nameOrder = ProtoUtil.getNameOrder(block.meta)
        val children: Map[DesignPath, HierarchyBlock] = block.blocks.map {
          case (name, subblock) => (name, subblock.`type`)
        }.sortKeysFrom(nameOrder)
          .collect {
            case (name, BlockLike.Type.Hierarchy(subblock)) if subblock.blocks.nonEmpty => (path + name, subblock)
          }.toMap

        children.foreach(hBlock => {
          printNextHierarchyLevel(hBlock._2, hBlock._1)
        })
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
