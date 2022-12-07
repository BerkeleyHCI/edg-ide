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
import java.io.FileOutputStream


object PDFGeneratorUtil{

  private def generatePageSize(node: ElkNode): (Float, Float) = {
    val width = node.getWidth.toFloat + 2 * ElkNodePainter.margin.toFloat
    val height = node.getHeight.toFloat + 2 * ElkNodePainter.margin.toFloat
    (width, height)
  }

  def generate(content: HierarchyBlock, fileName: String): Unit = {

    val rootNode = HierarchyGraphElk.HBlockToElkNode(content)
    val (initWidth, initHeight) = generatePageSize(rootNode)
    val document = new Document(new Rectangle(initWidth, initHeight))

    // TODO: make a try..catch for FileOutputStream
    val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
    val header = new HeaderFooter(true)
    header.setBorder(Rectangle.NO_BORDER)
    header.setAlignment(Element.ALIGN_RIGHT)
    header.setPadding(0)
    document.setFooter(header)
    document.open()
    val cb = writer.getDirectContent
    val graphics = cb.createGraphics(initWidth, initHeight)
    val painter = new ElkNodePainter(rootNode)
    painter.paintComponent(graphics, Color.white)
    graphics.dispose()

    def printNextHierarchyLevel(block: HierarchyBlock, path: DesignPath = DesignPath()): Unit = {
      val nameOrder = ProtoUtil.getNameOrder(block.meta)
      val children: Map[DesignPath, HierarchyBlock] = block.blocks.map {
        case (name, subblock) => (name, subblock.`type`)
      }  .sortKeysFrom(nameOrder)
        .collect {
          case (name, BlockLike.Type.Hierarchy(subblock)) if subblock.blocks.nonEmpty => (path+name, subblock)
        }.toMap

      def printChild(node: ElkNode, path: String): Unit ={
        val (width, height) = generatePageSize(node)
        document.setPageSize(new Rectangle(width, height))
        document.newPage
        val subGraphics = cb.createGraphics(width, height)
        val painter = new ElkNodePainter(node)
        painter.paintComponent(subGraphics, Color.white)
        subGraphics.dispose()
      }

      children.foreach(hBlock => {
        val node = HierarchyGraphElk.HBlockToElkNode(hBlock._2, hBlock._1)
        printChild(node, hBlock._1.toString)
        printNextHierarchyLevel(hBlock._2, hBlock._1)
      })
    }

    printNextHierarchyLevel(content)
    document.close()
  }
}
