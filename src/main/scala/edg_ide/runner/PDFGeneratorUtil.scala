package edg_ide.runner

import com.lowagie.text.{Document, Rectangle}
import com.lowagie.text.pdf.{PdfContentByte, PdfWriter}
import edg_ide.swing.{ElkNodePainter, HierarchyBlockNode}
import edgir.elem.elem.{BlockLike, HierarchyBlock}
import org.eclipse.elk.graph.ElkNode
import edgir.elem.elem
import edg.util.SeqMapSortableFrom._
import edg.wir.{DesignPath, ProtoUtil}
import edg_ide.edgir_graph.HierarchyGraphElk

import java.awt.Color
import java.io.FileOutputStream
import scala.jdk.CollectionConverters.ListHasAsScala


object PDFGeneratorUtil{

//  private def getPrintableChildren(block: HierarchyBlock): Map[String, HierarchyBlock] = {
//    val nameOrder = ProtoUtil.getNameOrder(block.meta)
//    val children: Map[String, HierarchyBlock] = block.blocks.map {
//      case (name, subblock) => (name, subblock.`type`)
//    }  .sortKeysFrom(nameOrder)
//      .collect {
//        case (name, BlockLike.Type.Hierarchy(subblock)) => (name, subblock)
//      }.toMap
//
//    println(children)
//    children
//  }

//  def generate(content: HierarchyBlock, fileName: String): Unit = {
//
//    val rootNode = HierarchyGraphElk.HBlockToElkNode(content, depth=2)
//
//    // TODO: Set a fixed document size and scale graphics accordingly?
//    val width = rootNode.getWidth.toFloat + 2 * ElkNodePainter.margin.toFloat
//    val height = rootNode.getHeight.toFloat + 2 * ElkNodePainter.margin.toFloat
//    val document = new Document(new Rectangle(width, height))
//
//    // TODO: make a try..catch for FileOutputStream
//    val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
//    document.open()
//    val cb = writer.getDirectContent
//    val graphics = cb.createGraphics(width, height)
//    val painter = new ElkNodePainter(rootNode)
//    painter.paintComponent(graphics, Color.white)
//    graphics.dispose()
//
//    def printChild(node: ElkNode, path: String): Unit ={
//      println("Printing...")
//      document.newPage
//      val subGraphics = cb.createGraphics(width, height)
//      val painter = new ElkNodePainter(node)
//      painter.paintComponent(subGraphics, Color.white)
//      subGraphics.dispose()
//      println("Printed" + path)
//    }
//
//    getPrintableChildren(content).foreach(hBlock => {
//      if (hBlock != null) {
//        val node = HierarchyGraphElk.HBlockToElkNode(hBlock._2)
//        printChild(node, hBlock._1)
//      }
//      else{
//        println("Null node encountered")
//      }
//    })
//    println("FINISHED PRINTING")
//    document.close()
//  }

  def generate(content: HierarchyBlock, fileName: String): Unit = {
    val rootNode = HierarchyGraphElk.HBlockToElkNode(content, depth=2)
    // TODO: Set a fixed document size and scale graphics accordingly?
    val width = rootNode.getWidth.toFloat + 2 * ElkNodePainter.margin.toFloat
    val height = rootNode.getHeight.toFloat + 2 * ElkNodePainter.margin.toFloat
    val document = new Document(new Rectangle(width, height))

    // TODO: make a try..catch for FileOutputStream
    val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
    document.open()
    val cb = writer.getDirectContent
    val graphics = cb.createGraphics(width, height)
    val painter = new ElkNodePainter(rootNode)
    painter.paintComponent(graphics, Color.white)
    graphics.dispose()
    document.close()
  }
}
