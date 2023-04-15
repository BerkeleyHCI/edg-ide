package edg_ide.runner

import com.lowagie.text.{Anchor, Document, Element, Font, FontFactory, HeaderFooter, Paragraph, Phrase, Rectangle}
import com.lowagie.text.pdf.{PdfPCell, PdfPTable, PdfWriter}
import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.ProtoUtil.BlockProtoToSeqMap
import edgir.elem.elem.{BlockLike, HierarchyBlock}
import org.eclipse.elk.graph.ElkNode
import edg.wir.DesignPath
import edg_ide.edgir_graph.HierarchyGraphElk.PropertyMapper
import edg_ide.edgir_graph.{EdgeWrapper, HierarchyGraphElk, NodeDataWrapper, PortWrapper}
import edg_ide.swing.blocks.ElkNodePainter
import edgir.ref.ref.LibraryPath

import java.awt.Color
import java.io.{FileNotFoundException, FileOutputStream, IOException}
import scala.collection.mutable

object PDFGeneratorUtil{

  /*
    Note for future development:
      If infinite loop (and eventually StackOverflow) occurs when adding header/footer, check that the
      page size is large enough/page margin is small enough so that header/footer content do not exceed
      the available space in the document.
   */
  final val PAGE_MARGIN = 15
  final val font = new Font(Font.HELVETICA, 12f, Font.UNDERLINE, Color.BLUE)
  final val TABLE_ROW_HEIGHT = 23f


  private def generatePageSize(node: ElkNode): (Float, Float) = {
    val width = node.getWidth.toFloat + 2.5f * ElkNodePainter.margin.toFloat
    val height = node.getHeight.toFloat + 2.5f * ElkNodePainter.margin.toFloat
    (width, height)
  }


  def getDuplicationList(block: HierarchyBlock, path: DesignPath = DesignPath(),
                         dupList: mutable.Map[LibraryPath, Set[DesignPath]]): Unit ={

    val className = block.getSelfClass
    dupList.getOrElseUpdate(className, Set(path)) match {
      case existingSet: Set[DesignPath] => dupList(className) = existingSet + path
    }

    block.blocks.asPairs.map {
      case (name, subblock) => (name, subblock.`type`)
    }.collect {
      case (name, BlockLike.Type.Hierarchy(subblock)) =>  (path + name, subblock)
    }.foreach {
      case (path, subblock) => getDuplicationList(subblock, path, dupList)
    }
  }


  def generate(content: HierarchyBlock,
               mappers: Seq[PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper]] = Seq(),
               fileName: String): Unit = {

    val dupList = mutable.Map[LibraryPath, Set[DesignPath]]()
    getDuplicationList(content, dupList = dupList)

    try {
      val document = new Document()
      val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))


      def printNode(node: ElkNode, className: LibraryPath, path: DesignPath): Unit = {
        val dupSet = dupList.getOrElse(className, Set.empty)
        val (width, height) = generatePageSize(node)
        val adjustedHeight = if(dupSet.size == 1) {
          height
        } else {
          TABLE_ROW_HEIGHT * dupSet.size + height + ElkNodePainter.margin
        }
        document.setPageSize(new Rectangle(width, adjustedHeight))
        document.setMargins(2*PAGE_MARGIN, 2*PAGE_MARGIN, PAGE_MARGIN, PAGE_MARGIN)

        // Sets the header and footer of each page. Target of the component target link is set as the header.
        val targetAnchor = new Anchor(path.toString)
        targetAnchor.setName(path.toString)
        val headerPhrase = new Phrase("Design Path: ")
        headerPhrase.add(targetAnchor)
        val pageHeader = new HeaderFooter(headerPhrase,false)
        pageHeader.setAlignment(Element.ALIGN_LEFT)
        document.setHeader(pageHeader)

        val pageFooter = new HeaderFooter(new Phrase("Page "), true)
        pageFooter.setAlignment(Element.ALIGN_RIGHT)
        pageFooter.setBorder(Rectangle.NO_BORDER)
        document.setFooter(pageFooter)

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
        val graphics = cb.createGraphics(width, adjustedHeight)
        val painter = new ElkNodePainter(node)
        painter.paintComponent(graphics, Color.white)
        graphics.dispose()

        // Prints out table if there are multiple usages of the same component
        if(dupSet.size > 1) {
          val table = new PdfPTable(1)

          val message = new Paragraph(s"${className.toSimpleString} is also used at the following:")
          val title = new PdfPCell(message)
          title.setBorder(Rectangle.NO_BORDER)
          table.addCell(title)

          dupSet.foreach { path =>
            val parentAnchor = new Anchor
            val (parent, current) = path.split
            parentAnchor.setReference(s"#${parent.toString}")
            parentAnchor.add(new Phrase(s"${parent.toString}", font))
            parentAnchor.add(new Phrase(s".${current.toString}"))

            val cellContent = new Paragraph
            cellContent.add(parentAnchor)
            val cell = new PdfPCell(cellContent)
            cell.setFixedHeight(TABLE_ROW_HEIGHT)
            table.addCell(cell)
          }
          val tableY = adjustedHeight - height + ElkNodePainter.margin
          table.setTotalWidth(width - 2 * ElkNodePainter.margin)
          table.writeSelectedRows(0, -1, ElkNodePainter.margin.toFloat, tableY, writer.getDirectContent())
        }
      }


      def printNextHierarchyLevel(block: HierarchyBlock, path: DesignPath = DesignPath()): Unit = {
        val node = HierarchyGraphElk.HBlockToElkNode(block, path, mappers = mappers)
        printNode(node, block.getSelfClass, path)

        block.blocks.asPairs.map {
          case (name, subblock) => (name, subblock.`type`)
        }.collect {
          case (name, BlockLike.Type.Hierarchy(subblock)) if subblock.blocks.nonEmpty => (path + name, subblock, subblock.getSelfClass)
        }.foreach {
          case (path, subblock, className) => {
            if (path == dupList.getOrElse(className, Set.empty).head) {
              printNextHierarchyLevel(subblock, path)
            }
          }
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
