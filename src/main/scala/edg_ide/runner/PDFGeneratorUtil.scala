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
  private final val PAGE_MARGIN = 16f
  private final val TABLE_ROW_HEIGHT = 23f
  final val font = new Font(Font.HELVETICA, 12f, Font.UNDERLINE, Color.BLUE)
  final val font_bold = new Font(Font.HELVETICA, 12f, Font.BOLD, Color.BLACK)


  private def generatePageSize(node: ElkNode): (Float, Float) = {
    val width = node.getWidth.toFloat + 2f * ElkNodePainter.margin.toFloat + 1.5f * PAGE_MARGIN
    val height = node.getHeight.toFloat + 2f * ElkNodePainter.margin.toFloat + 1.5f * PAGE_MARGIN
    (width, height)
  }

  /*
    Generates a map of component class name to a set of DesignPath that uses the component.
    This is used to generate a table of components that are used multiple times in the design.
   */
  def getDuplicationList(block: HierarchyBlock, path: DesignPath = DesignPath(),
                          dupList: mutable.Map[LibraryPath, Set[DesignPath]] = mutable.Map()): Map[LibraryPath, Set[DesignPath]] = {
    val className = block.getSelfClass
    dupList.getOrElseUpdate(className, Set(path)) match {
      case existingSet: Set[DesignPath] => dupList(className) = existingSet + path
    }

    block.blocks.asPairs.map {
      case (name, subblock) => (name, subblock.`type`)
    }.collect {
      case (name, BlockLike.Type.Hierarchy(subblock)) => (path + name, subblock)
    }.foreach {
      case (path, subblock) => getDuplicationList(subblock, path, dupList)
    }

    dupList.toMap
  }


  def generate(content: HierarchyBlock,
               mappers: Seq[PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper]] = Seq(),
               fileName: String): Unit = {

    val dupList = getDuplicationList(content)

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
      val targetAnchor = new Anchor(path.toString, font_bold)
      targetAnchor.setName(path.toString)
      val headerPhrase = new Phrase
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

        dupSet.foreach { dupPath =>
          val cellContent = new Paragraph
          val (parent, current) = dupPath.split
          if (parent.toString == "(root)") {
            val parentAnchor = new Anchor
            parentAnchor.setReference(s"#${parent.toString}")
            parentAnchor.add(new Phrase(s"${parent.toString}", font))
            parentAnchor.add(new Phrase(s".${current}"))
            cellContent.add(parentAnchor)
          } else {
            var prevStep = ""
            dupPath.toString.split('.').foreach { step =>
              val anchor = new Anchor
              if (prevStep == "") {
                anchor.setReference(s"#${step}")
                anchor.add(new Phrase(s"${step}", font))
                prevStep = step
              } else {
                if (step == dupPath.toString.split('.').last) {
                  anchor.add(new Phrase(s".${step}"))
                }
                else {
                  anchor.setReference(s"#${prevStep}.${step}")
                  anchor.add(new Phrase("."))
                  anchor.add(new Phrase(s"${step}", font))
                  prevStep = prevStep + "." + step
                }
              }
              cellContent.add(anchor)
            }
          }
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
          if (dupList.getOrElse(className, Set.empty).headOption.contains(path)) {
            printNextHierarchyLevel(subblock, path)
          }
        }
      }
    }

    printNextHierarchyLevel(content)
    document.close()
  }
}
