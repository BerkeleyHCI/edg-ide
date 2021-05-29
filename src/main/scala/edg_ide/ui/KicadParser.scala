package edg_ide.ui
import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

// Kicad IR
sealed trait KicadComponent
// TODO: add layers to this data structure?
// TODO distinguish pad names from other geometry
case class Rectangle(x:Float, y:Float, width:Float, height:Float, name: String) extends KicadComponent
case class Line(x0:Float, y0:Float, x1:Float, y1:Float) extends KicadComponent


class KicadParser(kicadFilePath:String) {

  private var kicadFile: File = new File("")

  def setKicadFile(kicadFile: File): Unit = {
    this.kicadFile = kicadFile
  }


  def containsAtom(slist:SList, atom: String): Boolean = {
    val filtered = slist.values.filter {
      case Atom(symbol) if symbol == atom => true
      case _ => false
    }

    filtered.nonEmpty
  }

  // Given an SList, scan the SList for the only sublist tagged by `name`
  // i.e. in the list ((a b c) ((d) e f) (d 1 2) d h i), if we search for
  // sublists tagged by 'd', we want to return (d 1 2)
  def getOnlySublistByName(list: SList, name: String): Option[SList] = {
    val subLists = list.values
      .filter {
        case _:Atom => false
        case SList(values) =>
          values.head match {
            case atom: Atom =>
              atom.symbol.equals(name)
            case _: SList =>false
          }
      }
      .map {
        // all Atoms should have been filtered out at this point
        e => e.asInstanceOf[SList]
      }

    if (subLists.length == 1)
      return Some(subLists(0))

    if (subLists.isEmpty)
      return None

    // enforcing the "Only" in getOnlySublistByName
    throw new IllegalArgumentException("Multiple subLists with name, only expected one: " + name)
  }

  // Given a position-identifying list of the form (name:String, a:Float, b:Float),
  // return (a, b) or throw an Exception
  def extractPosition(list:SList): (Float, Float) = {
    require((list.values.length == 3) && (list.values.head.isInstanceOf[Atom]),
      "Expected list of the form (string float float) but got: " + list.toString)


    (list.values.tail.head, list.values.tail.tail.head) match {
      case (Atom(xpos), Atom(ypos)) =>
        (xpos.toFloatOption, ypos.toFloatOption) match {
          case (Some(xpos), Some(ypos)) => (xpos, ypos)
          case _ => throw new IllegalArgumentException("Expected (float, float), but got non-numerical value: " + xpos + ypos)
        }
      case badVal => throw new IllegalArgumentException("Expected float (float, float), but got: " + badVal)
    }

  }

  def parseKicadFile(): ArrayBuffer[KicadComponent] = {
    try {
      val fileReader = Source.fromFile(kicadFile)
      val lines = fileReader.getLines()
      var s = ""
      for (l <- lines.toList) {
        s += l
      }

      val parsed = ExpressionParser.parse(s)
      var kicadComponents = new ArrayBuffer[KicadComponent]

      for (parsedElement <- parsed.values) {

        parsedElement match {
          case list: SList =>
            if (list.values.nonEmpty && list.values.head.isInstanceOf[Atom]) {
              val layerList = getOnlySublistByName(list, "layer")
              list.values.head.asInstanceOf[Atom].symbol match {

                // Match on the different components we want to handle
                case "pad" if containsAtom(list, "rect") || containsAtom(list, "roundrect") =>
                  val locationList = getOnlySublistByName(parsedElement.asInstanceOf[SList], "at")
                  val sizeList = getOnlySublistByName(parsedElement.asInstanceOf[SList], "size")

                  require(locationList.isDefined && sizeList.isDefined)
                  val (x, y) = extractPosition(locationList.get)
                  val (w, h) = extractPosition(sizeList.get)

                  val name = parsedElement.asInstanceOf[SList].values(1).asInstanceOf[Atom].symbol
                  kicadComponents.addOne(new Rectangle(x, y, w, h, name))


                case "fp_line" if getOnlySublistByName(list, "layer").isDefined && layerList.get.values.length == 2 && layerList.get.values.tail.head == Atom("F.SilkS") =>
                  val startPosList = getOnlySublistByName(list, "start")
                  val endPosList = getOnlySublistByName(list, "end")

                  require(startPosList.isDefined && endPosList.isDefined)
                  val (startX, startY) = extractPosition(startPosList.get)
                  val (endX, endY) = extractPosition(endPosList.get)

                  kicadComponents.addOne(new Line(startX, startY, endX, endY))

                case _ =>
            }
          }

          case _ =>
        }

      }

      fileReader.close()
      kicadComponents
    }
    catch {
      // Fail noisily but don't crash the plugin -- just don't draw anything
      case e:FileNotFoundException => {}
        println("Couldn't open kicad file for parsing: ", kicadFilePath)
        e.printStackTrace()
        ArrayBuffer()
      case t:Throwable =>
        println("Error while parsing kicad file")
        t.printStackTrace()
        ArrayBuffer()
    }
  }




}


