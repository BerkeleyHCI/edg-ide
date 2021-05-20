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

  // Given a parsed Element, determine if it is a "fp_line"
  // e.g. (fp_line (start x y) (end x y) (layer F.SilkS) (width w))
  def isFpLine(element: Element):Boolean = {

    element match {
      case Atom(symbol) =>
        false
      case SList(values) =>
        // Match on "fp_line" atom in list
        val lineFiltered = values.filter {
          case Atom(symbol) => symbol.equalsIgnoreCase("fp_line")
          case _:SList => false
        }

        lineFiltered.nonEmpty
    }
  }

  // Given an element, determine if it is an "fp_line" with a "FSilk.S" layer
  // e.g. (fp_line (start x y) (end 22.x y) (layer F.SilkS) (width w))
  def isFSilkLine(element: Element): Boolean = {
    if (!isFpLine(element)) return false

    element match {
      case Atom(symbol) =>
        // should never get here, since isFpLine() would reject any Atoms
        throw new Exception("[Error] Got an Atom but accepted it as an fp_line...something is wrong!: " + element.toString)
        false
      case SList(values) =>
        // Search for "Silk" layer
        // (layer F.SilkS)

        val layerFiltered = values.tail.filter {
          case _:Atom =>
            false
          case s:SList =>
            if (s.values.length != 2) false
            else {
              val bothAtoms = s.values.head.isInstanceOf[Atom] && s.values.tail.head.isInstanceOf[Atom]
              if (!bothAtoms) false
              else {
                // Can safely convert to atom
                val layerTokenAtom = s.values.head.asInstanceOf[Atom]
                val silkTokenAtom = s.values.tail.head.asInstanceOf[Atom]
                layerTokenAtom.symbol.equalsIgnoreCase("layer") && silkTokenAtom.symbol.contains("Silk")
              }
            }
        }

        layerFiltered.nonEmpty
    }
  }


  // Given a parsed Element, determine if it is a "pad"
  // (pad ... (at x y) (size w h) ... )
  def isPad(element: Element):Boolean = {
    element match {
      case Atom(symbol) =>
        false
      case SList(values) =>
        // Match on "pad" atom in list
        val padFiltered = values.filter {
          case Atom(symbol) => symbol.equalsIgnoreCase("pad")
          case _:SList => false

        }
        padFiltered.nonEmpty
    }
  }

  // Given a parsed Element, determine if it is a rectangular pad
  // (pad ... (at x y) (size w h) ... )
  def isRectPad(element: Element): Boolean = {
    if (!isPad(element)) return false

    element match {
      case Atom(symbol) =>
        false
      case SList(values) =>
        // Match on "rect" (or roundrect) atom in list
        val rectFiltered = values.filter {
          case Atom(symbol) => symbol.equalsIgnoreCase("rect")
          case _:SList => false
        }
        rectFiltered.nonEmpty
    }
  }

  // Given an SList, scan the SList for the only sublist tagged by `name`
  // i.e. in the list ((a b c) ((d) e f) (d 1 2) d h i), if we search for
  // sublists tagged by 'd', we want to return (d 1 2)
  def getOnlySublistByName(list: SList, name: String): Option[SList] = {
    val sublists = list.values
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

    if (sublists.length == 1)
      return Some(sublists(0))

     None
  }

  // Given a position-identifying list of the form (name:String, a:Float, b:Float),
  // return (a, b) or throw an Exception
  def extractPosition(list:SList): (Float, Float) = {
    require((list.values.length == 3) && (list.values.head.isInstanceOf[Atom]),
      "Expected list of the form (string float float) but got: " + list.toString)

    var position = (0.0f, 0.0f)

    // Verify (... xpos ...) is numerical value
    list.values.tail.head match {
      case Atom(symbol) =>
        val parsedFloat = symbol.toFloatOption
        if (parsedFloat.isEmpty) {
          throw new IllegalArgumentException("Expected float for first value, but got non-numerical value: " + parsedFloat.toString)
        }
        else {
          position = (parsedFloat.get, position._2)
        }
      case SList(values) =>
        throw new IllegalArgumentException("Expected float for first value, but got SList: " + values.toString)
    }

    // Verify (... ypos) is numerical value
    list.values.tail.tail.head match {
      case Atom(symbol) =>
        val parsedFloat = symbol.toFloatOption
        if (parsedFloat.isEmpty) {
          throw new IllegalArgumentException("Expected float for first value, but got non-numerical value: " + parsedFloat.toString)
        }
        else {
          position = (position._1, parsedFloat.get)
        }
      case SList(values) =>
        throw new IllegalArgumentException("Expected float for first value, but got SList: " + values.toString)
    }

    position
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

        if (isFSilkLine(parsedElement)) {
          // We can safely assume parsedElement is an SList because of FSilkLine.
          // Ensure that startPosList and endPosList are well-formed, and get start/end positions.
          val startPosList = getOnlySublistByName(parsedElement.asInstanceOf[SList], "start")
          val endPosList = getOnlySublistByName(parsedElement.asInstanceOf[SList], "end")

          if (startPosList.isDefined && endPosList.isDefined) {
            val (startX, startY) = extractPosition(startPosList.get)
            val (endX, endY) = extractPosition(endPosList.get)

            kicadComponents.addOne(new Line(startX, startY, endX, endY))
          }
        }

        else if (isRectPad(parsedElement)) {
          // We can safely assume parsedElement is an SList because of FSilkLine.
          // Ensure that locationList and sizeList are well-formed, and get location/size.
          val locationList = getOnlySublistByName(parsedElement.asInstanceOf[SList], "at")
          val sizeList = getOnlySublistByName(parsedElement.asInstanceOf[SList], "size")

          if (locationList.isDefined && sizeList.isDefined) {
            val (x, y) = extractPosition(locationList.get)
            val (w, h) = extractPosition(sizeList.get)

            val name = parsedElement.asInstanceOf[SList].values(1).asInstanceOf[Atom].symbol
            kicadComponents.addOne(new Rectangle(x, y, w, h, name))
          }
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


