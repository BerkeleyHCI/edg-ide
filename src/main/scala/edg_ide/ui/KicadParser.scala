package edg_ide.ui
import java.io.{File, FileNotFoundException}
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


  // Given an Element, determine if it contains a valid position. If so, return the position. Otherwise, return None.
  // (tag xpos ypos) where xpos and ypos are numerical values and tag is a string, such as "start", "end", "at"
  def extractPos(element: Element, tag:String): Option[(Float, Float)] = {
    var position = (0f, 0f)
    element match {
      case Atom(symbol) =>
        None
      case SList(values) =>
        if (values.length != 3) return None

        // Verify (tag ....)
        values.head match {
          case Atom(symbol) =>
            if (!symbol.equalsIgnoreCase(tag)) {
              return None
            }
          case SList(values) =>
            return None

        }

        // Verify (... xpos ...) is numerical value
        values.tail.head match {
          case Atom(symbol) =>
            val parsedFloat = symbol.toFloatOption
            if (parsedFloat.isEmpty) {
              return None
            }
            else {
              position = (parsedFloat.get, position._2)
            }
          case SList(values) =>
            return None
        }

        // Verify (... ypos) is numerical value
        values.tail.tail.head match {
          case Atom(symbol) =>
            val parsedFloat = symbol.toFloatOption
            if (parsedFloat.isEmpty) {
              return None
            }
            else {
              position = (position._1, parsedFloat.get)
            }

          case SList(values) =>
            return None
        }

        Some(position)
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

        if (isFSilkLine(parsedElement)) {
          // We can safely assume parsedElement is an SList because of FSilkLine.
          // Ensure that startPosList and endPosList are well-formed, and get start/end positions.

          val startPosList = parsedElement.asInstanceOf[SList].values
          .map { e => extractPos(e, "start")} // mutate all Elements to be either Some((x, y)) or None }
          .filter(coordinates => coordinates.isDefined) // drop the Nones
          .map(coordinates => coordinates.getOrElse((-1f,-1f))) // unwrap the Optional (could just use get)

          val endPosList = parsedElement.asInstanceOf[SList].values
          .map { e => extractPos(e, "end") }
          .filter(optionalCoordinates => optionalCoordinates.isDefined)
          .map(coordinates => coordinates.getOrElse((-1f,-1f)))

          if (startPosList.length == 1 && endPosList.length == 1) {
            val startPos = startPosList(0)
            val endPos = endPosList(0)
            kicadComponents.addOne(new Line(startPos._1, startPos._2, endPos._1, endPos._2))
          }
        }

        else if (isRectPad(parsedElement)) {
          // We can safely assume parsedElement is an SList because of FSilkLine.
          // Ensure that locationList and sizeList are well-formed, and get location/size.
          val locationList = parsedElement.asInstanceOf[SList].values
            .map {
              e => extractPos(e, "at") // mutate all Elements to be either Some((x, y)) or None
            }
            .filter(coordinates => coordinates.isDefined) // drop the Nones
            .map(coordinates => coordinates.getOrElse((-1f,-1f))) // unwrap the Optional (could just use get)

          val sizeList = parsedElement.asInstanceOf[SList].values
            .map { e => extractPos(e, "size") }
            .filter(optionalCoordinates => optionalCoordinates.isDefined)
            .map(coordinates => coordinates.getOrElse((-1f,-1f)))

          if (locationList.length == 1 && sizeList.length == 1) {
            val location = locationList(0)
            val size = sizeList(0)

            val name = parsedElement.asInstanceOf[SList].values(1).asInstanceOf[Atom].symbol
            kicadComponents.addOne(new Rectangle(location._1, location._2, size._1, size._2, name))
          }
        }
      }

      fileReader.close()
      kicadComponents
    }
    catch {
      case e:FileNotFoundException =>
        println("Couldn't open kicad file for parsing: ", kicadFilePath)
        e.printStackTrace()
        throw e
      case t:Throwable =>
        println("Error while parsing kicad file")
        t.printStackTrace()
        throw t
    }
  }




}


