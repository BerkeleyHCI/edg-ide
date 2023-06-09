package edg_ide.ui
import edg_ide.util.AreaUtils
import edg_ide.util.external.{Atom, Element, ExpressionParser, SList}

import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

// Kicad IR
sealed trait KicadComponent {
  // Returns bounds as ((xmin, ymin), (xmax, ymax))
  def bounds: ((Float, Float), (Float, Float))
}

// TODO distinguish pad names from other geometry
case class Rectangle(x: Float, y: Float, width: Float, height: Float, name: String) extends KicadComponent {
  override def bounds: ((Float, Float), (Float, Float)) =
    ((x - width / 2, y - height / 2), (x + width / 2, y + height / 2))
}

case class Oval(x: Float, y: Float, width: Float, height: Float, name: String) extends KicadComponent {
  override def bounds: ((Float, Float), (Float, Float)) =
    ((x - width / 2, y - height / 2), (x + width / 2, y + height / 2))
}

case class Line(x0: Float, y0: Float, x1: Float, y1: Float, layers: Set[String]) extends KicadComponent {
  override def bounds: ((Float, Float), (Float, Float)) = ((x0, y0), (x1, y1))
}

case class KicadFootprint(elts: Seq[KicadComponent]) {
  // Returns overall bounds as ((xmin, ymin), (xmax, ymax))
  def bounds: ((Float, Float), (Float, Float)) = {
    if (elts.isEmpty) {
      ((0, 0), (0, 0))
    } else {
      elts.map(_.bounds).reduce { (elt1, elt2) =>
        val ((xmin1, ymin1), (xmax1, ymax1)) = elt1
        val ((xmin2, ymin2), (xmax2, ymax2)) = elt2
        ((Seq(xmin1, xmin2).min, Seq(ymin1, ymin2).min), (Seq(xmax1, xmax2).min, Seq(ymax1, ymax2).max))
      }
    }
  }

  // Calculates the area formed by courtyard lines, if they form a closed path
  def courtyardArea: Option[Float] = {
    val courtyardEdges = elts.collect { // collect courtyard lines and transform structure
      case Line(x0, y0, x1, y1, layers) if layers.contains("F.CrtYd") =>
        ((x0, y0), (x1, y1))
    }
    AreaUtils.doubleAreaOf(courtyardEdges).map(_ / 2)
  }
}

object KicadParser {
  // Given an SList, scan the SList for the only sublist tagged by `name`
  // i.e. in the list ((a b c) ((d) e f) (d 1 2) d h i), if we search for
  // sublists tagged by 'd', we want to return (d 1 2)
  protected def getOnlySublistByName(list: Seq[Element], name: String): SList = {
    val subLists = list
      .filter {
        case _: Atom => false
        case SList(values) =>
          values.head match {
            case atom: Atom =>
              atom.symbol.equals(name)
            case _: SList => false
          }
      }
      .map {
        // all Atoms should have been filtered out at this point
        e => e.asInstanceOf[SList]
      }

    if (subLists.length == 1)
      return subLists(0)

    // enforcing the "Only" in getOnlySublistByName
    if (subLists.length > 1)
      throw new IllegalArgumentException("Multiple subLists with name, only expected one: " + name)

    throw new IllegalArgumentException("No subList with name, expected one: " + name)

  }

  // Given a position-identifying list of the form (name:String, a:Float, b:Float),
  // return (a, b) or throw an Exception
  protected def extractPosition(list: SList): (Float, Float, Float) = {
    list.values match {
      case (_: Atom) :: Atom(xPos) :: Atom(yPos) :: Nil =>
        (xPos.toFloatOption, yPos.toFloatOption) match {
          case (Some(xPos), Some(yPos)) => (xPos, yPos, 0)
          case _ =>
            throw new IllegalArgumentException(
              "Expected (float, float), but got non-numerical value: " + xPos + yPos
            )
        }
      case (_: Atom) :: Atom(xPos) :: Atom(yPos) :: Atom(rot) :: Nil =>
        (xPos.toFloatOption, yPos.toFloatOption, rot.toFloatOption) match {
          case (Some(xPos), Some(yPos), Some(rot)) => (xPos, yPos, rot)
          case _ =>
            throw new IllegalArgumentException(
              "Expected (float, float, float), but got non-numerical value: " + xPos + yPos + rot
            )
        }
      case badVal => throw new IllegalArgumentException("Expected (float, float), but got: " + badVal)
    }
  }

  // Extracts a (width, height) given a sexpr of the form (at, w, h)
  protected def extractSize(list: SList): (Float, Float) = {
    list.values match {
      case (_: Atom) :: Atom(width) :: Atom(height) :: Nil =>
        (width.toFloatOption, height.toFloatOption) match {
          case (Some(width), Some(height)) => (width, height)
          case _ =>
            throw new IllegalArgumentException(
              "Expected (float, float), but got non-numerical value: " + width + height
            )
        }
      case badVal => throw new IllegalArgumentException("Expected (float, float), but got: " + badVal)
    }
  }

  // Given a SList with direct child Atoms containing strings that may start and end with quote marks,
  // returns a new SList with those Atoms with leading and trailing quote marks removed.
  protected def stripChildAtom(list: SList): SList = {
    SList(list.values.map {
      case Atom(atomValue) => Atom(atomValue.stripPrefix("\"").stripSuffix("\""))
      case elt => elt
    })
  }

  def parseKicadFile(kicadFile: File): KicadFootprint = {
    try {
      val fileReader = Source.fromFile(kicadFile)
      val lines = fileReader.getLines()
      var s = ""
      for (l <- lines.toList) {
        s += l
      }

      val parsed = ExpressionParser.parse(s)

      val kicadComponents = parsed.values.flatMap {
        case SList(Atom("pad") :: Atom(name) :: _ :: Atom(geom) :: tail)
          if geom == "rect" || geom == "roundrect" =>
          val (x, y, r) = extractPosition(getOnlySublistByName(tail, "at"))
          val (w, h) = extractSize(getOnlySublistByName(tail, "size"))
          if (r == 0 || r == 180) {
            Some(Rectangle(x, y, w, h, name.stripPrefix("\"").stripSuffix("\"")))
          } else if (r == 90 || r == 270) {
            Some(Rectangle(x, y, h, w, name.stripPrefix("\"").stripSuffix("\"")))
          } else {
            throw new IllegalArgumentException(f"rotation not supported $r")
          }
        case SList(Atom("pad") :: Atom(name) :: _ :: Atom(geom) :: tail)
          if geom == "oval" || geom == "circle" =>
          val (x, y, r) = extractPosition(getOnlySublistByName(tail, "at"))
          val (w, h) = extractSize(getOnlySublistByName(tail, "size"))
          if (r == 0 || r == 180) {
            Some(Oval(x, y, w, h, name.stripPrefix("\"").stripSuffix("\"")))
          } else if (r == 90 || r == 270) {
            Some(Oval(x, y, h, w, name.stripPrefix("\"").stripSuffix("\"")))
          } else {
            throw new IllegalArgumentException(f"rotation not supported $r")
          }
        case SList(Atom("fp_line") :: tail) =>
          val layerList = stripChildAtom(getOnlySublistByName(tail, "layer")).values.collect {
            case Atom(layer) => layer
          }.toSet
          val (startX, startY) = extractSize(getOnlySublistByName(tail, "start"))
          val (endX, endY) = extractSize(getOnlySublistByName(tail, "end"))
          Some(Line(startX, startY, endX, endY, layerList))

        case _ => None
      }

      fileReader.close()
      KicadFootprint(kicadComponents)
    } catch {
      // Fail noisily but don't crash the plugin -- just don't draw anything
      case e: FileNotFoundException =>
        {}
        println("Couldn't open kicad file for parsing: ", kicadFile.getName)
        e.printStackTrace()
        KicadFootprint(Seq())
      case t: Throwable =>
        println("Error while parsing kicad file")
        t.printStackTrace()
        KicadFootprint(Seq())
    }
  }
}
