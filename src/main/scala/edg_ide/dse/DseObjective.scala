package edg_ide.dse
import scala.reflect.runtime.universe._
import edg.compiler.{ArrayValue, BooleanValue, Compiler, ExprValue, FloatValue, IntValue, RangeEmpty, RangeValue, TextValue}
import edg.wir.ProtoUtil.ParamProtoToSeqMap
import edg.wir.{DesignPath, IndirectDesignPath}
import edg_ide.ui.{EdgSettingsState, KicadParser}
import edgir.elem.elem.HierarchyBlock
import edgir.schema.schema
import edgir.schema.schema.Design

import java.io.File
import scala.collection.{SeqMap, mutable}


// Abstract base class for all design space objectives - some function of the design
// that produces a metric that is useful to the user, eg cost or some design parameter
//
// Must be serializable so configs can be saved and persist across IDE restarts
sealed trait DseObjective { self: Serializable =>
  // TODO: also needs libraries and external sources?
  def objectiveToString: String  // short human-friendly string describing this configuration

  def calculate(design: schema.Design, compiler: Compiler): Any
}


sealed trait DseTypedObjective[+T] extends DseObjective { self: Serializable =>
//  implicit val classTagB: TypeTag[T] = typeTag[T]

  def calculate(design: schema.Design, compiler: Compiler): T
}


// Extracts the value of a single parameter
case class DseObjectiveParameter(path: IndirectDesignPath)
    extends DseTypedObjective[Option[Any]] with Serializable {
  override def objectiveToString = f"Parameter($path)"

  override def calculate(design: Design, compiler: Compiler): Option[Any] = {
    compiler.getParamValue(path) match {
      case Some(FloatValue(value)) => Some(value)
      case Some(IntValue(value)) => Some(value)
      case Some(RangeValue(lower, upper)) => Some((lower, upper))
      case Some(BooleanValue(value)) => Some(value)
      case Some(TextValue(value)) => Some(value)
      case Some(RangeEmpty) => Some((None, None))
      case Some(ArrayValue(values)) => Some(values)  // TODO recursively unpack
      case None => None
    }
  }
}


// Stores footprint area so the data is not serialized and cached across multiple compilation runs
object DseObjectiveFootprintArea {
  private val footprintAreaCache = mutable.Map[String, Float]()

  def getFootprintArea(footprintName: String): Float = {
    footprintAreaCache.getOrElseUpdate(footprintName, {
      var kicadDirectory = new File(EdgSettingsState.getInstance().kicadDirectory)
      val footprintSplit = footprintName.split(':')
      for (libraryName <- footprintSplit.init) {
        kicadDirectory = new File(kicadDirectory, libraryName + ".pretty")
      }
      val footprintFile = new File(kicadDirectory, footprintSplit.last + ".kicad_mod")
      val footprint = KicadParser.parseKicadFile(footprintFile)
      if (footprint.elts.isEmpty) {
        println(s"bad footprint $footprintName") // TODO unified error logging
      }
      footprint.courtyardArea.getOrElse(0)
    })
  }
}


case class DseObjectiveFootprintArea(rootPath: DesignPath = DesignPath())
    extends DseTypedObjective[Float] with Serializable {
  override def objectiveToString = f"FootprintArea($rootPath)"

  override def calculate(design: Design, compiler: Compiler): Float = {
    new DesignBlockMap[Float] {
      override def mapBlock(path: DesignPath, block: HierarchyBlock, blocks: SeqMap[String, Float]): Float = {
        val thisArea = if (path.startsWith(rootPath)) {
          compiler.getParamValue((path + "fp_footprint").asIndirect) match {
            case Some(TextValue(footprintName)) => DseObjectiveFootprintArea.getFootprintArea(footprintName)
            case _ => 0
          }
        } else {
          0
        }
        thisArea + blocks.values.sum
      }
    }.map(design)
  }
}


// Counts the total number of footprints
case class DseObjectiveFootprintCount(rootPath: DesignPath = DesignPath())
    extends DseTypedObjective[Int] with Serializable {
  override def objectiveToString = f"FootprintCount($rootPath)"

  override def calculate(design: Design, compiler: Compiler): Int = {
    new DesignBlockMap[Int] {
      override def mapBlock(path: DesignPath, block: HierarchyBlock, blocks: SeqMap[String, Int]): Int = {
        val thisValue = if (path.startsWith(rootPath) && block.params.toSeqMap.contains("fp_footprint")) 1 else 0
        thisValue + blocks.values.sum
      }
    }.map(design)
  }
}
