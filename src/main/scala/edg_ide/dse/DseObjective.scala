package edg_ide.dse
import edg.compiler.{ArrayValue, BooleanValue, Compiler, FloatValue, IntValue, RangeEmpty, RangeValue, TextValue}
import edg.util.Errorable
import edg.wir.ProtoUtil.ParamProtoToSeqMap
import edg.wir.{DesignPath, IndirectDesignPath}
import edg_ide.ui.KicadParser
import edg_ide.util.KicadFootprintUtil
import edgir.elem.elem.HierarchyBlock
import edgir.schema.schema
import edgir.schema.schema.Design

import scala.collection.{SeqMap, mutable}


// Abstract base class for all design space objectives - some function of the design
// that produces a metric that is useful to the user, eg cost or some design parameter
//
// Must be serializable so configs can be saved and persist across IDE restarts
sealed trait DseObjective { self: Serializable =>
  def objectiveToString: String  // short human-friendly string describing this configuration

  def calculate(design: schema.Design, compiler: Compiler): Any
}


// Abstract base class that defines the calculation type
sealed abstract class DseTypedObjective[T] extends DseObjective { self: Serializable =>
  def calculate(design: schema.Design, compiler: Compiler): T
}


// Abstract base class for parameter values
sealed abstract class DseObjectiveParameter[T] extends DseTypedObjective[Option[T]] with Serializable


case class DseFloatParameter(path: IndirectDesignPath) extends DseObjectiveParameter[Float] {
  override def objectiveToString = f"FloatParameter($path)"

  override def calculate(design: Design, compiler: Compiler): Option[Float] = {
    compiler.getParamValue(path) match {
      case Some(FloatValue(value)) => Some(value)
      case _ => None
    }
  }
}


case class DseIntParameter(path: IndirectDesignPath) extends DseObjectiveParameter[BigInt] {
  override def objectiveToString = f"IntParameter($path)"

  override def calculate(design: Design, compiler: Compiler): Option[BigInt] = {
    compiler.getParamValue(path) match {
      case Some(IntValue(value)) => Some(value)
      case _ => None
    }
  }
}


case class DseRangeParameter(path: IndirectDesignPath) extends DseObjectiveParameter[(Float, Float)] {
  // TODO support empty range case
  override def objectiveToString = f"RangeParameter($path)"

  override def calculate(design: Design, compiler: Compiler): Option[(Float, Float)] = {
    compiler.getParamValue(path) match {
      case Some(RangeValue(minValue, maxValue)) => Some((minValue, maxValue))
      case _ => None
    }
  }
}


case class DseBooleanParameter(path: IndirectDesignPath) extends DseObjectiveParameter[Boolean] {
  override def objectiveToString = f"BooleanParameter($path)"

  override def calculate(design: Design, compiler: Compiler): Option[Boolean] = {
    compiler.getParamValue(path) match {
      case Some(BooleanValue(value)) => Some(value)
      case _ => None
    }
  }
}


case class DseStringParameter(path: IndirectDesignPath) extends DseObjectiveParameter[String] {
  override def objectiveToString = f"StringParameter($path)"

  override def calculate(design: Design, compiler: Compiler): Option[String] = {
    compiler.getParamValue(path) match {
      case Some(TextValue(value)) => Some(value)
      case _ => None
    }
  }
}


// Stores footprint area so the data is not serialized and cached across multiple compilation runs
object DseObjectiveFootprintArea {
  private val footprintAreaCache = mutable.Map[String, Float]()

  def getFootprintArea(footprintName: String): Float = {
    footprintAreaCache.getOrElseUpdate(footprintName, {
      KicadFootprintUtil.getFootprintFile(footprintName) match {
        case Errorable.Success(footprintFile) =>
          val footprint = KicadParser.parseKicadFile(footprintFile)
          footprint.courtyardArea.getOrElse(0)
        case Errorable.Error(msg) => println(msg); 0  // TODO unified error logging
      }
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
