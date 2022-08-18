package edg_ide.dse
import edg.compiler.{ExprValue, FloatValue, IntValue, RangeValue}
import edg.wir.{DesignPath, IndirectDesignPath}
import edgir.elem.elem.HierarchyBlock
import edgir.schema.schema
import edgir.schema.schema.Design

import scala.collection.SeqMap


// Abstract base class for all design space objectives - some function of the design
// that produces a metric that is useful to the user, eg cost or some design parameter
sealed trait DseObjective {
  // TODO: also needs libraries and external sources?
  def calculate(design: schema.Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float]
}

// Extracts the value of a single parameter
case class DseObjectiveParameter(path: DesignPath) extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = {
    values.get(path.asIndirect) match {
      case Some(FloatValue(value)) => Some(value)
      case Some(IntValue(value)) => Some(value.toFloat)
      case Some(RangeValue(lower, upper)) => Some((lower + upper) / 2)  // TODO support more data types
      case _ => None
    }
  }
}

// Sums up the value of some parameter
case class DseObjectiveSummation() extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = ???
}

case class DseObjectiveFootprintArea() extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = ???
}

// Counts the total number of footprints
case class DseObjectiveFootprintCount(rootDesignPath: DesignPath = DesignPath()) extends DseObjective {
  class FootprintCountDesignMap extends DesignBlockMap[Int] {
    override def mapBlock(path: DesignPath, block: HierarchyBlock, blocks: SeqMap[String, Int]): Int = {
      val thisCount = if (path.startsWith(rootDesignPath) && block.params.contains("fp_footprint")) 1 else 0
      thisCount + blocks.values.sum  // above rootDesignPath still must sum up contents
    }
  }
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = {
    Some(new FootprintCountDesignMap().map(design).toFloat)
  }
}

case class DseObjectiveCost() extends DseObjective {
  override def calculate(design: Design, values: Map[IndirectDesignPath, ExprValue]): Option[Float] = ???
}
