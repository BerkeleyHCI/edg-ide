package edg_ide.dse
import com.intellij.openapi.project.Project
import edg.ElemBuilder
import edg.compiler.{Compiler, ExprValue, PythonInterface, TextValue}
import edg.util.Errorable
import edg.wir.ProtoUtil.ParamProtoToSeqMap
import edg.wir.{DesignPath, IndirectDesignPath}
import edg_ide.proven.ProvenStatus
import edg_ide.ui.{BlockVisualizerService, KicadParser}
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

  def calculate(design: schema.Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Any
}


// Abstract base class for numeric calculation results
sealed abstract class DseFloatObjective extends DseObjective { self: Serializable =>
  def calculate(design: schema.Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Float
}


sealed abstract class DseIntObjective extends DseObjective { self: Serializable =>
  def calculate(design: schema.Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Int
}


// Abstract base class for parameter values
case class DseObjectiveParameter(path: IndirectDesignPath, exprType: Class[_ <: ExprValue])
    extends DseObjective { self: Serializable =>
  override def objectiveToString = f"Parameter($path)"

  override def calculate(design: Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Option[ExprValue] = {
    compiler.getParamValue(path)
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


case class DseObjectiveFootprintArea(rootPath: DesignPath) extends DseFloatObjective with Serializable {
  override def objectiveToString = f"FootprintArea($rootPath)"

  override def calculate(design: Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Float = {
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
case class DseObjectiveFootprintCount(rootPath: DesignPath) extends DseIntObjective with Serializable {
  override def objectiveToString = f"FootprintCount($rootPath)"

  override def calculate(design: Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Int = {
    new DesignBlockMap[Int] {
      override def mapBlock(path: DesignPath, block: HierarchyBlock, blocks: SeqMap[String, Int]): Int = {
        val thisValue = if (path.startsWith(rootPath) && block.params.toSeqMap.contains("fp_footprint")) 1 else 0
        thisValue + blocks.values.sum
      }
    }.map(design)
  }
}


case class DseObjectivePrice() extends DseFloatObjective with Serializable {
  override def objectiveToString = f"Price"

  override def calculate(design: Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Float = {
    pythonInterface.runBackend(
      ElemBuilder.LibraryPath("electronics_lib.PriceGetter.GeneratePrice"), design, compiler.getAllSolved, Map()
    ).toOption.map(value => value(DesignPath()).toFloat).getOrElse(0)
  }
}


case class DseObjectiveUnprovenCount(rootPath: DesignPath) extends DseIntObjective with Serializable {
  override def objectiveToString = f"UnprovenCount($rootPath)"

  override def calculate(design: Design, compiler: Compiler, pythonInterface: PythonInterface, project: Project): Int = {
    new DesignBlockMap[Int] {
      override def mapBlock(path: DesignPath, block: HierarchyBlock, blocks: SeqMap[String, Int]): Int = {
        val thisProven = BlockVisualizerService(project).getProvenDatabase.getRecords(block.getSelfClass)
        val thisUnprovenCount = thisProven.getLatestStatus match {
          case ProvenStatus.Working => 0
          case _ => 1
        }
        thisUnprovenCount + blocks.values.sum
      }
    }.map(design)
  }
}
