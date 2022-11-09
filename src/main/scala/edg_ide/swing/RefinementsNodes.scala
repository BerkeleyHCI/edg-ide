package edg_ide.swing

import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.ExprToString
import edgrpc.hdl.{hdl => edgrpc}


object RefinementsNodes {
  class ClassRefinementsNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.subclasses.map { subclass =>
      (subclass.source, subclass.getReplacement)
    }.collect {
      case (edgrpc.Refinements.Subclass.Source.Cls(srcType), replaceType) =>
        RefinementsDetailNode(srcType.toSimpleString, replaceType.toSimpleString)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Class Subclasses"
  }

  class InstanceRefinementsNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.subclasses.map { subclass =>
      (subclass.source, subclass.getReplacement)
    }.collect {
      case (edgrpc.Refinements.Subclass.Source.Path(srcPath), replaceType) =>
        RefinementsDetailNode(ExprToString(srcPath), replaceType.toSimpleString)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Subclasses"
  }

  class ClassValuesNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.getValue)
    }.collect {
      case (edgrpc.Refinements.Value.Source.ClsParam(srcTypeParam), replaceValue) =>
        RefinementsDetailNode(
          srcTypeParam.getCls.toSimpleString + ":" + ExprToString(srcTypeParam.getParamPath),
          ExprToString(replaceValue))
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Class Values"
  }

  class InstanceValuesNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.getValue)
    }.collect {
      case (edgrpc.Refinements.Value.Source.Path(srcPath), replaceValue) =>
        RefinementsDetailNode(ExprToString(srcPath), ExprToString(replaceValue))
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Values"
  }

  // Freeform node with arbitrary text and path
  case class RefinementsDetailNode(target: String, value: String) extends ElementDetailNode {
    override lazy val children = Seq()
    override def getColumns(index: Int): String = value
    override def toString: String = target
  }
}
