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
        new RefinementsDetailNode(srcType.toSimpleString, replaceType.toSimpleString)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Subclass Refinement"
  }

  class InstanceRefinementsNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.subclasses.map { subclass =>
      (subclass.source, subclass.getReplacement)
    }.collect {
      case (edgrpc.Refinements.Subclass.Source.Path(srcPath), replaceType) =>
        new RefinementsDetailNode(ExprToString(srcPath), replaceType.toSimpleString)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Subclass Refinement"
  }

  class ClassValuesNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.getValue)
    }.collect {
      case (edgrpc.Refinements.Value.Source.ClsParam(srcTypeParam), replaceValue) =>
        new RefinementsDetailNode(
          srcTypeParam.getCls.toSimpleString + ":" + ExprToString(srcTypeParam.getParamPath),
          ExprToString(replaceValue))
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Class Values Refinement"
  }

  class InstanceValuesNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.getValue)
    }.collect {
      case (edgrpc.Refinements.Value.Source.Path(srcPath), replaceValue) =>
        new RefinementsDetailNode(ExprToString(srcPath), ExprToString(replaceValue))
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Value Refinement"
  }

  // Freeform node with arbitrary text and path
  class RefinementsDetailNode(target: String, value: String) extends ElementDetailNode {
    override lazy val children = Seq()
    override def getColumns(index: Int): String = value
    override def toString: String = target
  }
}
