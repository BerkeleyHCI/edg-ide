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
    override def toString: String = "Subclass Refinements"
  }

  class InstanceRefinementsNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.subclasses.map { subclass =>
      (subclass.source, subclass.getReplacement)
    }.collect {
      case (edgrpc.Refinements.Subclass.Source.Path(srcPath), replaceType) =>
        new RefinementsDetailNode(ExprToString(srcPath), replaceType.toSimpleString)
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Subclass Refinements"
  }

  class ClassValuesNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.value)
    }.collect {
      case (edgrpc.Refinements.Value.Source.ClsParam(srcTypeParam), edgrpc.Refinements.Value.Value.Expr(expr)) =>
        new RefinementsDetailNode(
          srcTypeParam.getCls.toSimpleString + ":" + ExprToString(srcTypeParam.getParamPath),
          ExprToString(expr))
      case (edgrpc.Refinements.Value.Source.ClsParam(srcTypeParam), edgrpc.Refinements.Value.Value.Param(path)) =>
        new RefinementsDetailNode(
          srcTypeParam.getCls.toSimpleString + ":" + ExprToString(srcTypeParam.getParamPath),
          f"ParamValue(${ExprToString(path)}")
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Class Values Refinements"
  }

  class InstanceValuesNode(refinements: edgrpc.Refinements) extends ElementDetailNode {
    override lazy val children = refinements.values.map { value =>
      (value.source, value.value)
    }.collect {
      case (edgrpc.Refinements.Value.Source.Path(srcPath), edgrpc.Refinements.Value.Value.Expr(expr)) =>
        new RefinementsDetailNode(ExprToString(srcPath), ExprToString(expr))
      case (edgrpc.Refinements.Value.Source.Path(srcPath), edgrpc.Refinements.Value.Value.Param(path)) =>
        new RefinementsDetailNode(ExprToString(srcPath), f"ParamValue(${ExprToString(path)}")
    }
    override def getColumns(index: Int): String = ""
    override def toString: String = "Instance Value Refinements"
  }

  // Freeform node with arbitrary text and path
  class RefinementsDetailNode(target: String, value: String) extends ElementDetailNode {
    override lazy val children = Seq()
    override def getColumns(index: Int): String = value
    override def toString: String = target
  }
}
