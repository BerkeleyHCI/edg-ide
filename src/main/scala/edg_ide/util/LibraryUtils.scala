package edg_ide.util

import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.Library
import edg.wir.ProtoUtil.ParamProtoToSeqMap
import edgir.ref.ref

object LibraryUtils {
  // returns the top-most superclass that defines some parameter name
  // returns None if thisClass does not define the parameter, and errors out if there are multiple top-most superclasses
  def blockParamGetDefiningSuperclass(
      library: Library,
      thisClass: ref.LibraryPath,
      paramName: String
  ): Option[ref.LibraryPath] = {
    val thisBlock = library.getBlock(thisClass, ignoreRefinements = true).get
    if (thisBlock.params.get(paramName).isEmpty) {
      return None
    }
    val definingSuperclasses = thisBlock.superclasses.flatMap { superclass =>
      blockParamGetDefiningSuperclass(library, superclass, paramName)
    }.distinct
    definingSuperclasses match {
      case Seq() => Some(thisClass)
      case Seq(baseClass) => Some(baseClass)
      case _ =>
        throw new IllegalArgumentException(s"multiple superclasses of ${thisClass.toSimpleString} defines $paramName")
    }
  }
}
