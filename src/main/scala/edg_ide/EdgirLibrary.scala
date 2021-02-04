package edg_ide

import edg.schema.schema
import edg.elem.elem
import edg.ref.ref.{LibraryPath, LocalStep}


class NoLibraryElement(err: String) extends Exception(err)
class LibraryElementTypeMismatch(err: String) extends Exception(err)


class EdgirLibrary(ir: schema.Library) {
  val rootPath = LibraryPath(target=None)  // dummy path to serve as the root

  val elts: Map[LibraryPath, schema.Library.NS.Val.Type] = ir.root.getOrElse(schema.Library.NS())
      .members.map { case (name, member) =>
    val libraryPath = LibraryPath(target=Some(LocalStep(step=LocalStep.Step.Name(name))))
    (libraryPath, member.`type`)
  }

  // Starting at some LibraryPath, return the reachable set (all child subclass LibraryPaths)
  def childrenRecursive(path: LibraryPath,
                        eltChildren: Map[LibraryPath, Set[LibraryPath]]): Set[LibraryPath] = {
    eltChildren.get(path) match {
      case Some(childPaths) => Set(path) ++ childPaths.flatMap { childPath =>
        childrenRecursive(childPath, eltChildren)
      }
      case None => Set()
    }
  }

  val blockChildren: Map[LibraryPath, Set[LibraryPath]] = elts.toSeq
      .collect { case (eltPath, schema.Library.NS.Val.Type.HierarchyBlock(elt)) =>
        elt.superclasses match {case Nil => Seq((rootPath, eltPath))  // no superclass: generate a dummy root superclass
            case superclasses =>  // otherwise, generate all pairs
              superclasses.map(superclassPath => (superclassPath, eltPath))
          }
      }.flatten  // pairs of (superclass, child class)
      .groupBy { case (superclassPath, eltPath) => superclassPath }  // superclass -> Seq[(superclass, child class)]
      .mapValues( superclassPairs => superclassPairs.map(_._2).toSet).toMap

  val reachableBlocks = childrenRecursive(rootPath, blockChildren)
  val unreachableBlocks = blockChildren.keys.toSet -- reachableBlocks -- Set(rootPath)

  def getBlock(path: LibraryPath): elem.HierarchyBlock = {
    elts.get(path) match {
      case None => throw new NoLibraryElement(s"Not in library: $path")
      case Some(elt) => elt match {
        case schema.Library.NS.Val.Type.HierarchyBlock(thing) => thing
        case elt => throw new LibraryElementTypeMismatch(
          s"Library type mismatch, expected HierarchyBlock, got ${elt.getClass}: $path")
      }
    }
  }

  def getPort(path: LibraryPath): elem.Port = {
    elts.get(path) match {
      case None => throw new NoLibraryElement(s"Not in library: $path")
      case Some(elt) => elt match {
        case schema.Library.NS.Val.Type.Port(thing) => thing
        case elt => throw new LibraryElementTypeMismatch(
          s"Library type mismatch, expected Port, got ${elt.getClass}: $path")
      }
    }
  }

  def getBundle(path: LibraryPath): elem.Bundle = {
    elts.get(path) match {
      case None => throw new NoLibraryElement(s"Not in library: $path")
      case Some(elt) => elt match {
        case schema.Library.NS.Val.Type.Bundle(thing) => thing
        case elt => throw new LibraryElementTypeMismatch(
          s"Library type mismatch, expected Bundle, got ${elt.getClass}: $path")
      }
    }
  }

  def getLink(path: LibraryPath): elem.Link = {
    elts.get(path) match {
      case None => throw new NoLibraryElement(s"Not in library: $path")
      case Some(elt) => elt match {
        case schema.Library.NS.Val.Type.Link(thing) => thing
        case elt => throw new LibraryElementTypeMismatch(
          s"Library type mismatch, expected Link, got ${elt.getClass}: $path")
      }
    }
  }
}
