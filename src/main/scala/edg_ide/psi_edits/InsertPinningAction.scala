package edg_ide.psi_edits

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.jetbrains.python.psi.types.TypeEvalContext
import edg.ElemBuilder
import edg.elem.elem
import edg.ref.ref
import edg.util.Errorable
import edg_ide.ui.PopupUtils
import edg_ide.util.ExceptionNotifyImplicits.ExceptSeq
import edg_ide.util.{DesignAnalysisUtils, exceptable}

import java.awt.event.MouseEvent


object InsertPinningAction {
  case class SelectPortItem(portPath: Seq[String]) {
    override def toString: String = portPath.mkString(".")
  }

  protected def recursivePortPathsAndTypes(path: Seq[String], port: elem.PortLike):
      Seq[(Seq[String], ref.LibraryPath)] = port.is match {
    case elem.PortLike.Is.Port(port) => Seq((path, port.superclasses.head))
    case elem.PortLike.Is.Bundle(port) =>
      Seq((path, port.superclasses.head)) ++
          port.ports.flatMap { case (name, subport) =>
            recursivePortPathsAndTypes(path :+ name, subport)
          }
    case elem.PortLike.Is.LibElem(lib) => Seq((path, lib))
    // TODO support Array and others - but arrays won't be seen here
    case _ => Seq()
  }

  protected def circuitPortsOf(name: String, port: elem.PortLike, project: Project): Seq[Seq[String]] = {
    // TODO refactor into CircuitBlock? doesn't really belong in VoltagePorts
    val circuitPortType = ElemBuilder.LibraryPath("electronics_model.VoltagePorts.CircuitPort")  // TODO belongs in shared place?
    val circuitPortClass = DesignAnalysisUtils.pyClassOf(circuitPortType, project).get

    recursivePortPathsAndTypes(Seq(name), port).map { case (path, portType) =>
      (path, DesignAnalysisUtils.pyClassOf(portType, project).toOption)
    }.collect { case (path, Some(portClass)) if portClass.isSubclass(circuitPortClass, TypeEvalContext.codeAnalysis(project, null)) =>
      path
    }
  }

  def createInsertPinningFlow(block: elem.HierarchyBlock, pin: String,
                              event: MouseEvent, project: Project,
                              continuation: (Seq[String], PsiElement) => Unit): Errorable[Unit] = exceptable {
    val circuitPortItems = block.ports.flatMap { case (name, port) =>
      circuitPortsOf(name, port, project)
    }.toSeq.exceptEmpty(s"block contains no CircuitPorts")
        .map { path => SelectPortItem(path) }

    PopupUtils.createMenuPopup(s"Connect port to $pin", circuitPortItems, event) { selected =>
      println(s"$selected")
    }
  }
}
