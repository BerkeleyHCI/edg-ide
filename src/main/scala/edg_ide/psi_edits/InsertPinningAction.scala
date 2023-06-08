package edg_ide.psi_edits

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiParserFacade}
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi._
import edg.compiler.ExprToString
import edg.util.Errorable
import edg.wir.ProtoUtil.PortProtoToSeqMap
import edg.{ElemBuilder, ExprBuilder}
import edg_ide.ui.PopupUtils
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}
import edgir.elem.elem
import edgir.ref.ref

import java.awt.event.MouseEvent

object InsertPinningAction {
  case class SelectPortItem(portPath: ref.LocalPath, assigned: Boolean) {
    override def toString: String = ExprToString(portPath) + Option.when(assigned)(" (assigned)").getOrElse("")
    def toCodeExpr: String = ExprToString(portPath)
  }

  protected def recursivePortPathsAndTypes(
      path: ref.LocalPath,
      port: elem.PortLike
  ): Seq[(ref.LocalPath, ref.LibraryPath)] = port.is match {
    case elem.PortLike.Is.Port(port) => Seq((path, port.getSelfClass))
    case elem.PortLike.Is.Bundle(port) =>
      Seq((path, port.getSelfClass)) ++
        port.ports.asPairs.flatMap { case (name, subport) =>
          val subpath = path.update(
            _.steps :+= ref.LocalStep().update(_.name := name)
          )
          recursivePortPathsAndTypes(subpath, subport)
        }
    case elem.PortLike.Is.LibElem(lib) => Seq((path, lib))
    // TODO support Array and others - but arrays won't be seen here
    case _ => Seq()
  }

  protected def circuitPortsOf(path: ref.LocalPath, port: elem.PortLike, project: Project): Seq[ref.LocalPath] = {
    // TODO refactor into CircuitBlock? doesn't really belong in VoltagePorts
    val circuitPortType =
      ElemBuilder.LibraryPath("electronics_model.VoltagePorts.CircuitPort") // TODO belongs in shared place?
    val circuitPortClass = DesignAnalysisUtils.pyClassOf(circuitPortType, project).get

    recursivePortPathsAndTypes(path, port).map { case (path, portType) =>
      (path, DesignAnalysisUtils.pyClassOf(portType, project).toOption)
    }.collect {
      case (path, Some(portClass))
        if portClass.isSubclass(circuitPortClass, TypeEvalContext.codeAnalysis(project, null)) =>
        path
    }
  }

  def createInsertPinningFlow(
      block: elem.HierarchyBlock,
      pin: String,
      pinning: Map[String, ref.LocalPath],
      event: MouseEvent,
      project: Project,
      continuation: (ref.LocalPath, PsiElement) => Unit
  ): Errorable[Unit] = exceptable {
    // TODO Dedup w/ InsertFootprintAction?
    val blockClass = DesignAnalysisUtils.pyClassOf(block.getSelfClass, project).exceptError
    val after = InsertAction.getCaretAtFileOfType(
      blockClass.getContainingFile,
      classOf[PyStatementList],
      project
    ).exceptError
    val containingPsiFunction = PsiTreeUtil.getParentOfType(after, classOf[PyFunction])
      .exceptNull(s"not in a function in ${after.getContainingFile.getName}")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
      .exceptNull(s"not in a class in ${containingPsiFunction.getContainingFile.getName}")
    requireExcept(containingPsiClass == blockClass, s"not in ${blockClass.getName}")

    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
      .headOption.exceptNone(s"function ${containingPsiFunction.getName} has no self")
      .getName

    val containingCall = PsiTreeUtil.getParentOfType(after, classOf[PyCallExpression])
      .exceptNull(s"not in a function call")
    requireExcept(containingCall.getCallee.textMatches(s"$selfName.footprint"), "not in a self.footprint call")
    val argDict = containingCall.getArgument(2, "pinning", classOf[PyExpression])
      .exceptNull("call has no pinning argument")
      .instanceOfExcept[PyDictLiteralExpression]("pinning argument is not a dict literal")

    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(argDict)
    val matchingKv = argDict.getElements.filter { elt =>
      elt.getKey match {
        case strLit: PyStringLiteralExpression => strLit.getStringValue == pin
        case _ => false
      }
    }.toSeq match {
      case Seq(kv) => Some(kv)
      case Seq() => None
      case _ => exceptable.fail(s"more than one pinning entry for $pin")
    }

    val priorPinned = pinning.values.toSet

    val circuitPortItems = block.ports.asPairs.flatMap { case (name, port) =>
      val portPath = ExprBuilder.Ref(name)
      circuitPortsOf(portPath, port, project)
    }.toSeq.exceptEmpty(s"block contains no CircuitPorts")
      .map { path => SelectPortItem(path, priorPinned.contains(path)) }
      .sortWith { case (l, r) => !l.assigned && r.assigned }

    PopupUtils.createMenuPopup(s"Connect port to $pin", circuitPortItems, event) { selected =>
      val selectedRefCode = "self." + selected.toCodeExpr // TODO should use a dedicated code construct
      val newElt = matchingKv match {
        case Some(kv) => // modify existing
          val newValue = psiElementGenerator.createExpressionFromText(languageLevel, selectedRefCode)
          writeCommandAction(project).withName(s"Update pin $pin in ${blockClass.getName}").compute(() => {
            kv.getValue.replace(newValue)
          })

        case None => // append new
          val newKv = psiElementGenerator.createExpressionFromText(
            languageLevel,
            s"{'$pin': $selectedRefCode}"
          ) // apparently it can't directly parse a KeyValueExpr
            .asInstanceOf[PyDictLiteralExpression]
            .getElements.head

          // for some reason, PyElementGenerator.getInstance(project).createNewLine inserts two spaces
          val newline = PsiParserFacade.getInstance(project).createWhiteSpaceFromText("\n")

          writeCommandAction(project).withName(s"Set pin $pin in ${blockClass.getName}").compute(() => {
            // and apparently PyDictLiteral doesn't have an .addArgument like PyArgumentList
            // adapted (poorly?) from PyArgumentListImpl.addArgument
            argDict.getElements.lastOption match {
              case Some(lastArg) =>
                psiElementGenerator.insertItemIntoListRemoveRedundantCommas(argDict, lastArg, newKv)
              case None => // empty dict
                // InsertIntoList gets super confused with a null previous elt
                argDict.addAfter(newKv, argDict.getFirstChild)
            }
          })
      }
      continuation(selected.portPath, newElt)
    }
  }
}
