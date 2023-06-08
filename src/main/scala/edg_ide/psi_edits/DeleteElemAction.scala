package edg_ide.psi_edits

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.pom.Navigatable
import com.intellij.psi.PsiElement
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.search.{GlobalSearchScope, LocalSearchScope}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.PsiUtils
import edg_ide.ui.PopupUtils
import edg_ide.util.exceptable

import scala.jdk.CollectionConverters.CollectionHasAsScala

/** Action to delete an element (any member of a Block, including sub-Block and Ports).
  *
  * TODO: this isn't visible because the static analysis is so leaky it might as well not be useful
  */
object DeleteElemAction {
  // TODO dedup w/ NavigateToBlockAction?
  case class NavigateNode(desc: String, action: () => Unit) {
    override def toString: String = desc
  }

  def createDeleteElemFlow(
      assignment: PyAssignmentStatement,
      actionName: String,
      project: Project,
      continuation: PsiElement => Unit
  ): Errorable[() => Unit] = exceptable {
    val assignmentTargets = assignment.getRawTargets.toSet

    val containingFunctionName = Option(PsiTreeUtil.getParentOfType(assignment, classOf[PyFunction]))
      .map(_.getName).getOrElse("")
    val (searchScope, scopeDesc) = if (containingFunctionName == "__init__") { // assume only init visible elsewhere
      (GlobalSearchScope.projectScope(project), "public variable")
    } else {
      val containingClass = Option(PsiTreeUtil.getParentOfType(assignment, classOf[PyClass])).getOrElse(assignment)
      (new LocalSearchScope(containingClass), "internal variable")
    }
    val allReferences = assignmentTargets.toSeq.flatMap { assignmentTarget =>
      ReferencesSearch.search(assignmentTarget, searchScope).findAll().asScala
    }
    // TODO for things defined in contents (instead of init), this can turn up a lot of dynamic usages
    val references = allReferences.map {
      _.getElement()
    }.filter {
      case ref: PyExpression => !assignmentTargets.contains(ref)
      case _ => true
    }

    val items = references.map { reference =>
      val fileLine = PsiUtils.fileLineOf(reference, project)
        .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
      val statement = Option(PsiTreeUtil.getParentOfType(reference, classOf[PyStatement]))
        .getOrElse(reference)

      NavigateNode(
        s"Goto ${statement.getText}$fileLine",
        () => {
          reference.asInstanceOf[Navigatable].navigate(true)
        }
      )
    } :+ NavigateNode(
      "Unsafe Delete",
      () => {
        val prev = Seq(
          // Can't use getNextSibling (and related) because it can return a deleted whitespace
          Option(PsiTreeUtil.getNextSiblingOfType(assignment, classOf[PyStatement])),
          Option(PsiTreeUtil.getPrevSiblingOfType(assignment, classOf[PyStatement])),
          Option(assignment.getParent),
        ).flatten.head
        writeCommandAction(project).withName(actionName).compute(() => {
          assignment.delete()
        })

        continuation(prev)
      }
    )

    def deleteElemFlow: Unit = {
      if (items.length == 1) {
        items.head.action()
      } else {
        PopupUtils.createMenuPopup(
          s"Remaining references to $scopeDesc ${assignment.getLeftHandSideExpression.getText}",
          items,
          project
        ) { selected =>
          selected.action()
        }
      }
    }
    () => deleteElemFlow
  }
}
