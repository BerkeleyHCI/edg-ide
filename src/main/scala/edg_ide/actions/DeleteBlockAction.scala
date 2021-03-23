package edg_ide.actions

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.pom.Navigatable
import com.intellij.psi.PsiElement
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.impl.references.PyQualifiedReference
import com.jetbrains.python.psi.{PyAssignmentExpression, PyAssignmentStatement, PyExpression, PyStatement}
import edg.util.Errorable
import edg_ide.PsiUtils
import edg_ide.ui.PopupUtils
import edg_ide.util.exceptable

import scala.jdk.CollectionConverters.CollectionHasAsScala

object DeleteBlockAction {
  // TODO dedup w/ NavigateToBlockAction?
  case class NavigateNode(desc: String, action: () => Unit) {
    override def toString: String = desc
  }

  def createDeleteBlockFlow(assignment: PyAssignmentStatement, actionName: String,
                            project: Project,
                            continuation: PsiElement => Unit): Errorable[() => Unit] = exceptable {
    val assignmentTargets = assignment.getRawTargets.toSet
    val searchScope = GlobalSearchScope.projectScope(project)
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

      NavigateNode(s"Goto ${statement.getText}$fileLine", () => {
        reference.asInstanceOf[Navigatable].navigate(true)
      })
    } :+ NavigateNode(
      "Unsafe Delete", () => {
        val prev = assignment.getPrevSibling
        writeCommandAction(project).withName(actionName).compute(() => {
          assignment.delete()
        })

        continuation(prev)
      }
    )

    def deleteBlockFlow: Unit = {
      if (items.length == 1) {
        items.head.action()
      } else {
        PopupUtils.createMenuPopup(s"Remaining references to ${assignment.getLeftHandSideExpression.getText}", items, project) { selected =>
          selected.action()
        }
      }
    }
    () => deleteBlockFlow
  }
}
