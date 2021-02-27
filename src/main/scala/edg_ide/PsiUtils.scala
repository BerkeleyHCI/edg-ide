package edg_ide

import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.jetbrains.python.psi.{LanguageLevel, PyAssignmentStatement, PyClass, PyElementGenerator, PyRecursiveElementVisitor, PyReferenceExpression, PyTargetExpression}
import edg.util.Errorable

import scala.collection.mutable

object PsiUtils {
  def fileLineOf(element: PsiElement, project: Project): Errorable[String] = {
    val psiFile = Errorable(element.getContainingFile, "no file")
    val psiDocumentManager = PsiDocumentManager.getInstance(project)
    val psiDocument = psiFile.map("no document")(psiDocumentManager.getDocument(_))
    (psiFile + psiDocument).map { case (psiFile, psiDocument) =>
      val lineNumber = psiDocument.getLineNumber(element.getTextOffset)
      s"${psiFile.getName}:$lineNumber"
    }
  }

  def findAssignmentsTo(container: PyClass, targetName: String,
                        project: Project): Seq[PyAssignmentStatement] = {
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    println(s"VisitClass $container")

    container.getMethods.toSeq.collect { method =>
      val parameters = method.getParameterList.getParameters
      val selfName = parameters(0).getName
      val targetReference = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(method),
        s"$selfName.$targetName"
      ).asInstanceOf[PyReferenceExpression]

      println(s"VisitMethod ${method.getName}  $selfName  ${targetReference.getText}")

      if (parameters.nonEmpty) {
        // TODO support ElementDict and array ops
        val methodAssigns = mutable.ListBuffer[PyAssignmentStatement]()
        method.accept(new PyRecursiveElementVisitor() {
          override def visitPyAssignmentStatement(node: PyAssignmentStatement): Unit = {
            println(s"    VisitAssign $node")
            if (node.getTargets.exists(expr => expr.textMatches(targetReference))) {
              methodAssigns += (node)
            }
          }
        })
        methodAssigns.toSeq
      } else {
        Seq()
      }
    }.flatten
  }

  // Return all siblings (including itself) of a PsiElement
  @deprecated
  def psiSiblings(element: PsiElement): Seq[PsiElement] = element match {
    case element: PsiElement => Seq(element) ++ psiSiblings(element.getNextSibling)
    case _ => Seq()  // null case
  }

  // Returns whether the two Seq of PsiElements produce text matches (individual element-wise)
  @deprecated
  def psiTextMatches(seq1: Seq[PsiElement], seq2: Seq[PsiElement]): Boolean = {  // TODO better match impl?
    seq1.lengthCompare(seq2.length) == 0 && (seq1.zip(seq2).map { case (val1, val2) =>
      val1.textMatches(val2)
    }.forall(x => x))
  }

  /**
    * For a PyReferenceExpression of the form self.something, return Some("something"), or None
    */
  def psiSelfReference(project: Project, element: PyReferenceExpression): Option[String] = element match {
    case element: PyReferenceExpression =>
      val psiElementGenerator = PyElementGenerator.getInstance(project)
      val refElement = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(element),
        "self.placeholder"
      ).asInstanceOf[PyReferenceExpression]

      val elemSiblings = psiSiblings(element.getChildren()(0))
      val refSiblings = psiSiblings(refElement.getChildren()(0))

      if (psiTextMatches(elemSiblings.slice(0, elemSiblings.length - 1),
        refSiblings.slice(0, refSiblings.length - 1))) {
        Some(elemSiblings.last.getText)
      } else {
        None
      }
    case _ => None
  }

  /**
    * For a PyTargetExpression of the form self.something, return Some("something"), or None
    */
  def psiSelfTarget(project: Project, element: PyTargetExpression): Option[String] = element match {
    case element: PyTargetExpression =>
      val psiElementGenerator = PyElementGenerator.getInstance(project)
      val refElement = psiElementGenerator.createExpressionFromText(LanguageLevel.forElement(element),
        "self.placeholder"  // TODO: should actually use an assignment so this is a PyTargetExpression?
      ).asInstanceOf[PyReferenceExpression]

      val elemSiblings = psiSiblings(element.getChildren()(0))
      val refSiblings = psiSiblings(refElement.getChildren()(0))

      if (psiTextMatches(elemSiblings.slice(0, elemSiblings.length - 1),
        refSiblings.slice(0, refSiblings.length - 1))) {
        Some(elemSiblings.last.getText)
      } else {
        None
      }
    case _ =>
      None
  }
}
