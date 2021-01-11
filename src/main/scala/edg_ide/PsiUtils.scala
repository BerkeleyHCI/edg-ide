package edg_ide

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.jetbrains.python.psi.{LanguageLevel, PyElementGenerator, PyReferenceExpression, PyTargetExpression}

object PsiUtils {
  // Return all siblings (including itself) of a PsiElement
  def psiSiblings(element: PsiElement): Seq[PsiElement] = element match {
    case element: PsiElement => Seq(element) ++ psiSiblings(element.getNextSibling)
    case _ => Seq()  // null case
  }

  // Returns whether the two Seq of PsiElements produce text matches (individual element-wise)
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
