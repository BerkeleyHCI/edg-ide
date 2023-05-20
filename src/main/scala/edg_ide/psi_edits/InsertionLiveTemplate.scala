package edg_ide.psi_edits

import com.intellij.codeInsight.highlighting.HighlightManager
import com.intellij.codeInsight.template.impl.{ConstantNode, TemplateState}
import com.intellij.codeInsight.template.{Template, TemplateBuilderImpl, TemplateEditingAdapter, TemplateManager}
import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.colors.EditorColors
import com.intellij.openapi.editor.markup.RangeHighlighter
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.popup.JBPopup
import com.intellij.openapi.ui.{ComponentValidator, ValidationInfo}
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi._

import javax.swing.JEditorPane
import scala.jdk.CollectionConverters.CollectionHasAsScala


trait InsertionLiveTemplateVariable[TreeType <: PsiElement, ElementType <: PsiElement] {
  def name: String

  // extract this variable's segment given the full sub-tree for the segment
  def extract(tree: TreeType): ElementType

  // whether to take the reference of the extracted variable's element, or use the full element
  def isReference: Boolean = false

  // validates the current segment, returning None for no errors, or Some(msg) if there is an error
  // and preventing further progress
  def validate(contents: String, templateState: TemplateState): Option[String] = None
}

object InsertionLiveTemplate {
  class Variable[TreeType <: PsiElement, ElementType <: PsiElement](
      val name: String, extractor: TreeType => ElementType,
      validator: (String, TemplateState) => Option[String] = (_, _) => None) extends InsertionLiveTemplateVariable[TreeType, ElementType] {
    override def extract(tree: TreeType): ElementType = extractor(tree)
    override def validate(contents: String, templateState: TemplateState): Option[String] = validator(contents, templateState)
  }

  class Reference[TreeType <: PsiElement, ElementType <: PsiElement](
      val name: String, extractor: TreeType => ElementType,
      validator: (String, TemplateState) => Option[String] = (_, _) => None) extends InsertionLiveTemplateVariable[TreeType, ElementType] {
    override def extract(tree: TreeType): ElementType = extractor(tree)
    override def validate(contents: String, templateState: TemplateState): Option[String] = validator(contents, templateState)
    override def isReference: Boolean = true
  }

  // utility validator for Python names, that also checks for name collisions (if class passed in; ignoring the
  // current template being edited)
  def validatePythonName(name: String, templateState: TemplateState, pyClass: Option[PyClass]): Option[String] = {
    val existingNames = pyClass match {
      case Some(pyClass) =>
        val templateRange = new TextRange(templateState.getCurrentExpressionContext.getTemplateStartOffset,
          templateState.getCurrentExpressionContext.getTemplateEndOffset)
        pyClass.getInstanceAttributes.asScala.filter(psi =>
          !templateRange.contains(psi.getTextRange)
        ).map(_.getName).toSet
      case None => Set[String]()
    }

    if (!LanguageNamesValidation.isIdentifier(PythonLanguage.getInstance(), name)) {
      Some("not a valid name")
    } else if (existingNames.contains(name)) {
      Some("name already used")
    } else {
      None
    }
  }
}


/** Utilities for insertion live templates.
  *
  * @param actionName name of the insertion action, for undo
  * @param after insertion location, after this element
  * @param newSubtree new subtree to be inserted
  * @param variables list of variables for the live template, each specified as a name
  *                  and function from the inserted subtree to the PSI element to be highlighted
  *                  and boolean of whether it is a reference
  */
class InsertionLiveTemplate[TreeType <: PyStatement](project: Project, editor: Editor, actionName: String,
                            after: PsiElement, newSubtree: TreeType,
                            variables: IndexedSeq[InsertionLiveTemplateVariable[TreeType, PsiElement]]) {
  private class TemplateListener(project: Project, editor: Editor,
                                 containingList: PyStatementList, newAssignIndex: Int,
                                 tooltip: JBPopup, highlighters: Iterable[RangeHighlighter]) extends TemplateEditingAdapter {
    private var currentTooltip = tooltip

    override def beforeTemplateFinished(state: TemplateState, template: Template): Unit = {
      super.beforeTemplateFinished(state, template)
    }

    override def templateFinished(template: Template, brokenOff: Boolean): Unit = {
      // this is called when:
      // - tab/enter-cycle through last element (brokenOff=false)
      // - escape (brokenOff=true)
      // this does NOT get called when making an edit outside the template (instead, templateCancelled is called)
      super.templateFinished(template, brokenOff)
      if (brokenOff) {
        templateAnyCancelled(template)
      }
      templateEnded(template)
    }

    override def currentVariableChanged(templateState: TemplateState, template: Template, oldIndex: Int, newIndex: Int): Unit = {
      // called when the selected template variable is changed (on tab/enter-cycling)
      super.currentVariableChanged(templateState, template, oldIndex, newIndex)
      if (newIndex >= 0) {  // newIndex=-1 when finishing, and variables are invalid
        val oldVariable = variables(oldIndex)
        val oldVariableValue = templateState.getCurrentExpressionContext.getVariableValue(oldVariable.name).getText
        val validationError = oldVariable.validate(oldVariableValue, templateState)
        currentTooltip.closeOk(null)
        validationError match {
          case Some(err) =>
            templateState.previousTab() // must be before the tooltip, so the tooltip is placed correctly
            currentTooltip = createTemplateTooltip(err, editor, true)
          case None => // ignored
        }
      }
    }

    override def waitingForInput(template: Template): Unit = {
      // called when the template starts
      super.waitingForInput(template)
    }

    override def templateCancelled(template: Template): Unit = {
      // this is called only when making an edit outside the current templated item
      // this does NOT get called when escaping during a template (instead, templateFinished is called with brokenOff=true)
      println(f"TemplateListener::templateCancelled")
      super.templateCancelled(template)
      //      templateAnyCancelled(template)
      templateEnded(template)
    }

    // Called when the template is cancelled (broken off or "cancelled")
    def templateAnyCancelled(template: Template): Unit = {
      val assignCandidate = containingList.getStatements()(newAssignIndex)
      require(assignCandidate.isInstanceOf[PyAssignmentStatement])
      writeCommandAction(project).withName("TODO cancel").compute(() => {
        assignCandidate.delete()
      })
    }

    // internal API, called when the template is ended for any reason (successful or not)
    def templateEnded(template: Template): Unit = {
      currentTooltip.closeOk(null)
      highlighters.foreach { highlighter =>
        HighlightManager.getInstance(project).removeSegmentHighlighter(editor, highlighter)
      }
    }
  }

  private def createTemplateTooltip(message: String, editor: Editor, isError: Boolean = false): JBPopup = {
    var hintHeight: Int = 0

    var validationInfo = new ValidationInfo(message, null)
    if (!isError) {
      validationInfo = validationInfo.asWarning()
    }
    val popupBuilder = ComponentValidator.createPopupBuilder(
      validationInfo,
      (editorPane: JEditorPane) => {
        hintHeight = editorPane.getPreferredSize.height
      }
    ).setCancelKeyEnabled(false)  // otherwise this eats the cancel keypress for the live template
    val popup = popupBuilder.createPopup()
    popup.showInBestPositionFor(editor)
    popup
  }

  def run(): Unit = {
    writeCommandAction(project).withName(actionName).compute(() => {
      val containingList = after.getParent.asInstanceOf[PyStatementList]
      val newStmt = containingList.addAfter(newSubtree, after).asInstanceOf[TreeType]
      val newStmtIndex = containingList.getStatements.indexOf(newStmt)

      // these must be constructed before template creation, other template creation messes up the locations
      val highlighters = new java.util.ArrayList[RangeHighlighter]()
      // flags = 0 means ignore esc, otherwise it eats the esc keypress
      HighlightManager.getInstance(project).addOccurrenceHighlight(editor,
        newStmt.getTextRange.getStartOffset, newStmt.getTextRange.getEndOffset,
        EditorColors.LIVE_TEMPLATE_INACTIVE_SEGMENT, 0, highlighters)

      new OpenFileDescriptor(project, containingList.getContainingFile.getVirtualFile, newStmt.getTextRange.getStartOffset)
          .navigate(true) // sets focus on the text editor so the user can type into the template
      editor.getCaretModel.moveToOffset(newStmt.getTextOffset) // needed so the template is placed at the right location

      val tooltip = createTemplateTooltip("this is a tooltip", editor)

      val builder = new TemplateBuilderImpl(newStmt)
      val variablePsis = variables.map { variable =>
        val variablePsi = variable.extract(newStmt)
        if (!variable.isReference) {
          builder.replaceElement(variablePsi, variable.name,
            new ConstantNode(variablePsi.getText), true)
        } else {
          builder.replaceElement(variablePsi.getReference, variable.name,
            new ConstantNode(variablePsi.getReference.getCanonicalText), true)
        }
        variablePsi
      }
      variablePsis.lastOption.foreach { lastVariablePsi =>
        builder.setEndVariableAfter(newStmt.getLastChild)
      }

      // must be called before building the template
      PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(editor.getDocument)

      // specifically must be an inline template (actually replace the PSI elements), otherwise the block of new code is inserted at the caret
      val template = builder.buildInlineTemplate()
      val templateListener = new TemplateListener(
        project, editor, containingList, newStmtIndex,
        tooltip, highlighters.asScala)
      val templateState = TemplateManager.getInstance(project).runTemplate(editor, template)
      templateState.addTemplateStateListener(templateListener)
    })
  }
}
