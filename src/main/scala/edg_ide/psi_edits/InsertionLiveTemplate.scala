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
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile, PsiWhiteSpace}
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi._

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala


trait InsertionLiveTemplateVariable[TreeType <: PsiElement] {
  def name: String

  // extract this variable's segment given the full sub-tree for the segment
  def extract(tree: TreeType): PsiElement

  // whether to take the reference of the extracted variable's element, or use the full element
  def isReference: Boolean = false

  // validates the current segment, returning None for no errors, or Some(msg) if there is an error
  // and preventing further progress
  def validate(contents: String, templateState: TemplateState): Option[String] = None

  // sets the default text value for this variable
  def getDefaultValue(variablePsi: PsiElement): String
}

object InsertionLiveTemplate {
  class Variable[TreeType <: PsiElement](
      val name: String, extractor: TreeType => PsiElement,
      validator: (String, TemplateState) => Option[String] = (_, _) => None,
      defaultValue: Option[String] = None) extends InsertionLiveTemplateVariable[TreeType] {
    override def extract(tree: TreeType): PsiElement = extractor(tree)
    override def validate(contents: String, templateState: TemplateState): Option[String] = validator(contents, templateState)
    override def getDefaultValue(variablePsi: PsiElement): String = defaultValue.getOrElse(variablePsi.getText)
  }

  class Reference[TreeType <: PsiElement](
      val name: String, extractor: TreeType => PsiElement,
      validator: (String, TemplateState) => Option[String] = (_, _) => None,
      defaultValue: Option[String] = None) extends InsertionLiveTemplateVariable[TreeType] {
    override def extract(tree: TreeType): PsiElement = extractor(tree)
    override def validate(contents: String, templateState: TemplateState): Option[String] = validator(contents, templateState)
    override def isReference: Boolean = true
    override def getDefaultValue(variablePsi: PsiElement): String = defaultValue.getOrElse(variablePsi.getReference.getCanonicalText)
  }

  // utility validator for Python names, that also checks for name collisions (if class passed in; ignoring the
  // current template being edited)
  def validatePythonName(name: String, templateState: TemplateState, pyClass: Option[PyClass]): Option[String] = {
    val existingNames = pyClass match {
      case Some(pyClass) =>
        val templateRange = new TextRange(templateState.getExpressionContextForSegment(0).getTemplateStartOffset,
          templateState.getExpressionContextForSegment(0).getTemplateEndOffset)
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

  // deletes the template text, ending the template by cancellation.
  def deleteTemplate(templateState: TemplateState): Unit = {
    val templateExpression = templateState.getExpressionContextForSegment(0)
    val templateEndOffset = templateExpression.getTemplateEndOffset
    var templateElem = templateExpression.getPsiElementAtStartOffset

    // separate the traversal from deletion, so the end offsets are stable as we build the deletion list
    val deleteElems = mutable.ListBuffer[PsiElement]()
    while (templateElem != null && templateElem.getTextOffset <= templateEndOffset) {
      if (templateElem.getTextRange.getEndOffset <= templateExpression.getTemplateEndOffset) {
        deleteElems.append(templateElem)
        templateElem = templateElem.getNextSibling
      } else {  // otherwise recurse into element to get a partial range
        templateElem = templateElem.getFirstChild
      }
    }

    // delete the PSI elements, this also cancels the template (as if the user typed outside the template)
    deleteElems.foreach { deleteElem =>
      if (deleteElem.isValid) {  // guard in case the element changed from a prior deletion
        deleteElem.delete()
      }
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
                            variables: IndexedSeq[InsertionLiveTemplateVariable[TreeType]]) {
  private val kHelpTooltip = "[Enter] next; [Esc] end"

  // hooks to be overridden in subclasses
  // this is called when the template finishes (either cycling past last variable, or esc-ing out, but not
  // making edits outside), state reflects the post-completion state
  protected def onTemplateCompleted(state: TemplateState, brokenOff: Boolean): Unit = { }

  private class TemplateListener(project: Project, editor: Editor,
                                 tooltip: JBPopup, highlighters: Iterable[RangeHighlighter]) extends TemplateEditingAdapter {
    private var currentTooltip = tooltip
    private var finishedTemplateState: Option[TemplateState] = None

    override def beforeTemplateFinished(state: TemplateState, template: Template): Unit = {
      super.beforeTemplateFinished(state, template)
      finishedTemplateState = Some(state)  // save the state for when we have brokenOff
    }

    override def templateFinished(template: Template, brokenOff: Boolean): Unit = {
      // this is called when:
      // - tab/enter-cycle through last element (brokenOff=false)
      // - escape (brokenOff=true)
      // this does NOT get called when making an edit outside the template (instead, templateCancelled is called)
      super.templateFinished(template, brokenOff)
      templateEnded(template)
      onTemplateCompleted(finishedTemplateState.get, brokenOff)
    }

    private var lastChangeWasRevert = false
    override def currentVariableChanged(templateState: TemplateState, template: Template, oldIndex: Int, newIndex: Int): Unit = {
      // called when the selected template variable is changed (on tab/enter-cycling)
      super.currentVariableChanged(templateState, template, oldIndex, newIndex)
      var variableReverted = false

      // don't validate when newIndex = -1, that indicates the template has ended and nothing here matters
      if (oldIndex < variables.length && newIndex >= 0) {
        val oldVariable = variables(oldIndex)
        val oldVariableValue = templateState.getVariableValue(oldVariable.name).getText
        val validationError = oldVariable.validate(oldVariableValue, templateState)
        currentTooltip.closeOk(null)
        validationError match {
          case Some(err) =>  // TODO: this does nothing when the template is finishing
            lastChangeWasRevert = true  // avoid showing popup when this is called again from previousTab()
            variableReverted = true
            if (newIndex > oldIndex) {
              templateState.previousTab() // must be before the tooltip, so the tooltip is placed correctly
            } else {
              templateState.nextTab()
            }
            currentTooltip = createTemplateTooltip(f"${oldVariable.name} | $err", editor, true)
          case None =>  // ignored
        }
      }

      if (!variableReverted) {
        if (newIndex >= 0 && newIndex < variables.length && !lastChangeWasRevert) {
          currentTooltip = createTemplateTooltip(variables(newIndex).name, editor)
        }
        lastChangeWasRevert = false

        if (newIndex == variables.length) { // bypass guard
          templateState.gotoEnd(false)
        }
      }
    }

    override def waitingForInput(template: Template): Unit = {
      // called when the template starts
      super.waitingForInput(template)
    }

    override def templateCancelled(template: Template): Unit = {
      // this is called only when making an edit outside the current templated item
      //   including backspacing past the beginning of the item, or editing a different templated item
      // this is called BEFORE currentVariableChanged
      // this does NOT get called when escaping during a template (instead, templateFinished is called with brokenOff=true)
      super.templateCancelled(template)
      templateEnded(template)
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
    var validationInfo = new ValidationInfo(message, null)
    if (!isError) {
      validationInfo = validationInfo.asWarning()
    }
    val popupBuilder = ComponentValidator.createPopupBuilder(
      validationInfo, _ => ()
    ).setCancelKeyEnabled(false)  // otherwise this eats the cancel keypress for the live template
    val popup = popupBuilder.createPopup()
    popup.showInBestPositionFor(editor)
    popup
  }

  // starts this template and returns the TemplateState
  def run(): TemplateState = {
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

      val tooltip = createTemplateTooltip(f"${variables.head.name} | $kHelpTooltip", editor)

      val builder = new TemplateBuilderImpl(newStmt)
      val variablePsis = variables.map { variable =>
        val variablePsi = variable.extract(newStmt)
        if (!variable.isReference) {
          builder.replaceElement(variablePsi, variable.name,
            new ConstantNode(variable.getDefaultValue(variablePsi)), true)
        } else {
          builder.replaceElement(variablePsi.getReference, variable.name,
            new ConstantNode(variable.getDefaultValue(variablePsi)), true)
        }
        variablePsi
      }
      // this guard variable allows validation on the last element by preventing the template from ending
      val endRelativeOffset = newStmt.getTextRange.getEndOffset - newStmt.getTextRange.getStartOffset
      builder.replaceRange(new TextRange(endRelativeOffset, endRelativeOffset),
        "")
      builder.setEndVariableAfter(newStmt.getLastChild)

      // must be called before building the template
      PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(editor.getDocument)

      // specifically must be an inline template (actually replace the PSI elements), otherwise the block of new code is inserted at the caret
      val template = builder.buildInlineTemplate()
      val templateListener = new TemplateListener(
        project, editor,
        tooltip, highlighters.asScala)
      val templateState = TemplateManager.getInstance(project).runTemplate(editor, template)
      templateState.addTemplateStateListener(templateListener)
      templateState
    })
  }
}
