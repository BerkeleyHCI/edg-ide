package edg_ide.psi_edits

import com.intellij.codeInsight.highlighting.HighlightManager
import com.intellij.codeInsight.template.impl.{ConstantNode, TemplateState}
import com.intellij.codeInsight.template.{Template, TemplateBuilderImpl, TemplateEditingAdapter, TemplateManager}
import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.colors.EditorColors
import com.intellij.openapi.editor.markup.RangeHighlighter
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor}
import com.intellij.openapi.ui.popup.JBPopup
import com.intellij.openapi.ui.{ComponentValidator, ValidationInfo}
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.intellij.util.ui.UIUtil
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.ExceptNotify
import edg_ide.util.exceptable

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait InsertionLiveTemplateVariable {
  def name: String

  // returns this variable's segment
  def elt: PsiElement

  // whether to take the reference of the extracted variable's element, or use the full element
  def isReference: Boolean = false

  // validates the current segment, returning None for no errors, or Some(msg) if there is an error
  // and preventing further progress
  def validate(contents: String, templateState: TemplateState): Option[String] = None

  // sets the default text value for this variable
  def getDefaultValue: String
}

object InsertionLiveTemplate {
  class Variable(
      val name: String,
      val elt: PsiElement,
      validator: (String, TemplateState) => Option[String] = (_, _) => None,
      defaultValue: Option[String] = None
  ) extends InsertionLiveTemplateVariable {
    override def validate(contents: String, templateState: TemplateState): Option[String] =
      validator(contents, templateState)
    override def getDefaultValue: String = defaultValue.getOrElse(elt.getText)
  }

  class Reference(
      val name: String,
      val elt: PsiElement,
      validator: (String, TemplateState) => Option[String] = (_, _) => None,
      defaultValue: Option[String] = None
  ) extends InsertionLiveTemplateVariable {
    override def validate(contents: String, templateState: TemplateState): Option[String] =
      validator(contents, templateState)
    override def isReference: Boolean = true
    override def getDefaultValue: String = defaultValue.getOrElse(elt.getReference.getCanonicalText)
  }

  // utility validator for Python names, that also checks for name collisions (if class passed in; ignoring the
  // current template being edited)
  def validatePythonName(
      name: String,
      templateState: TemplateState,
      pyClass: Option[PyClass]
  ): Option[String] = {
    val existingNames = pyClass match {
      case Some(pyClass) =>
        val templateRange = new TextRange(
          templateState.getExpressionContextForSegment(0).getTemplateStartOffset,
          templateState.getExpressionContextForSegment(0).getTemplateEndOffset
        )
        pyClass.getInstanceAttributes.asScala
          .filter(psi => !templateRange.contains(psi.getTextRange))
          .map(_.getName)
          .toSet
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
  * @param elt
  *   existing PsiElement to instantiate the template around
  * @param variables
  *   list of variables for the live template, see variable definition
  */
class InsertionLiveTemplate(elt: PsiElement, variables: IndexedSeq[InsertionLiveTemplateVariable]) {
  private class TemplateListener(editor: Editor, tooltip: JBPopup, highlighters: Iterable[RangeHighlighter])
      extends TemplateEditingAdapter {
    private var currentTooltip = tooltip

    override def templateFinished(template: Template, brokenOff: Boolean): Unit = {
      // this is called when:
      // - tab/enter-cycle through last element (brokenOff=false)
      // - escape (brokenOff=true)
      // this does NOT get called when making an edit outside the template (instead, templateCancelled is called)
      super.templateFinished(template, brokenOff)
      templateEnded(template)
    }

    private var lastChangeWasRevert = false
    override def currentVariableChanged(
        templateState: TemplateState,
        template: Template,
        oldIndex: Int,
        newIndex: Int
    ): Unit = {
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
          case Some(err) => // TODO: this does nothing when the template is finishing
            lastChangeWasRevert = true // avoid showing popup when this is called again from previousTab()
            variableReverted = true
            if (newIndex > oldIndex) {
              templateState.previousTab() // must be before the tooltip, so the tooltip is placed correctly
            } else {
              templateState.nextTab()
            }
            currentTooltip = createTemplateTooltip(f"${oldVariable.name} | $err", editor, true)
          case None => // ignored
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
        HighlightManager.getInstance(editor.getProject).removeSegmentHighlighter(editor, highlighter)
      }
    }
  }

  private def createTemplateTooltip(message: String, editor: Editor, isError: Boolean = false): JBPopup = {
    var validationInfo = new ValidationInfo(message, null)
    if (!isError) {
      validationInfo = validationInfo.asWarning()
    }
    val popupBuilder = ComponentValidator
      .createPopupBuilder(
        validationInfo,
        _ => ()
      )
      .setCancelKeyEnabled(false) // otherwise this eats the cancel keypress for the live template
    val popup = popupBuilder.createPopup()
    popup.showInBestPositionFor(editor)
    popup
  }

  // starts this template and returns the TemplateState if successfully started
  // must run in a write command action
  def run(
      initialTooltip: Option[String] = None,
      overrideTemplateVarValues: Option[Seq[String]] = None
  ): Errorable[TemplateState] = exceptable {
    val project = elt.getProject
    val fileDescriptor =
      new OpenFileDescriptor(project, elt.getContainingFile.getVirtualFile, elt.getTextRange.getStartOffset)
    val editor = FileEditorManager.getInstance(project).openTextEditor(fileDescriptor, true)

    TemplateManager.getInstance(project).getActiveTemplate(editor).exceptEquals(null, "another template active")

    // opens / sets the focus onto the relevant text editor, so the user can start typing
    editor.getCaretModel.moveToOffset(
      elt.getTextOffset
    ) // needed so the template is placed at the right location

    // these must be constructed before template creation, other template creation messes up the locations
    val highlighters = new java.util.ArrayList[RangeHighlighter]()
    // flags = 0 means ignore esc, otherwise it eats the esc keypress
    HighlightManager
      .getInstance(project)
      .addOccurrenceHighlight(
        editor,
        elt.getTextRange.getStartOffset,
        elt.getTextRange.getEndOffset,
        EditorColors.LIVE_TEMPLATE_INACTIVE_SEGMENT,
        0,
        highlighters
      )

    val builder = new TemplateBuilderImpl(elt)
    variables.zipWithIndex.foreach { case (variable, variableIndex) =>
      val variableValue = overrideTemplateVarValues match {
        case Some(overrideTemplateVarValues) if overrideTemplateVarValues.length > variableIndex =>
          overrideTemplateVarValues(variableIndex)
        case _ => variable.getDefaultValue
      }
      val variableExpr = new ConstantNode(variableValue)
      if (!variable.isReference) {
        builder.replaceElement(variable.elt, variable.name, variableExpr, true)
      } else {
        builder.replaceElement(variable.elt.getReference, variable.name, variableExpr, true)
      }
    }
    // this guard variable allows validation on the last element by preventing the template from ending
    val endRelativeOffset = elt.getTextRange.getEndOffset - elt.getTextRange.getStartOffset
    builder.replaceRange(new TextRange(endRelativeOffset, endRelativeOffset), "")
    builder.setEndVariableAfter(elt.getLastChild)

    // must be called before building the template
    PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(editor.getDocument)

    // specifically must be an inline template (actually replace the PSI elements), otherwise the block of new code is inserted at the caret
    val template = builder.buildInlineTemplate()
    val templateState = TemplateManager.getInstance(project).runTemplate(editor, template)

    // if the editor just started, it isn't marked as showing and the tooltip creation crashes
    // TODO the positioning is still off, but at least it doesn't crash
    UIUtil.markAsShowing(editor.getContentComponent, true)
    val firstVariableName = variables.headOption.map(_.name).getOrElse("")
    val tooltipString = initialTooltip match {
      case Some(initialTooltip) => f"$firstVariableName | $initialTooltip"
      case None => firstVariableName
    }
    val tooltip = createTemplateTooltip(tooltipString, editor)

    // note, waitingForInput won't get called since the listener seems to be attached afterwards
    val templateListener = new TemplateListener(editor, tooltip, highlighters.asScala)
    templateState.addTemplateStateListener(templateListener)
    templateState
  }
}
