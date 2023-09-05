package edg_ide.psi_edits

import com.intellij.codeInsight.highlighting.HighlightManager
import com.intellij.codeInsight.template.impl.{ConstantNode, TemplateState}
import com.intellij.codeInsight.template.{Template, TemplateBuilderImpl, TemplateEditingAdapter, TemplateManager}
import com.intellij.lang.LanguageNamesValidation
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.colors.EditorColors
import com.intellij.openapi.editor.markup.RangeHighlighter
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.ui.popup.JBPopup
import com.intellij.openapi.ui.{ComponentValidator, ValidationInfo}
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile}
import com.intellij.util.ui.UIUtil
import com.jetbrains.python.PythonLanguage
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.ExceptErrorable
import edg_ide.util.exceptable

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

/** Insertion live template that manages the full live template lifecycle, from AST insertion, to new features, to
  * optional deletion.
  *
  * On top of the existing vanilla live template functionality, adds these:
  *   - variable validation
  *   - tooltips
  *   - better API
  */
abstract class InsertionLiveTemplate(containingFile: PsiFile) {
  // IMPLEMENT ME
  // insert the AST for the template, storing whatever state is needed for deletion
  // this happens in an externally-scoped write command action
  // can fail (returns an error message), in which case no AST changes should happen
  // on success, returns the container PSI element (which may contain more than the inserted elements),
  // a Seq of the inserted elements (can be empty if just the container), and the variables
  protected def buildTemplateAst(editor: Editor)
      : Errorable[(PsiElement, Seq[PsiElement], Seq[InsertionLiveTemplateVariable])]

  // IMPLEMENT ME
  // deletes the AST for the template, assuming the template has started (can read state from buildTemplateAst)
  // and is still active (only variables have been modified)
  // may throw an error if called when those conditions are not met
  // this happens in (and must be called from) an externally-scoped write command action
  def deleteTemplate(): Unit

  // IMPLEMENT ME - optional
  // called when the template is completed (not broken off), to optionally do any cleaning up
  protected def cleanCompletedTemplate(state: TemplateState): Unit = {}

  // IMPLEMENT ME - optional
  // called when the template is cancelled (esc), to optionally do any cleaning up
  // NOT triggered when making an edit outside the template
  protected def cleanCanceledTemplate(state: TemplateState): Unit = {}

  private class TemplateListener(
      editor: Editor,
      variables: Seq[InsertionLiveTemplateVariable],
      tooltip: JBPopup,
      highlighters: Iterable[RangeHighlighter]
  ) extends TemplateEditingAdapter {
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

  private class CleanCompletedListener extends TemplateFinishedListener {
    override def templateFinished(state: TemplateState, brokenOff: Boolean): Unit = {
      if (!brokenOff) {
        cleanCompletedTemplate(state)
      } else {
        cleanCanceledTemplate(state)
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
  // caller should make sure another template isn't active in the same editor, otherwise weird things can happen
  // must be called from an externally-scoped writeCommandAction
  def run(helpTooltip: String, overrideVariableValues: Map[String, String]): Errorable[TemplateState] = exceptable {
    val project = containingFile.getProject

    // only open a new editor (which messes with the caret position) if needed
    val fileEditorManager = FileEditorManager.getInstance(project)
    val containingVirtualFile = containingFile.getVirtualFile
    val editor = fileEditorManager.openFile(containingVirtualFile, true)
      .collect { case editor: TextEditor => editor.getEditor }
      .head

    // if the editor just started, it isn't marked as showing and the tooltip creation crashes
    UIUtil.markAsShowing(editor.getContentComponent, true)

    val templateManager = TemplateManager.getInstance(project)
    Option(templateManager.getActiveTemplate(editor)).foreach { activeTemplate =>
      // multiple simultaneous templates does the wrong thing
      exceptable.fail("another template is already active")
    }

    val (templateContainer, templateEltsOpt, variables) = buildTemplateAst(editor).exceptError
    val templateElts = templateEltsOpt match { // guaranteed nonempty
      case Seq() => Seq(templateContainer)
      case _ => templateEltsOpt
    }

    // create the tooltip at the start of the template
    editor.getCaretModel.moveToOffset(templateElts.head.getTextRange.getStartOffset)
    val tooltipString = variables.headOption match {
      case Some(firstVariable) => f"${firstVariable.name} | $helpTooltip"
      case None => helpTooltip
    }
    val tooltip = createTemplateTooltip(tooltipString, editor)

    // move the caret to the beginning of the container, since the template is (inexplicably) caret sensitive
    val startingOffset = templateContainer.getTextRange.getStartOffset
    editor.getCaretModel.moveToOffset(startingOffset)

    // these must be constructed before template creation, other template creation messes up the locations
    val highlighters = new java.util.ArrayList[RangeHighlighter]()
    // flags = 0 means ignore esc, otherwise it eats the esc keypress
    HighlightManager
      .getInstance(project)
      .addOccurrenceHighlight(
        editor,
        templateElts.head.getTextRange.getStartOffset,
        templateElts.last.getTextRange.getEndOffset,
        EditorColors.LIVE_TEMPLATE_INACTIVE_SEGMENT,
        0,
        highlighters
      )

    val builder = new TemplateBuilderImpl(templateContainer)
    variables.foreach { variable =>
      val variableValue = overrideVariableValues.get(variable.name) match {
        case Some(overrideVariableValue) => overrideVariableValue
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
    val endRelativeOffset = templateElts.last.getTextRange.getEndOffset - startingOffset
    builder.replaceRange(new TextRange(endRelativeOffset, endRelativeOffset), "")
    builder.setEndVariableAfter(templateElts.last)

    // must be called before building the template
    PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(editor.getDocument)

    // specifically must be an inline template (actually replace the PSI elements), otherwise the block of new code is inserted at the caret
    val template = builder.buildInlineTemplate()
    val templateState = TemplateManager.getInstance(project).runTemplate(editor, template)

    // note, waitingForInput won't get called since the listener seems to be attached afterwards
    templateState.addTemplateStateListener(new TemplateListener(editor, variables, tooltip, highlighters.asScala))
    templateState.addTemplateStateListener(new CleanCompletedListener)
    templateState
  }
}
