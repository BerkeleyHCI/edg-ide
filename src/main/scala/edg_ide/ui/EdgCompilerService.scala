package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{ApplicationManager, ReadAction}
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiManager, PsiTreeChangeEvent, PsiTreeChangeListener}
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, ElaborateRecord, PythonInterfaceLibrary, hdl => edgrpc}
import edg.ref.ref
import edg.schema.schema
import edg.util.{Errorable, timeExec}
import edg.wir.Refinements
import edg_ide.util.DesignAnalysisUtils

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala


// Note: the implementation is here, but the actual service in plugin.xml is a Java class,
// because IntelliJ doesn't seem to like the Scala class.
object EdgCompilerService {
  def apply(project: Project): EdgCompilerService = {
    project.getService(classOf[EdgCompilerServiceWrapper]).asInstanceOf[EdgCompilerService]
  }
}


/** A single shared interface to Python and for running EDG compilation jobs.
  *
  * TODO: perhaps split the library service out?
  */
class EdgCompilerService(project: Project) extends
    PersistentStateComponent[EdgCompilerServiceState] with Disposable {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.ui.EdgCompilerService")

  val pyLib = new PythonInterfaceLibrary()

  // Tracks modified classes, so the appropriate library elements can be discarded on the next refresh.
  // This works in terms of ref.LibraryPath to avoid possibly outdated PSI references and needing
  // PSI read operations.
  val modifiedTypes = mutable.Set[ref.LibraryPath]()

  PsiManager.getInstance(project).addPsiTreeChangeListener(new PsiTreeChangeListener {
    override def beforeChildAddition(event: PsiTreeChangeEvent): Unit = { }
    override def beforeChildRemoval(event: PsiTreeChangeEvent): Unit = { }
    override def beforeChildReplacement(event: PsiTreeChangeEvent): Unit = { }
    override def beforeChildMovement(event: PsiTreeChangeEvent): Unit = { }
    override def beforeChildrenChange(event: PsiTreeChangeEvent): Unit = { }
    override def beforePropertyChange(event: PsiTreeChangeEvent): Unit = { }

    override def childAdded(event: PsiTreeChangeEvent): Unit = childAction(event)
    override def childRemoved(event: PsiTreeChangeEvent): Unit = childAction(event)
    override def childReplaced(event: PsiTreeChangeEvent): Unit = childAction(event)
    override def childrenChanged(event: PsiTreeChangeEvent): Unit = { }  // ends up as a file action
    override def childMoved(event: PsiTreeChangeEvent): Unit = childAction(event)
    override def propertyChanged(event: PsiTreeChangeEvent): Unit = { }

    def childAction(event: PsiTreeChangeEvent): Unit = {
      val containingClass = PsiTreeUtil.getParentOfType(event.getParent, classOf[PyClass])
      if (containingClass == null) {
        return
      }

      ApplicationManager.getApplication.invokeLater(() => {
        val extendedModifiedTypes = ReadAction.compute(() => {
          val inheritors = PyClassInheritorsSearch.search(containingClass, true).findAll().asScala
          (inheritors.toSeq :+ containingClass).map { modifiedClass =>
            DesignAnalysisUtils.typeOf(modifiedClass)
          }
        }).toSet
        val newTypes = modifiedTypes.synchronized {
          val newTypes = extendedModifiedTypes -- modifiedTypes
          modifiedTypes.addAll(newTypes)
          newTypes
        }
        BlockVisualizerService(project).visualizerPanelOption.foreach { visualizerPanel =>
          visualizerPanel.addStaleTypes(newTypes.toSeq)
        }
        // TODO update library cached status, so incremental discard
      })
    }
  }, this)

  /** Discards stale elements from modifiedPyClasses
    */
  def discardStale(): Unit = {
    val copyModifiedTypes = modifiedTypes.synchronized {
      val copy = modifiedTypes.toSet
      modifiedTypes.clear()
      copy
    }

    val discarded = copyModifiedTypes.filter { modifiedType =>
        // TODO should this use compiled library analysis or PSI analysis?
      pyLib.discardCached(modifiedType)
    }

    if (discarded.isEmpty) {
      notificationGroup.createNotification("No changes to source files", NotificationType.WARNING)
          .notify(project)
    } else {
      notificationGroup.createNotification(s"Cleaned cache",
        s"discarded ${discarded.size} changed modules",
        discarded.map(_.toSimpleString).mkString(", "),
        NotificationType.INFORMATION)
          .notify(project)
    }
  }

  /** Recompiles a design and all libraries not present in cache
    */
  def compile(topModule: String, designType: ref.LibraryPath,
              indicator: Option[ProgressIndicator]): (schema.Design, Compiler, edgrpc.Refinements, Long, Long) = {
    indicator.foreach(_.setText(s"EDG compiling: cleaning cache"))
    discardStale()

    indicator.foreach(_.setText(s"EDG compiling: reloading"))
    val (allLibraries, reloadTime) = timeExec {
      pyLib.indexModule(topModule) match {
        case Errorable.Success(indexed) => indexed
        case Errorable.Error(errMsg) =>
          notificationGroup.createNotification(
            s"Failed to reload", s"",
            s"$errMsg",
            NotificationType.WARNING)
              .notify(project)
          Seq()
      }
    }

    indicator.foreach(_.setIndeterminate(false))
    for ((libraryType, i) <- allLibraries.zipWithIndex) {
      indicator.foreach(_.setFraction(i.toFloat / allLibraries.size))
      indicator.foreach(_.setText(s"EDG compiling: library ${libraryType.toSimpleString}"))
      pyLib.getLibrary(libraryType) match {
        case Errorable.Success(_) => // ignored
        case Errorable.Error(errMsg) =>
          notificationGroup.createNotification(
            s"Failed to compile ${libraryType.toSimpleString}", s"",
            s"$errMsg",
            NotificationType.WARNING)
              .notify(project)
      }
    }

    indicator.foreach(_.setText(s"EDG compiling: design top"))
    indicator.foreach(_.setIndeterminate(true))
    val ((compiled, compiler, refinements), compileTime) = timeExec {
      val (block, refinements) = EdgCompilerService(project).pyLib.getDesignTop(designType).get // TODO propagate Errorable
      val design = schema.Design(contents = Some(block))

      val compiler = new Compiler(design, EdgCompilerService(project).pyLib,
        refinements = Refinements(refinements)) {
        override def onElaborate(record: ElaborateRecord): Unit = {
          super.onElaborate(record)
          indicator.foreach { indicator =>
            record match {
              case ElaborateRecord.Block(blockPath) =>
                indicator.setText(s"EDG compiling: block at $blockPath")
              case ElaborateRecord.Link(linkPath) =>
                indicator.setText(s"EDG compiling: link at $linkPath")
              case ElaborateRecord.Connect(toLinkPortPath, fromLinkPortPath) =>
                indicator.setText(s"EDG compiling: connect between $toLinkPortPath - $fromLinkPortPath")
              case ElaborateRecord.Generator(blockPath, fnName) =>
                indicator.setText(s"EDG compiling: generator at $blockPath:$fnName")
              case ElaborateRecord.BlockPortsConnected(blockPath) =>
                indicator.setText(s"EDG compiling: block ports connected at $blockPath")
              case elaborateRecord =>
                indicator.setText(s"EDG compiling: unknown operation $elaborateRecord")
            }
          }
        }
      }
      (compiler.compile(), compiler, refinements)
    }
    (compiled, compiler, refinements, reloadTime, compileTime)
  }

  override def getState: EdgCompilerServiceState = {
    // TODO discard stale cache?
    val state = new EdgCompilerServiceState
    state.serializedBlocks = pyLib.toLibraryPb.toProtoString
    state
  }

  override def loadState(state: EdgCompilerServiceState): Unit = {
    val library = schema.Library.fromAscii(state.serializedBlocks)
    pyLib.loadFromLibraryPb(library)
  }

  override def dispose(): Unit = { }
}
