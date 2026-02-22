package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationGroupManager}
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiManager, PsiTreeChangeEvent, PsiTreeChangeListener}
import com.intellij.util.concurrency.AppExecutorUtil
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.compiler.{Compiler, PythonInterfaceLibrary}
import edg.util.{Errorable, timeExec}
import edg.wir.Refinements
import edg_ide.util.ExceptionNotifyImplicits.ExceptErrorable
import edg_ide.util.{DesignAnalysisUtils, exceptable}
import edgir.ref.ref
import edgir.ref.ref.LibraryPath
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.util.concurrent.Callable
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

// Note: the implementation is here, but the actual service in plugin.xml is a Java class,
// because IntelliJ doesn't seem to like the Scala class.
object EdgCompilerService {
  def apply(project: Project): EdgCompilerService = {
    project.getService(classOf[EdgCompilerServiceWrapper]).asInstanceOf[EdgCompilerService]
  }

  def notificationGroup(): NotificationGroup =
    NotificationGroupManager.getInstance().getNotificationGroup("EDG IDE Notifications")
}

/** A single shared interface to Python and for running EDG compilation jobs.
  *
  * TODO: perhaps split the library service out?
  */
class EdgCompilerService(project: Project)
    extends PersistentStateComponent[EdgCompilerServiceState]
    with Disposable {
  val pyLib = new PythonInterfaceLibrary()

  // Tracks modified classes, so the appropriate library elements can be discarded on the next refresh.
  // This works in terms of ref.LibraryPath to avoid possibly outdated PSI references and needing
  // PSI read operations.
  val modifiedTypes = mutable.Set[ref.LibraryPath]()

  PsiManager
    .getInstance(project)
    .addPsiTreeChangeListener(
      new PsiTreeChangeListener {
        override def beforeChildAddition(event: PsiTreeChangeEvent): Unit = {}
        override def beforeChildRemoval(event: PsiTreeChangeEvent): Unit = {}
        override def beforeChildReplacement(event: PsiTreeChangeEvent): Unit = {}
        override def beforeChildMovement(event: PsiTreeChangeEvent): Unit = {}
        override def beforeChildrenChange(event: PsiTreeChangeEvent): Unit = {}
        override def beforePropertyChange(event: PsiTreeChangeEvent): Unit = {}

        override def childAdded(event: PsiTreeChangeEvent): Unit = childAction(event)
        override def childRemoved(event: PsiTreeChangeEvent): Unit = childAction(event)
        override def childReplaced(event: PsiTreeChangeEvent): Unit = childAction(event)
        override def childrenChanged(event: PsiTreeChangeEvent): Unit = {} // ends up as a file action
        override def childMoved(event: PsiTreeChangeEvent): Unit = childAction(event)
        override def propertyChanged(event: PsiTreeChangeEvent): Unit = {}

        def childAction(event: PsiTreeChangeEvent): Unit = {
          val containingClass = PsiTreeUtil.getParentOfType(event.getParent, classOf[PyClass])
          if (containingClass == null) {
            return
          }

          ReadAction
            .nonBlocking((() => {
              val inheritors = PyClassInheritorsSearch.search(containingClass, true).findAll().asScala
              val extendedModifiedTypes = (inheritors.toSeq :+ containingClass).map { modifiedClass =>
                DesignAnalysisUtils.typeOf(modifiedClass)
              }.toSet

              modifiedTypes.synchronized {
                val newTypes = extendedModifiedTypes -- modifiedTypes
                modifiedTypes.addAll(newTypes)
                newTypes
              }
            }): Callable[Set[ref.LibraryPath]])
            .finishOnUiThread(
              ModalityState.defaultModalityState(),
              newTypes => {
                BlockVisualizerService(project).visualizerPanelOption.foreach { visualizerPanel =>
                  visualizerPanel.addStaleTypes(newTypes.toSeq)
                }
              }
            )
            .inSmartMode(project)
            .submit(AppExecutorUtil.getAppExecutorService)
          // TODO update library cached status, so incremental discard
        }
      },
      this
    )

  /** Discards stale elements from modifiedPyClasses, returning the discarded classes.
    */
  def discardStale(): Set[LibraryPath] = {
    val copyModifiedTypes = modifiedTypes.synchronized {
      val copy = modifiedTypes.toSet
      modifiedTypes.clear()
      copy
    }

    val discarded = copyModifiedTypes.filter { modifiedType =>
      // TODO should this use compiled library analysis or PSI analysis?
      pyLib.discardCached(modifiedType)
    }
    discarded
  }

  // Rebuilds library elements, (re)indexing the module and requesting all of them from the compiler.
  // Does not discard any elements and does not rebuild cached elements
  // progressFn is called for each library requested, passing in the library, index, and total library count,
  // for all library elements regardless of whether it's cached.
  def rebuildLibraries(
      module: String,
      progressFn: Option[(ref.LibraryPath, Int, Int) => Unit] = None
  ): Errorable[(Set[ref.LibraryPath], Long, Long)] = exceptable {
    val (indexed, indexTime) = timeExec {
      pyLib.indexModule(module).mapErr(msg => s"failed to index: $msg").exceptError.toSet
    }
    val (_, rebuildTime) = timeExec {
      indexed.toSeq.zipWithIndex.foreach { case (libraryType, i) =>
        progressFn.foreach { fn => fn(libraryType, i, indexed.size) }
        EdgCompilerService(project).pyLib.getLibrary(libraryType) // run for effect, errors skipped
      }
    }
    (indexed, indexTime, rebuildTime)
  }

  // Compiles a top level design
  // progressFn is called (by compiler hook) for each compiler elaborate record
  def compile(
      designType: ref.LibraryPath,
      progressFn: Option[Float => Unit] = None
  ): (schema.Design, Compiler, edgrpc.Refinements) = {
    val (block, refinements) = EdgCompilerService(project).pyLib
      .getDesignTop(designType)
      .mapErr(msg => s"invalid top-level design: $msg")
      .get // TODO propagate Errorable
    val design = schema.Design(contents = Some(block))

    val compiler =
      new Compiler(
        design,
        EdgCompilerService(project).pyLib,
        refinements = Refinements(refinements),
        progressFn = progressFn
      )
    (compiler.compile(), compiler, refinements)
  }

  override def getState: EdgCompilerServiceState = {
    val state = new EdgCompilerServiceState
    val settings = EdgSettingsState.getInstance()
    if (settings.persistBlockCache) {
      state.serializedBlocks = pyLib.toLibraryPb.toProtoString
    }
    state
  }

  override def loadState(state: EdgCompilerServiceState): Unit = {
    val settings = EdgSettingsState.getInstance()
    if (settings.persistBlockCache) {
      val library = schema.Library.fromAscii(state.serializedBlocks)
      pyLib.loadFromLibraryPb(library)
    }
  }

  override def dispose(): Unit = {}
}
