package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}

import collection.mutable
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{ApplicationManager, ReadAction}
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiManager, PsiTreeChangeEvent, PsiTreeChangeListener}
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.compiler.{Compiler, ElaborateRecord, PythonInterface, PythonInterfaceLibrary, hdl => edgrpc}
import edg.schema.schema
import edg.util.{Errorable, timeExec}
import edg.wir.Refinements
import edg_ide.EdgirUtils
import edg.ref.ref

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

  val pyLib = new PythonInterfaceLibrary(new PythonInterface())

  // Tracks modified classes, so the appropriate library elements can be discarded on the next refresh.
  val modifiedPyClasses = mutable.Set[PyClass]()

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
      ApplicationManager.getApplication.invokeLater(() => {
        if (containingClass != null) {
          modifiedPyClasses.synchronized {
            modifiedPyClasses += containingClass
          }
        }
      })
    }
  }, this)

  /** Discards stale elements from modifiedPyClasses
    */
  def discardStale(): Unit = {
    val copyModifiedClasses = modifiedPyClasses.synchronized {
      val copy = modifiedPyClasses.toSet
      modifiedPyClasses.clear()
      copy
    }

    // TODO use library analysis search
    val extendedModifiedClassNames = ReadAction.compute(() => {
      copyModifiedClasses.flatMap { modifiedClass =>
        val inheritors = PyClassInheritorsSearch.search(modifiedClass, true).findAll().asScala
        inheritors.toSeq :+ modifiedClass
      }.map (_.getQualifiedName)
    })

    val discarded = extendedModifiedClassNames.flatMap { modifiedClassName =>
      pyLib.discardCached(modifiedClassName)
    }

    if (discarded.isEmpty) {
      notificationGroup.createNotification("No changes to source files", NotificationType.WARNING)
          .notify(project)
    } else {
      notificationGroup.createNotification(s"Cleaned cache",
        s"discarded ${discarded.size} changed modules",
        discarded.map(EdgirUtils.SimpleLibraryPath).mkString(", "),
        NotificationType.INFORMATION)
          .notify(project)
    }
  }

  /** Loads all library elements visible from some module.
    * Returns (loaded elements, error elements)
    */
  def fillCache(module: String, indicator: Option[ProgressIndicator]): (Seq[ref.LibraryPath], Seq[ref.LibraryPath]) = {
    indicator.foreach(_.setIndeterminate(true))
    indicator.foreach(_.setText(s"EDG library compiling: indexing"))
    val allLibraries = pyLib.reloadModule(module)
    indicator.foreach(_.setIndeterminate(false))

    val loadSuccess = mutable.ListBuffer[ref.LibraryPath]()
    val loadFailure = mutable.ListBuffer[ref.LibraryPath]()

    for ((libraryPath, i) <- allLibraries.zipWithIndex) {
      indicator.foreach(_.setFraction(i.toFloat / allLibraries.size))
      indicator.foreach(_.setText(s"EDG library compiling: ${EdgirUtils.SimpleLibraryPath(libraryPath)}"))
      pyLib.getLibrary(libraryPath) match {
        case Errorable.Success(_) => loadSuccess += libraryPath
        case Errorable.Error(_) => loadFailure += libraryPath
      }
    }

    (loadSuccess.toSeq, loadFailure.toSeq)
  }

  def compile(topModule: String, designType: ref.LibraryPath,
              indicator: Option[ProgressIndicator]): (schema.Design, Compiler, edgrpc.Refinements, Long, Long) = {
    indicator.foreach(_.setText(s"EDG compiling: cleaning cache"))
    discardStale()

    indicator.foreach(_.setText(s"EDG compiling: reloading"))
    val (_, reloadTime) = timeExec {
      pyLib.reloadModule(topModule)
    }

    indicator.foreach(_.setText(s"EDG compiling: design top"))
    val ((compiled, compiler, refinements), compileTime) = timeExec {
      val (block, refinements) = EdgCompilerService(project).pyLib.getDesignTop(designType).get  // TODO propagate Errorable
      val design = schema.Design(contents = Some(block.copy(superclasses = Seq(designType))))  // TODO dedup w/ superclass resolution in BlockLink.Block

      val compiler = new Compiler(design, EdgCompilerService(project).pyLib,
        refinements=Refinements(refinements)) {
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
