package edg_ide.ui

import collection.mutable
import com.intellij.openapi.Disposable
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.project.Project
import edg.compiler.{Compiler, ElaborateRecord, PythonInterface, PythonInterfaceLibrary, hdl => edgrpc}
import edg.schema.schema
import edg.util.{Errorable, timeExec}
import edg.wir.Refinements
import edg_ide.EdgirUtils
import edg.ref.ref


// Note: the implementation is here, but the actual service in plugin.xml is a Java class,
// because IntelliJ doesn't seem to like the Scala class.
object EdgCompilerService {
  def apply(project: Project): EdgCompilerService = {
    project.getService(classOf[EdgCompilerServiceWrapper]).asInstanceOf[EdgCompilerService]
  }
}


/** A single shared interface to Python and for running EDG compilation jobs.
  */
class EdgCompilerService(project: Project) extends Disposable {
  val pyLib = new PythonInterfaceLibrary(new PythonInterface())

  // Loads all library elements visible from some module.
  // Returns (loaded elements, error elements)
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

  def compile(design: schema.Design, refinements: edgrpc.Refinements,
              indicator: Option[ProgressIndicator]): (schema.Design, Compiler, Long) = {
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
    val (compiled, time) = timeExec {
      compiler.compile()
    }
    (compiled, compiler, time)
  }

  override def dispose(): Unit = { }
}
