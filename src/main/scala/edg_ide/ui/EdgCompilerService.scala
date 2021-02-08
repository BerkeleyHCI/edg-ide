package edg_ide.ui

import com.intellij.openapi.Disposable
import com.intellij.openapi.project.Project
import edg.compiler.{PythonInterface, PythonInterfaceLibrary}
import edg.schema.schema
import edg.compiler.Compiler
import edg.util.timeExec


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
  def compile(design: schema.Design): (schema.Design, Compiler, Long) = {
    val compiler = new Compiler(design, EdgCompilerService(project).pyLib)
    val (compiled, time) = timeExec {
      compiler.compile()
    }
    (compiled, compiler, time)
  }

  override def dispose(): Unit = { }
}
