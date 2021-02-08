package edg_ide.actions

import com.intellij.openapi.vfs.VirtualFile
import scala.collection.mutable


/** Util for working with Python modules from VirtualFiles
  */
object ModuleUtil {
  def from(base: VirtualFile, file: VirtualFile): Option[Seq[String]] = {
    val pathBuilder = mutable.ListBuffer(file.getNameWithoutExtension)
    var fileDir = file.getParent
    while (fileDir.exists()) {
      if (fileDir == base) {
        return Some(pathBuilder.toSeq.reverse)
      }
      pathBuilder += fileDir.getName
      fileDir = fileDir.getParent
    }
    None
  }
}
