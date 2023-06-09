package edg_ide.util

import edg.util.Errorable
import edg_ide.ui.EdgSettingsState

import java.io.File
import scala.annotation.tailrec
import scala.util.control.Breaks

object KicadFootprintUtil {
  @tailrec
  private def filePathJoin(base: File, join: Seq[String]): Option[File] = {
    join match {
      case Seq() => Some(base)
      case Seq(init, tail @ _*) =>
        val nextFile = new File(base, init)
        if (nextFile.exists()) {
          filePathJoin(nextFile, tail)
        } else {
          None
        }
    }
  }

  // returns the footprint file for a given footprint name (structured as eg, Device_R:R_0603) if it can be found
  def getFootprintFile(footprintName: String): Errorable[File] = exceptable {
    val footprintPath = footprintName match {
      case s"$libraryName:$footprintName" => Seq(f"$libraryName.pretty", f"$footprintName.kicad_mod")
      case _                              => exceptable.fail(s"malformed footprint path $footprintName")
    }

    // use a for loop to return the first match, instead of mapping over everything else
    var footprintFile: Option[File] = None
    val loop = new Breaks
    loop.breakable {
      EdgSettingsState.getInstance().kicadDirectories.foreach { kicadDirectory =>
        filePathJoin(new File(kicadDirectory), footprintPath) match {
          case Some(joinedFile) => footprintFile = Some(joinedFile); loop.break()
          case None             => // continue
        }
      }
    }

    footprintFile match {
      case Some(footprintFile) => footprintFile
      case None                => exceptable.fail(s"unable to find footprint file for $footprintName")
    }
  }
}
