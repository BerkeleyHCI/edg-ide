package edg_ide.swing

import com.intellij.ui.JBColor
import edg.EdgirUtils.SimpleLibraryPath
import edgir.ref.ref
import edg_ide.proven.{BlockProvenRecords, ProvenStatus, UserProvenRecord}

import java.awt.Color

// Data holder for a Block's proven status
sealed trait ProvenNodeBase

object EmptyProven extends ProvenNodeBase {
  override def toString = ""
}

class BlockProven(blockClass: ref.LibraryPath, val records: BlockProvenRecords) extends ProvenNodeBase {
  private val latestData = records.getDataOfStatus(records.latestStatus)
  private val latestRecords = latestData.flatMap { case (design, records) => records }

  override lazy val toString: String = if (records.isEmpty) {
    ""
  } else { // get all instance-level records
    latestRecords.size.toString
  }

  lazy val htmlDescription = {
    val latestStatus = records.latestStatus
    val latestStatusColor = if (ProvenStatus.colorOf(records.latestStatus) == JBColor.GREEN) {
      "#00c000"
    } else {
      SwingHtmlUtil.colorToHtml(ProvenStatus.colorOf(records.latestStatus))
    }
    val header = f"""<b>${latestRecords.size} <font style="color:$latestStatusColor;">$latestStatus</font> instances across ${latestData.size} designs:</b>"""

    val dataFormatted = records.data.map { case ((file, version), records) =>
      val statusCountString = records.groupBy(_._1.status).map { case (status, records) =>
        val statusColor = if (ProvenStatus.colorOf(status) == JBColor.GREEN) {
          "#00c000"
        } else {
          SwingHtmlUtil.colorToHtml(ProvenStatus.colorOf(status))
        }
        s"""<font style="color:$statusColor;">$status (${records.size})</font>"""
      }.mkString(", ")

      val comments = records.map(_._1).collect {
        case record: UserProvenRecord => record.comments
      }.flatten.map { comment =>
        s"- - $comment"
      }.mkString("\n")
      val commentsString = if (comments.nonEmpty) s"\n$comments" else ""

      s"""- <b>${file.getName.replace(".edg", "").replace("Test", "").replace("Board", "")}</b>@${version.substring(0, 7)}: $statusCountString$commentsString"""
    }.toSeq.reverse  // most recent first

    f"""<b>${blockClass.toSimpleString}</b><hr>$header
        ${dataFormatted.mkString("\n")}
        """
  }
}
