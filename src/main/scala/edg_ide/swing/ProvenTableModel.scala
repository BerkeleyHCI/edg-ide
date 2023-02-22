package edg_ide.swing

import edg.EdgirUtils.SimpleLibraryPath
import edgir.ref.ref
import edg_ide.proven.{BlockProvenRecords, ProvenStatus, UserProvenRecord}

// Data holder for a Block's proven status
sealed trait ProvenNodeBase

object EmptyProven extends ProvenNodeBase {
  override def toString = ""
}

class BlockProven(path: ref.LibraryPath,
                  val records: BlockProvenRecords) extends ProvenNodeBase {
  override lazy val toString: String = if (records.isEmpty) {
    ""
  } else {
    records.getDataOfStatus(records.getLatestStatus).flatMap { case (design, records) => records }.size.toString
  }

  lazy val htmlDescription = {
    val dataFormatted = records.data.map { case ((file, version), records) =>
      val statusCountString = records.groupBy(_._1.status).map { case (status, records) =>
        val statusColor = SwingHtmlUtil.colorToHtml(ProvenStatus.colorOf(status))
        s"""<font style="color:$statusColor;">$status (${records.size})</font>"""
      }.mkString(", ")

      val comments = records.map(_._1).collect {
        case record: UserProvenRecord => record.comments
      }.flatten.map { comment =>
        s"- $comment"
      }.mkString("<br/>")
      val commentsString = if (comments.nonEmpty) s"<br/>$comments" else ""

      s"""<b>${file.getName}</b>@${version.substring(0, 7)}: $statusCountString$commentsString"""
    }.toSeq.reverse  // most recent first

    f"""<b>${path.toSimpleString}</b><hr>
        ${dataFormatted.mkString("<br/>")}
        """
  }
}
