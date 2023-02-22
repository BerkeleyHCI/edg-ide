package edg_ide.swing

import edg.EdgirUtils.SimpleLibraryPath
import edgir.ref.ref
import edg_ide.proven.{BlockProvenRecords, ProvenStatus}

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
    val dataFormatted = records.data.flatMap { case ((file, version), records) =>
      records.groupBy(_._1.status).map { case (status, records) =>
        val statusColor = SwingHtmlUtil.colorToHtml(ProvenStatus.colorOf(status))
        s"""<font style="color:$statusColor;"><b>$status</b>: ${file.getName}@${version.substring(0, 7)} (${records.size})</font>"""
      }
    }

    f"""<b>${path.toSimpleString}</b><hr>
        ${dataFormatted.mkString("<br/>")}
        """
  }
}
