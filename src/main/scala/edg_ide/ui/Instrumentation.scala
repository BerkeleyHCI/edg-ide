package edg_ide.ui

import com.ibm.icu.text.SimpleDateFormat
import de.siegmar.fastcsv.writer.CsvWriter

import java.io.FileWriter
import java.util.Date
import scala.jdk.CollectionConverters.SeqHasAsJava


/** Instrumentation that allows logging to a CSV, eg for user studies
  */
object Instrumentation {
  val timeFormat = new SimpleDateFormat("yyyy-MM-dd HHmm ss Z")

  var writerCsv: Option[(java.io.Writer, CsvWriter)] = None
  var startMillis: Long = 0

  private def getWriterCsv(): (java.io.Writer, CsvWriter) = {
    writerCsv match {
      case Some(writerCsv) => writerCsv
      case None =>
        val openTime = timeFormat.format(new Date())
        startMillis = System.currentTimeMillis()
        val fileName = s"EdgIdeInstrumentation $openTime"
        val writer = new FileWriter(fileName)
        val csv = CsvWriter.builder().build(writer)

        writerCsv = Some((writer, csv))
        csv.writeRow(Seq("time", "offset", "cls", "tag", "data").asJava)
        (writer, csv)
    }
  }

  def writeRow(cls: Any, tag: String, data: String): Unit = {
    val rowTime = timeFormat.format(new Date())
    val rowMillis = System.currentTimeMillis() - startMillis
    val (writer, csv) = getWriterCsv()
    csv.writeRow(Seq(cls.getClass.getSimpleName, rowTime, (rowMillis / 1000f).toString, tag, data).asJava)
    writer.flush()
  }
}
