package edg_ide.proven

import de.siegmar.fastcsv.reader.{CsvReader, NamedCsvReader}
import edg.wir.DesignPath
import edgir.ref.ref
import edgir.schema.schema
import edgir.elem.elem

import java.io.{File, FileInputStream, FileReader}
import scala.collection.{SeqMap, mutable}
import scala.util.Using


object ProvenStatus extends Enumeration {
  type Status = Value

  val untested = Value(0, "untested")
  val broken = Value(1, "broken")
  val fixed = Value(2, "fixed")  // fixed in code but not end-to-end (HDL to PCB) tested
  val working = Value(3, "working")

  def toEnum(s: String): Option[Status] = {
    values.find(_.toString == s)
  }
}


sealed trait ProvenRecord {
  def in: File
  def version: String
}


class UserProvenRecord(status: ProvenStatus.Status, val in: File, val version: String,
                       val comments: Option[String]) extends ProvenRecord {
  override def toString = f"${this.getClass.getSimpleName}($status, $comments)"
}


class InnerProvenRecord(parent: ProvenRecord) extends ProvenRecord {
  override def toString = f"${this.getClass.getSimpleName}($parent)"

  override def in = parent.in
  override def version = parent.version
}


class UntestedRecord(val in: File, val version: String) extends ProvenRecord {
  override def toString = f"${this.getClass.getSimpleName}"
}


object ProvenDataReader {
  private val kFieldFile = "file"
  private val kFieldVersion = "version"
  private val kFieldPath = "path"
  private val kFieldStatus = "status"
  private val kFieldComments = "comments"
  private val kFixedVersion = "fixed-version"

  def read(file: File): ProvenDatabase = {
    val containingDir = file.getParentFile

    // value is (design path, git version, proven record)
    // design path can include * wildcard
    val designToRecord = mutable.Map[File, mutable.ArrayBuffer[(String, String, ProvenRecord)]]()

    val reader = NamedCsvReader.builder().build(new FileReader(file))
    var lastFileVersion: Option[(File, String)] = None
    reader.stream().forEach { row =>
      if (row.getField(kFieldFile).nonEmpty || row.getField(kFieldVersion).nonEmpty) {
        require(row.getField(kFieldFile).nonEmpty && row.getField(kFieldVersion).nonEmpty,
          "both file and field must be nonempty")
        lastFileVersion = Some((new File(containingDir, row.getField(kFieldFile)), row.getField(kFieldVersion)))
      }
      val (file, version) = lastFileVersion.get

      val comments = row.getField(kFieldComments) match {
        case "" => None
        case comments => Some(comments)
      }
      val status = row.getField(kFieldStatus) match {
        case "working" => new UserProvenRecord(ProvenStatus.working, comments)
        case "broken" if row.getField(kFixedVersion).nonEmpty => new UserProvenRecord(ProvenStatus.fixed, comments)
        case "broken" => new UserProvenRecord(ProvenStatus.broken, comments)

      }
      designToRecord.getOrElseUpdate(file, mutable.ArrayBuffer()).append(
        (row.getField(kFieldPath), row.getField(kFieldVersion), status))
    }

    // processes a block, with the potentially matching records from a higher level
    // for record keys: an empty list means apply this record, a nonempty list means apply it to a child
    // proven propagation rules (eg, working applies to all children, but broken doesn't) are applied here
    // untested is applied by default if no record matches
    // multiple record matches are an error
    def processBlock(block: elem.HierarchyBlock, records: Map[Seq[String], ProvenRecord]): Unit = {

    }

    designToRecord.foreach { case (file, records) =>
      val design = Using(new FileInputStream(file)) { fileInputStream =>
        schema.Design.parseFrom(fileInputStream)
      }.get
      processBlock(design.getContents, records.map { case (path, version, record) =>

      })
    }
    println(designToRecord)
    ???
  }

}
class ProvenDatabase {
  private val data: mutable.Map[ref.LibraryPath, mutable.SeqMap[(String, String, ProvenRecord),
      mutable.ArrayBuffer[DesignPath]]] = mutable.Map()

}
