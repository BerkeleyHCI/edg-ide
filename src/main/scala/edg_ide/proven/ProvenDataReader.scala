package edg_ide.proven

import de.siegmar.fastcsv.reader.NamedCsvReader
import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.DesignPath
import edgir.ref.ref
import edgir.schema.schema
import edgir.elem.elem
import org.eclipse.emf.ecore.xmi.IllegalValueException

import java.io.{File, FileInputStream, FileReader}
import scala.collection.{SeqMap, mutable}
import scala.util.Using
import scala.util.matching.Regex


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
  def file: File
  def version: String
  def status: ProvenStatus.Status
}


class UserProvenRecord(val status: ProvenStatus.Status, val file: File, val version: String, pattern: Seq[String],
                       val comments: Option[String]) extends ProvenRecord {
  override def toString = f"${this.getClass.getSimpleName}($status, $comments from $file:${pattern.mkString(".")})"
}


class InnerProvenRecord(parent: ProvenRecord) extends ProvenRecord {
  override def toString = f"${this.getClass.getSimpleName}($parent)"

  override def file = parent.file
  override def version = parent.version
  override def status = parent.status
}


class UntestedRecord(val file: File, val version: String) extends ProvenRecord {
  override def toString = f"${this.getClass.getSimpleName}(from $file)"
  override def status = ProvenStatus.untested
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

    // map is (file, version) -> (design path, proven record)
    // design path can include * wildcard
    val designToRecord = mutable.SeqMap[(File, String), mutable.ArrayBuffer[(Seq[Regex], ProvenRecord)]]()

    val reader = NamedCsvReader.builder().build(new FileReader(file))
    var lastFileVersion: Option[(File, String)] = None
    reader.stream().forEach { row =>
      if (row.getField(kFieldFile).nonEmpty || row.getField(kFieldVersion).nonEmpty) {
        require(row.getField(kFieldFile).nonEmpty && row.getField(kFieldVersion).nonEmpty,
          "both file and field must be nonempty")
        lastFileVersion = Some((new File(containingDir, row.getField(kFieldFile)), row.getField(kFieldVersion)))
      }
      val (file, version) = lastFileVersion.get
      val path = row.getField(kFieldPath).split('.')
      val pathRegex = path.map { pathComponent =>
        pathComponent.replace("*", ".*").r
      }
      val comments = row.getField(kFieldComments) match {
        case "" => None
        case comments => Some(comments)
      }

      val status = row.getField(kFieldStatus) match {
        case "working" =>
          new UserProvenRecord(ProvenStatus.working, file, version, path, comments)
        case "broken" if row.getField(kFixedVersion).nonEmpty =>
          new UserProvenRecord(ProvenStatus.fixed, file, version, path, comments)
        case "broken" =>
          new UserProvenRecord(ProvenStatus.broken, file, version, path, comments)
      }
      designToRecord.getOrElseUpdate((file, version), mutable.ArrayBuffer()).append(
        (pathRegex, status))
    }

    val dataBuilder = mutable.Map[ref.LibraryPath, mutable.ArrayBuffer[ProvenRecord]]()

    // processes a block, with the potentially matching records from a higher level
    // for record keys: an empty list means apply this record, a nonempty list means apply it to a child
    // proven propagation rules (eg, working applies to all children, but broken doesn't) are applied here
    // untested is applied by default if no record matches
    // multiple record matches are an error
    def processBlock(file: File, version: String, path: DesignPath, block: elem.HierarchyBlock,
                     records: SeqMap[Seq[Regex], ProvenRecord]): Unit = {
      val thisRecords = records.filter { case (recordPath, _) => recordPath.isEmpty }
      if (thisRecords.nonEmpty) {
        require(thisRecords.size == 1, f"multiple proven records at $file $path")
        val thisRecordProven = thisRecords.head._2
        dataBuilder.getOrElseUpdate(block.getSelfClass, mutable.ArrayBuffer()).append(thisRecordProven)
      } else {
        val thisUntestedRecord = new UntestedRecord(file, version)
        dataBuilder.getOrElseUpdate(block.getSelfClass, mutable.ArrayBuffer()).append(thisUntestedRecord)
      }

      block.blocks.foreach { case subBlockPair if subBlockPair.getValue.`type`.isHierarchy =>
        val subBlockMap = records.flatMap { case (recordPath, proven) => recordPath match {
          case Seq() if proven.status == ProvenStatus.working => Some(Seq(), proven)  // propagate working
          case Seq() => None  // don't propagate other records
          case Seq(pathHead, pathTail @ _*) if pathHead.matches(subBlockPair.name) => Some(pathTail -> proven)
          case Seq(pathHead, pathTail @ _*) => None  // non-matching, discard
        } }
        processBlock(file, version, path + subBlockPair.name, subBlockPair.getValue.getHierarchy, subBlockMap)
      }
    }

    designToRecord.foreach { case ((file, version), records) =>
      val design = Using(new FileInputStream(file)) { fileInputStream =>
        schema.Design.parseFrom(fileInputStream)
      }.get
      processBlock(file, version, DesignPath(), design.getContents, SeqMap.from(records.map { case (path, record) =>
        path -> record
      }))
    }

    new ProvenDatabase(dataBuilder.map { case (libraryPath, records) => libraryPath -> records.toSeq}.toMap)
  }

}
class ProvenDatabase(val data: Map[ref.LibraryPath, Seq[ProvenRecord]]) {

}