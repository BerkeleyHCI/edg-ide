package edg_ide.proven

import de.siegmar.fastcsv.reader.CsvReader
import edg.wir.DesignPath
import edgir.ref.ref

import java.io.{File, FileReader}
import scala.collection.{SeqMap, mutable}


object ProvenStatus extends Enumeration {
  type Status = Value

  val untested = Value(0, "untested")
  val broken = Value(1, "broken")
  val fixed = Value(2, "fixed")
  val working = Value(3, "working")

  def toEnum(s: String): Option[Status] = {
    values.find(_.toString == s)
  }
}


sealed trait ProvenRecord


class UserProvenRecord(status: ProvenStatus.Status, note: Option[String]) extends ProvenRecord {

}


class InnerProvenRecord(parent: ProvenRecord) extends ProvenRecord


class UntestedRecord() extends ProvenRecord


object ProvenDataReader {
  def read(file: File): ProvenDatabase = {
    val containingDir = file.getParentFile
    val reader = CsvReader.builder().build(new FileReader(file))
    reader.stream().forEach { row =>
      println(row)
    }
    ???
  }

}
class ProvenDatabase {
  private val data: mutable.Map[ref.LibraryPath, mutable.SeqMap[(String, String, ProvenRecord),
      mutable.ArrayBuffer[DesignPath]]] = mutable.Map()

}
