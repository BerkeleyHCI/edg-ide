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


object ProvenDataReader {
  def read(file: File): ProvenDatabase = {
    CsvReader.builder().build(new FileReader(file))
  }

}
class ProvenDatabase {
  private val data: mutable.Map[ref.LibraryPath, mutable.SeqMap[(String, String, ProvenStatus.Status),
      mutable.ArrayBuffer[DesignPath]]] = mutable.Map()

}
