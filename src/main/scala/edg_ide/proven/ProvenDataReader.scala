package edg_ide.proven

import de.siegmar.fastcsv.reader.CsvReader
import edg.wir.DesignPath
import edgir.ref.ref

import java.io.{File, FileReader}
import scala.collection.SeqMap


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
  def read(file: File): Map[ref.LibraryPath, SeqMap[(String, String, ProvenStatus.Status), Seq[DesignPath]]] = {
    CsvReader.builder().build(new FileReader(file))
  }

}
class ProvenDataReader {

}
