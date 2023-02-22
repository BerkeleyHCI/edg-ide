package edg_ide.proven.tests

import edg.ElemBuilder
import edg_ide.proven.{ProvenDataReader, ProvenStatus}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import scala.collection.SeqMap


class ProvenDataReaderTest extends AnyFlatSpec with Matchers {
  behavior of "ProvenDataReader"

  it should "read in data" in {
    val data = ProvenDataReader.read(new File("src/main/resources/proven-designs/data.csv"))

    val stmRecord = data.getRecords(ElemBuilder.LibraryPath("electronics_lib.Microcontroller_Stm32f103.Stm32f103_48"))
    stmRecord.isEmpty shouldEqual false
    stmRecord.data.map { case (design, records) => (design._1.getName, design._2) -> records.length } shouldEqual
        SeqMap(("TofArrayTest.edg", "c5a263037c00c5585586453d10de8963fa39d0e7") -> 1,
          ("BldcDriverBoard.edg", "de2f2692524fa4f20c05d237879fca5222511790") -> 1)
    stmRecord.getLatestStatus shouldEqual ProvenStatus.working

    val esp32c = data.getRecords(ElemBuilder.LibraryPath("electronics_lib.Microcontroller_Esp32c3.Esp32c3_Wroom02"))
    esp32c.isEmpty shouldEqual false
    esp32c.getLatestStatus shouldEqual ProvenStatus.fixed

    val caps = data.getRecords(ElemBuilder.LibraryPath("electronics_lib.JlcCapacitor.JlcCapacitor"))
    caps.isEmpty shouldEqual false
    caps.getLatestStatus shouldEqual ProvenStatus.working

    val nonexistent = data.getRecords(ElemBuilder.LibraryPath("nonexistent"))
    nonexistent.isEmpty shouldEqual true
    nonexistent.getLatestStatus shouldEqual ProvenStatus.untested
  }
}
