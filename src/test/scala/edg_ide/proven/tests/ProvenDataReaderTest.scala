package edg_ide.proven.tests

import edg.ElemBuilder
import edg_ide.proven.ProvenDataReader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import scala.collection.SeqMap


class ProvenDataReaderTest extends AnyFlatSpec with Matchers {
  behavior of "ProvenDataReader"

  it should "read in data" in {
    val data = ProvenDataReader.read(new File("src/main/resources/proven-designs/data.csv"))
    data.getRecords(ElemBuilder.LibraryPath("electronics_lib.Microcontroller_Stm32f103.Stm32f103_48")).data
        .map { case (design, records) => (design._1.getName, design._2) -> records.length } shouldEqual
        SeqMap(("TofArrayTest.edg", "c5a263037c00c5585586453d10de8963fa39d0e7") -> 1,
          ("BldcDriverBoard.edg", "de2f2692524fa4f20c05d237879fca5222511790") -> 1)
  }
}
