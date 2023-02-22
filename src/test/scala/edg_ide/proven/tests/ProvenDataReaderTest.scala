package edg_ide.proven.tests

import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemBuilder
import edg_ide.proven.ProvenDataReader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File


class ProvenDataReaderTest extends AnyFlatSpec with Matchers {
  behavior of "ProvenDataReader"

  it should "read in data" in {
    val data = ProvenDataReader.read(new File("src/main/resources/proven-designs/data.csv"))
    data.getRecords(ElemBuilder.LibraryPath("electronics_lib.Microcontroller_Stm32f103.Stm32f103_48"))
        .map(record => (record._1.file.getName, record._1.version)) shouldEqual(
        Seq(("TofArrayTest.edg", "c5a263037c00c5585586453d10de8963fa39d0e7"),
          ("BldcDriverBoard.edg", "de2f2692524fa4f20c05d237879fca5222511790"))
    )

    println(data.data.toSeq.sortBy(_._2.length).map(pair => f"${pair._1.toFullString} (${pair._2.length})"))
    println(data.getByDesign(ElemBuilder.LibraryPath("electronics_lib.JlcCapacitor.JlcCapacitor")).mapValues(_.length).toSeq)
  }
}
