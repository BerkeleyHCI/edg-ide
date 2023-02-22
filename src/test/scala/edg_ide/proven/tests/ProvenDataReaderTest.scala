package edg_ide.proven.tests

import edg.EdgirUtils.SimpleLibraryPath
import edg.{ElemBuilder, ExprBuilder}
import edg_ide.proven.ProvenDataReader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File


class ProvenDataReaderTest extends AnyFlatSpec with Matchers {
  behavior of "ProvenDataReader"

  it should "read in data" in {
    val data = ProvenDataReader.read(new File("src/main/resources/proven-designs/data.csv"))
    println(data.getRecords(ElemBuilder.LibraryPath("electronics_lib.Microcontroller_Stm32f103.Stm32f103_48")))

    println(data.data.toSeq.sortBy(_._2.length).map(pair => f"${pair._1.toFullString} (${pair._2.length})"))
    println(data.getByDesign(ElemBuilder.LibraryPath("electronics_lib.JlcCapacitor.JlcCapacitor")).mapValues(_.length).toSeq)
  }


}
