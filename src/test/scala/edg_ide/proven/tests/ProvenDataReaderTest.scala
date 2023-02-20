package edg_ide.proven.tests

import edg_ide.proven.ProvenDataReader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File


class ProvenDataReaderTest extends AnyFlatSpec with Matchers {
  behavior of "ProvenDataReader"

  it should "read in data" in {
    val data = ProvenDataReader.read(new File("src/main/resources/proven-designs/data.csv"))
  }


}
