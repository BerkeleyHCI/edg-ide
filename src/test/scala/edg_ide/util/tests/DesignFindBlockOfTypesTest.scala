package edg_ide.util.tests

import edg.ElemBuilder.{Block, Design, LibraryPath}
import edg.compiler.{Compiler, CompilerExpansionTest}
import edg.wir
import edg.wir.DesignPath
import edg_ide.util.DesignFindBlockOfTypes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DesignFindBlockOfTypesTest extends AnyFlatSpec with Matchers {
  behavior of "DesignFindBlockOfType"

  it should "find blocks by single type" in {
    val inputDesign = Design(Block.Block(
      superclass = "top",
      blocks = Map(
        "source" -> Block.Library("sourceContainerBlock"),
        "sink1" -> Block.Library("sinkContainerBlock"),
        "sink2" -> Block.Library("sinkContainerBlock"),
      )
    ))
    val compiler = new Compiler(inputDesign, new wir.EdgirLibrary(CompilerExpansionTest.library))
    val design = compiler.compile()

    new DesignFindBlockOfTypes(Set(LibraryPath("sourceBlock"))).map(design).toMap.keySet should contain(
      DesignPath() + "source" + "inner")
    new DesignFindBlockOfTypes(Set(LibraryPath("sinkBlock"))).map(design).toMap.keySet should contain allOf(
      DesignPath() + "sink1" + "inner",
      DesignPath() + "sink2" + "inner",
    )
    new DesignFindBlockOfTypes(Set(LibraryPath("sourceContainerBlock"))).map(design).toMap.keySet should contain(
      DesignPath() + "source")
    new DesignFindBlockOfTypes(Set(LibraryPath("sinkContainerBlock"))).map(design).toMap.keySet should contain allOf(
      DesignPath() + "sink1",
      DesignPath() + "sink2",
    )

    new DesignFindBlockOfTypes(Set(LibraryPath("top"))).map(design).toMap.keySet should contain(
      DesignPath(),
    )
  }

  it should "find blocks by multiple types" in {
    val inputDesign = Design(Block.Block(
      superclass = "top",
      blocks = Map(
        "source" -> Block.Library("sourceContainerBlock"),
        "sink1" -> Block.Library("sinkContainerBlock"),
        "sink2" -> Block.Library("sinkContainerBlock"),
      )
    ))
    val compiler = new Compiler(inputDesign, new wir.EdgirLibrary(CompilerExpansionTest.library))
    val design = compiler.compile()

    new DesignFindBlockOfTypes(Set(LibraryPath("sourceBlock"), LibraryPath("sinkBlock")))
        .map(design).toMap.keySet should contain allOf(
      DesignPath() + "source" + "inner",
      DesignPath() + "sink1" + "inner",
      DesignPath() + "sink2" + "inner",
    )

    new DesignFindBlockOfTypes(Set(LibraryPath("sinkContainerBlock"), LibraryPath("sourceContainerBlock")))
        .map(design).toMap.keySet should contain allOf(
      DesignPath() + "sink1",
      DesignPath() + "sink2",
      DesignPath() + "source")
  }
}
