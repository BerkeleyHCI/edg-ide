package edg_ide.util.tests

import edg.ElemBuilder.{Block, Design, LibraryPath}
import edg.compiler.{Compiler, CompilerExpansionTest}
import edg.wir
import edg.wir.DesignPath
import edg_ide.util.DesignFindBlockOfType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DesignFindBlockOfTypeTest extends AnyFlatSpec with Matchers {
  behavior of "DesignFindBlockOfType"

  it should "find blocks" in {
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

    new DesignFindBlockOfType(LibraryPath("sourceBlock")).map(design).toMap.keySet should contain(
      DesignPath() + "source" + "inner")
    new DesignFindBlockOfType(LibraryPath("sinkBlock")).map(design).toMap.keySet should contain allOf(
      DesignPath() + "sink1" + "inner",
      DesignPath() + "sink2" + "inner",
    )
    new DesignFindBlockOfType(LibraryPath("sourceContainerBlock")).map(design).toMap.keySet should contain(
      DesignPath() + "source")
    new DesignFindBlockOfType(LibraryPath("sinkContainerBlock")).map(design).toMap.keySet should contain allOf(
      DesignPath() + "sink1",
      DesignPath() + "sink2",
    )

    new DesignFindBlockOfType(LibraryPath("top")).map(design).toMap.keySet should contain(
      DesignPath(),
    )
  }
}
