package edg_ide.ui.tools

import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.EdgirUtils
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptOption}
import edg_ide.util.{BlockConnectedAnalysis, ConnectBuilder, ConnectTypes, exceptable, requireExcept}
import edgir.elem.elem

object NewConnectTool {
  def apply(interface: ToolInterface, portPath: DesignPath): Errorable[NewConnectTool] = exceptable {
    val focusPath = interface.getFocus
    val focusBlock = EdgirUtils
      .resolveExact(focusPath, interface.getDesign)
      .exceptNone("can't reach focus block")
      .instanceOfExcept[elem.HierarchyBlock]("focus block not a block")

    val portRef = { // get selected port as Seq(...) reference
      val (containingBlockPath, containingBlock) = EdgirUtils.resolveDeepestBlock(portPath, interface.getDesign)
      val portRef = portPath.postfixFromOption(containingBlockPath).exceptNone("port not in focus block")
      val portName = portRef.steps.headOption.exceptNone("port path empty").getName

      if (containingBlockPath == focusPath) { // boundary port
        Seq(portName)
      } else { // block port
        val (blockParent, blockName) = containingBlockPath.split
        requireExcept(blockParent == focusPath, "port not in focus block")
        Seq(blockName, portName)
      }
    }

    val analysis = new BlockConnectedAnalysis(focusBlock)
    val portLink = ???
    val portConnectedConstrs = ???
    val connectBuilder = ConnectBuilder(focusBlock, portLink, portConnectedConstrs)
      .exceptNone("invalid connections to port")

    new NewConnectTool(interface, connectBuilder, analysis)
  }
}

class NewConnectTool(val interface: ToolInterface, baseConnectBuilder: ConnectBuilder, analysis: BlockConnectedAnalysis)
    extends BaseTool {}
