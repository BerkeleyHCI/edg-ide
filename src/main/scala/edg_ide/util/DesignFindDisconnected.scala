package edg_ide.util

import edg.ExprBuilder
import edg.compiler.DesignBlockMap
import edgir.elem.elem
import edgir.expr.expr
import edgir.ref.ref
import edg.wir.DesignPath

import scala.collection.SeqMap


/** For a design, returns the DesignPath of all ports that are required but not connected in the parent,
  * and the list of ports of the block that are required.
  *
  * This is a heuristic operation that relies on a specific constraint style, and will not catch
  * more complex (eg, conditional) required-connected ports
  */
object DesignFindDisconnected extends DesignBlockMap[(Seq[DesignPath], Seq[String])] {
  override def mapBlock(path: DesignPath, block: elem.HierarchyBlock,
                        blocks: SeqMap[String, (Seq[DesignPath], Seq[String])]):
      (Seq[DesignPath], Seq[String]) = {
    val myConstrExprs = block.constraints.map { case (constrName, constr) =>  // unpack constraint expression type
      constr.expr
    }.toSeq  // allow multiple uses

    val myRequiredPorts = myConstrExprs.collect {  // unpack ref steps, if a ref
      case expr.ValueExpr.Expr.Ref(path) => path.steps
    }.collect {
      case Seq(ref.LocalStep(ref.LocalStep.Step.Name(portName), _),
        ExprBuilder.Ref.IsConnectedStep) => portName
    }

    val myConnectedPorts = myConstrExprs.collect {  // extract block side expr
      case expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(Some(blockExpr), Some(linkExpr), _)) =>
        blockExpr.expr
      case expr.ValueExpr.Expr.Exported(expr.ExportedExpr(Some(exteriorExpr), Some(interiorExpr), _)) =>
        interiorExpr.expr
    } .collect {  // extract steps
      case expr.ValueExpr.Expr.Ref(path) => path.steps
    } .collect {
      case Seq(ref.LocalStep(ref.LocalStep.Step.Name(blockName), _),
        ref.LocalStep(ref.LocalStep.Step.Name(portName), _)) => (blockName, portName)
    }

    // Required ports of children, as (block, port) tuple
    val childRequiredPorts = blocks.toSeq.flatMap { case (childName, (_, childRequiredPorts)) =>
      childRequiredPorts.map { childRequiredPort =>
        (childName, childRequiredPort)
      }
    }

    val missingChildRequiredPorts = (childRequiredPorts.toSet -- myConnectedPorts.toSet)
        .map { case (blockName, portName) =>
          path + blockName + portName
        }.toSeq

    val childUnconnectedPorts = blocks.flatMap { case (childName, (childUnconnectedPorts, _)) =>
      childUnconnectedPorts
    }

    (missingChildRequiredPorts ++ childUnconnectedPorts, myRequiredPorts)
  }

  override def mapBlockLibrary(path: DesignPath, block: ref.LibraryPath): (Seq[DesignPath], Seq[String]) = {
    // shouldn't happen
    (Seq(), Seq())
  }
}
