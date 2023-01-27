package edg.compiler

import edg.wir.DesignPath
import edgir.expr.expr
import edgir.ref.ref

object ExprVarToValue {

  def apply(item: expr.ValueExpr, compiler: Compiler, designPath: DesignPath): String = new ExprVarToValue(compiler, designPath).map(item)
}

class ExprVarToValue(compiler: Compiler, designPath: DesignPath) extends ExprToString() {

/* TODO:
  Override mapRef to show key and current values compared with constraints
 */

  override def mapRef(path: ref.LocalPath): String = {
    val value = compiler.getParamValue((designPath ++ path).asIndirect) match {
      case Some(value) => value.toStringValue
      case None => "Unsolved"
    }

    s"Value of ${(designPath ++ path).asIndirect} (${value})"
  }


}
