package edg.compiler
import edg.wir.DesignPath
import edg_ide.ui.ParamToUnitsStringUtil
import edgir.expr.expr
import edgir.ref.ref

object ExprVarToValue {
  def apply(item: expr.ValueExpr, compiler: Compiler, designPath: DesignPath): String = new ExprVarToValue(compiler, designPath).map(item)
}

class ExprVarToValue(compiler: Compiler, designPath: DesignPath) extends ExprToString() {
  override def mapRef(path: ref.LocalPath): String = {
    val value = ParamToUnitsStringUtil.paramToUnitsString((designPath ++ path).asIndirect, "", compiler)
    s"${(designPath ++ path).asIndirect} = ${value}"
  }
}
