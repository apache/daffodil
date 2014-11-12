package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import scala.xml.NamespaceBinding

trait ExpressionCompilerBase {

  def compile(nodeInfoKind: NodeInfo.Kind, exprWithBracesMaybe: String, namespaces: NamespaceBinding,
    compileInfoWherePropertyWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean): CompiledExpression

}