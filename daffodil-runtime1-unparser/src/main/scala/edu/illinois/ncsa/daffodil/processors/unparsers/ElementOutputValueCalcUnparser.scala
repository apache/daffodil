package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dpath.SuspendableExpression

class ElementOutputValueCalcUnparser(erd: ElementRuntimeData, repUnparser: Unparser)
  extends Unparser(erd) with TextUnparserRuntimeMixin {

  Assert.invariant(erd.outputValueCalcExpr.isDefined)
  val expr = erd.outputValueCalcExpr.get

  override lazy val childProcessors = Seq(repUnparser)

  def unparse(ustate: UState): Unit = {
    Assert.invariant(erd.outputValueCalcExpr.isDefined)

    val diSimple = ustate.currentInfosetNode.asSimple 
    // note. This got attached to infoset in StatementElementOutputValueCalcUnparser

    val se = SuspendableExpression(diSimple, expr, ustate, repUnparser)

    ustate.addSuspendedExpression(se)

  }

}
