package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dpath.SuspendableExpression

class ElementOutputValueCalcUnparser(erd: ElementRuntimeData, repUnparser: Unparser)
  extends UnparserObject(erd) with TextUnparserRuntimeMixin {

  Assert.invariant(erd.outputValueCalcExpr.isDefined)
  val expr = erd.outputValueCalcExpr.get

  override lazy val childProcessors = Seq(repUnparser)

  def unparse(ustate: UState): Unit = {
    Assert.invariant(erd.outputValueCalcExpr.isDefined)

    val diSimple = ustate.currentInfosetNode.asSimple
    // note. This got attached to infoset in StatementElementOutputValueCalcUnparser

    //
    // Forces the evaluation of runtime-valued things, and this will cause those
    // that actually are runtime-expressions to be cached on the infoset element.
    //
    // Then later when the actual unparse occurs, these will be accessed off the
    // infoset element's cache.
    //
    // Unparser.initialize(repUnparser) // happens centrally now

    val se = SuspendableExpression(diSimple, expr, ustate, repUnparser)

    ustate.addSuspendedExpression(se)

  }

}
