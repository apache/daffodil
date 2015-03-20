package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.DISimple

class ElementOutputValueCalcUnparser(context: ElementRuntimeData, expr: CompiledExpression)
  extends Unparser(context) {

  override lazy val childProcessors = Nil
  /**
   * For outputValueCalc, we defer evaluating them, and change into a mode where
   * we just pull in the infoset until we have enough infoset to evaluate the
   * expression.
   */
  def unparse(ustate: UState): Unit = {
    Assert.invariant(ustate.mode == UnparseMode)

    ustate.mode = AccumulateNodesMode
    val currentSimple = ustate.currentInfosetNode.get.asSimple
    ustate.addDeferredElement(currentSimple)

    pullInfosetEventsUntilAbleToProceed(ustate)
    ustate.mode = UnparseMode

    //
    // If we get here, then we're ready to evaluate the expression
    // 
    val value = expr.evaluate(ustate)
    currentSimple.setDataValue(value)
  }

  /**
   * Consumes infoset nodes - must deal with defaulting of
   * required defaultable elements.
   */
  def pullInfosetEventsUntilAbleToProceed(ustate: UState): Unit = {
    while (ustate.hasNext) {
      val nextEvent = ustate.next()
      val isNode = nextEvent.node
      handleDefaultable(isNode, ustate)
    }
  }

  def handleDefaultable(isNode: DINode, ustate: UState): Unit = {
    isNode match {
      case e: DISimple if e.erd.isDefaultable => Assert.notYetImplemented("Unparser defaulting")
      case _ => // ok
    }
  }

}