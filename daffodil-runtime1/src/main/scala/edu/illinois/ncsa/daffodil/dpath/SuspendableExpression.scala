package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Suspension
import edu.illinois.ncsa.daffodil.processors.TaskCoroutine
import edu.illinois.ncsa.daffodil.processors.SuspensionFactory

object SuspendableExpression extends SuspensionFactory {

  def apply(diSimple: DISimple, ce: CompiledExpression[AnyRef], ustate: UState, unparserForOVCRep: Unparser,
    maybeKnownLengthInBits: MaybeULong) {

    val cloneUState = setup(ustate, maybeKnownLengthInBits)

    val se = new SuspendableExpression(diSimple, ce, cloneUState, unparserForOVCRep)
    ustate.addSuspension(se)
  }

  // Pooling of these objects shut off while debugging the non-pooling version.
  //
  // This might get put back if we decide to pool these objects.
  //
  //  private val pool = new Pool[SuspendableExpression] {
  //    def allocate = new SuspendableExpression
  //  }
  //  def get() = pool.getFromPool
  //  def put(se: SuspendableExpression) {
  //    se.reset()
  //    pool.returnToPool(se)
  //  }
  //
}

class SuspendableExpression private (
  val diSimple: DISimple,
  val expr: CompiledExpression[AnyRef],
  override val ustate: UState,
  val unparserForOVCRep: Unparser)
    extends Suspension(ustate) {

  final override def rd = diSimple.erd

  protected class ExpressionCoroutine extends TaskCoroutine(ustate, mainCoroutine) {

    override final protected def body() {
      //println("Starting suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
      var v: Maybe[AnyRef] = Nope
      while (v.isEmpty) {
        v = expr.evaluateForwardReferencing(ustate, this)
        if (v.isEmpty) {
          Assert.invariant(this.isBlocked)
          resume(mainCoroutine, Suspension.NoData) // so main thread gets control back
        }
      }
      // we got the answer
      Assert.invariant(this.isDone)
      Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
      diSimple.setDataValue(v.get)
      //
      // now we have to unparse the value.
      //
      unparserForOVCRep.unparse1(ustate, rd)
    }

  }

  override final protected lazy val taskCoroutine = new ExpressionCoroutine

}
