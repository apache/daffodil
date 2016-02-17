package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.io.DirectOrBufferedDataOutputStream
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.equality._

object SuspendableExpression {

  def apply(diSimple: DISimple, ce: CompiledExpression[AnyRef], ustate: UState, unparserForOVCRep: Unparser) = {
    Assert.usage(ustate.currentInfosetNodeMaybe.isDefined)

    val original = ustate.dataOutputStream.asInstanceOf[DirectOrBufferedDataOutputStream]
    val buffered = original.addBuffered

    ustate.dataOutputStream = buffered

    val cloneUState = ustate.cloneForOVC(original)
    val se = new SuspendableExpression(diSimple, ce, cloneUState, unparserForOVCRep)
    se
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
  val ustate: UState,
  val unparserForOVCRep: Unparser) {

  private def erd = diSimple.erd

  private var _isDone: Boolean = false

  /**
   * True if the expression is done, and the result has been unparsed.
   */
  def isDone = _isDone

  private var _isMakingProgress = true

  /**
   * False if the expression blocked at the same spot, i.e.,
   * didn't make any forward progress.
   */
  def isMakingProgress = _isMakingProgress

  private var lastLocation = MaybeULong.Nope

  /**
   * After calling this, call isDone and if that's false isMakingProgress to
   * understand whether it is done, blocked on the exactly same situation, or blocked elsewhere.
   *
   * This status is needed to implement circular deadlock detection
   */
  def evaluate() {
    println("Starting suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
    val v = expr.evaluateForwardReferencing(ustate)
    if (v.isDefined) {
      // we got the answer
      Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
      val diSimple = ustate.currentInfosetNodeMaybe.get.asSimple
      diSimple.setDataValue(v.get)
      //
      // now we have to unparse the value.
      //
      unparserForOVCRep.unparse1(ustate, erd)
      //
      ustate.dataOutputStream.setFinished() // closes it out which will then chain forward to next buffering DOS.
      //
      println("Ending suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
      _isDone = true
      _isMakingProgress = true
      return
    }
    val ml = expr.expressionEvaluationBlockLocation
    Assert.invariant(ml.isDefined)
    if (lastLocation.isEmpty) {
      lastLocation = ml
      println("Blocking (First Time) suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
      return
    } else if (lastLocation.get =#= ml.get) {
      // we blocked at the same place we did last time
      println("Blocking (No Fwd Progress) suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)

      _isMakingProgress = false
    } else {
      println("Blocking (New Location) suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
      _isMakingProgress = true
    }
  }

}
