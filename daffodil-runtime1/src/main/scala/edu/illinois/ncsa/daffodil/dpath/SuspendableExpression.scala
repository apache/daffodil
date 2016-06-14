package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.io.DirectOrBufferedDataOutputStream
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import passera.unsigned.ULong
import edu.illinois.ncsa.daffodil.util.Coroutine
import edu.illinois.ncsa.daffodil.util.CoroutineException
import edu.illinois.ncsa.daffodil.util.CoroutineException
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

object SuspendableExpression {

  def apply(diSimple: DISimple, ce: CompiledExpression[AnyRef], ustate: UState, unparserForOVCRep: Unparser,
    maybeKnownLengthInBits: MaybeULong) = {
    Assert.usage(ustate.currentInfosetNodeMaybe.isDefined)

    val original = ustate.dataOutputStream.asInstanceOf[DirectOrBufferedDataOutputStream]
    val buffered = original.addBuffered

    if (maybeKnownLengthInBits.isDefined) {
      // since we know the length of the OVC we're skipping for now,
      // that means we know the absolute position of the bits in the buffer we're creating
      // and that means alignment operations don't have to suspend waiting for this knowledge
      Assert.invariant(original.maybeAbsBitPos0b.isDefined) // direct streams always know this.

      val originalAbsBitPos0b = original.maybeAbsBitPos0b.get

      // since we require OVC elements to have a length that can be determined
      // without their value being available, we are passed this length (in bits)
      // and can use it to initialize the absolute bit pos of the buffered output stream.
      //
      // This allows us to deal with alignment regions, that is, we can determine
      // their size since we know the absolute bit position.

      buffered.setMaybeAbsBitPos0b(ULong(originalAbsBitPos0b + maybeKnownLengthInBits.get))

    }

    //
    // clone the ustate for use when evaluating the expression
    //
    // TODO: Performance - copying this whole state, just for OVC is painful.
    // Some sort of copy-on-write scheme would be better.
    //
    val cloneUState = ustate.cloneForOVC(original)

    // the main-thread will carry on using the original ustate but unparsing
    // into this buffered stream.
    ustate.dataOutputStream = buffered

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

object NoData

class SuspendableExpression private (
  val diSimple: DISimple,
  val expr: CompiledExpression[AnyRef],
  val ustate: UState,
  val unparserForOVCRep: Unparser)
  extends Coroutine[AnyRef]
  with WhereBlockedLocation { mainCoroutine =>

  override final def isMain = true // This object itself is the "main" coroutine - the original sequential thread of control.

  private def erd = diSimple.erd

  private var _isMakingProgress = true

  /**
   * False if the expression blocked at the same spot, i.e.,
   * didn't make any forward progress.
   */
  def isMakingProgress = _isMakingProgress

  private class ExpressionCoroutine extends Coroutine[AnyRef] {

    private def whereBlockedInfo = mainCoroutine

    final protected def body() {
      //println("Starting suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
      var v: Maybe[AnyRef] = Nope
      while (v.isEmpty) {
        v = expr.evaluateForwardReferencing(ustate, whereBlockedInfo)
        if (v.isEmpty) {
          Assert.invariant(whereBlockedInfo.isBlocked)
          resume(mainCoroutine, NoData) // so main thread gets control back
        }
      }
      // we got the answer
      Assert.invariant(whereBlockedInfo.isDone)
      Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
      diSimple.setDataValue(v.get)
      //
      // now we have to unparse the value.
      //
      unparserForOVCRep.unparse1(ustate, erd)
      //
      ustate.dataOutputStream.setFinished() // closes it out which will then chain forward to next buffering DOS.
      //
      //println("Ending suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
      setDone
      _isMakingProgress = true
      resumeFinal(mainCoroutine, NoData)
      // and since we fall through here, the expression thread terminates now.
      // only suspendable expressions that aren't evaluated should hang around in a list
      // or on a data structure, so the coroutine should get picked up by the GC if
      // the suspendable expression itself is.
    }

    // TODO should this be hoisted into Coroutine[S] or ??
    override final protected def run() {
      try {
        waitForResume
        body()
      } catch {
        case ie: InterruptedException => // do nothing, mainCoroutine killed us
        case th: Throwable => {
          // tell consumer we're exiting via a throw
          // but not to rethrow it necessarily.
          val ce = new CoroutineException(th)
          resumeFinal(mainCoroutine, ce)
        }
      }
    }
  }

  private lazy val expressionCoroutine = new ExpressionCoroutine

  override final protected def run() {
    resume(expressionCoroutine, NoData)
    if (!isDone) {

      Assert.invariant(this.isBlocked)
      if (this.isBlockedSameLocation) {
        //println("Blocking (No Fwd Progress) suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
        _isMakingProgress = false
      } else if (this.isBlockedFirstTime) {
        //println("Blocking (First Time) suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
        _isMakingProgress = true
      } else {
        //println("Blocking (New Location) suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
        _isMakingProgress = true
      }
    } else {
      //println("Done: Suspendable expression for " + erd.name + ", expr=" + erd.outputValueCalcExpr.get.prettyExpr)
    }
  }

  /**
   * After calling this, call isDone and if that's false call isMakingProgress to
   * understand whether it is done, blocked on the exactly same situation, or blocked elsewhere.
   *
   * This status is needed to implement circular deadlock detection
   */
  final def evaluate() = run()

}
