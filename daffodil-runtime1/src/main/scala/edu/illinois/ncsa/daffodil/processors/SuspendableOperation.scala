package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import edu.illinois.ncsa.daffodil.processors.unparsers.UState

/**
 * Easier to use for defining things that must be able to block
 * while unparsing.
 */
trait SuspendableOperation { enclosing =>

  def rd: RuntimeData

  /**
   * Returns true if continuation can be run.
   *
   * If false, the operation will be suspended, and resumed
   * later. Once test is true, then the continuation will be run.
   */
  protected def test(ustate: UState): Boolean

  /**
   * The operation we want to do only if the test is true.
   */
  protected def continuation(ustate: UState): Unit

  private class SuspendableOp(override val ustate: UState)
      extends Suspension(ustate) {

    override def rd = enclosing.rd

    protected class Task extends TaskCoroutine(ustate, mainCoroutine) {
      override final protected def body() {
        while (!isDone) {
          try {
            val tst = test(ustate)
            if (tst)
              setDone
            else
              block(ustate.dataOutputStream, Suspension.NoData, 0, SuspendableOperationException)
          } catch {
            case ie: InfosetRetryableException =>
              block(ustate.dataOutputStream, Suspension.NoData, 0, ie)
          }
          if (!isDone) {
            Assert.invariant(isBlocked)
            resume(mainCoroutine, Suspension.NoData)
          }
        }
        Assert.invariant(isDone)
        continuation(ustate)
      }
    }

    override final protected lazy val taskCoroutine = new Task
  }

  def run(ustate: UState) {
    val tst =
      try {
        test(ustate)
      } catch {
        case ie: InfosetRetryableException =>
          false
      }
    if (tst)
      continuation(ustate) // don't bother with Task if we can avoid it
    else {
      val cloneUState = SuspensionFactory.setup(ustate, MaybeULong.Nope)
      val se = new SuspendableOp(cloneUState)
      ustate.addSuspension(se)
    }
  }
}

object SuspendableOperationException extends Exception
  with DiagnosticImplMixin with ThinThrowable
