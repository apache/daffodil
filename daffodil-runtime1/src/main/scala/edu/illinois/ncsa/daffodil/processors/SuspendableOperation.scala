/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

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
