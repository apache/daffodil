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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Suspension
import edu.illinois.ncsa.daffodil.processors.TaskCoroutine
import edu.illinois.ncsa.daffodil.processors.SuspensionFactory
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.RetryableException
import edu.illinois.ncsa.daffodil.util.LogLevel

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

trait SuspendableExpression
  extends WhereBlockedLocation { enclosing =>

  protected def expr: CompiledExpression[AnyRef]

  def rd: RuntimeData

  override def toString = "SuspendableExpression(" + rd.prettyName + ", expr=" + expr.prettyExpr + ")"

  protected def maybeKnownLengthInBits(ustate: UState): MaybeULong

  protected def processExpressionResult(ustate: UState, v: AnyRef): Unit

  protected class SuspendableExp(override val ustate: UState)
    extends Suspension(ustate) {

    override def rd = enclosing.rd

    override def toString = enclosing.toString

    protected class Task extends TaskCoroutine(ustate, mainCoroutine) {

      override final protected def doTask() {
        log(LogLevel.Debug, "Starting suspendable expression for %s, expr=%s", rd.prettyName, expr.prettyExpr)
        var v: Maybe[AnyRef] = Nope
        while (v.isEmpty) {
          v = expr.evaluateForwardReferencing(ustate, this)
          if (v.isEmpty) {
            Assert.invariant(this.isBlocked)
            log(LogLevel.Debug, "UnparserBlocking suspendable expression for %s, expr=%s", rd.prettyName, expr.prettyExpr)
            resume(mainCoroutine, Suspension.NoData) // so main thread gets control back
            log(LogLevel.Debug, "Retrying suspendable expression for %s, expr=%s", rd.prettyName, expr.prettyExpr)
          } else {
            Assert.invariant(this.isDone)
            Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
            log(LogLevel.Debug, "Completed suspendable expression for %s, expr=%s", rd.prettyName, expr.prettyExpr)
            processExpressionResult(ustate, v.get)
          }
        }
        Assert.invariant(this.isDone)
      }
    }

    override final protected lazy val taskCoroutine = new Task
  }

  def run(ustate: UState) {
    val tst =
      try {
        // don't bother with Task if we can avoid it
        Assert.invariant(ustate.dState.mode eq UnparserBlocking)
        ustate.dState.setMode(UnparserNonBlocking) // temporarily set to just test for blocking
        val result = Maybe(expr.evaluate(ustate))
        if (result.isDefined) {
          processExpressionResult(ustate, result.get)
          true
        } else false
      } catch {
        case _: RetryableException =>
          false
      } finally {
        ustate.dState.setMode(UnparserBlocking) // restore invariant.
      }
    if (tst) {
      // nothing. We're done. Don't need the task object.
    } else {
      val cloneUState = SuspensionFactory.setup(ustate, maybeKnownLengthInBits(ustate))
      val se = new SuspendableExp(cloneUState)
      ustate.addSuspension(se)
    }
  }

}

