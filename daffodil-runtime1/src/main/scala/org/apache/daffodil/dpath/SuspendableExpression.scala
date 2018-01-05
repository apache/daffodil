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

package org.apache.daffodil.dpath

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.processors.Suspension
import org.apache.daffodil.util.LogLevel

/**
 * Base for unparse-time expression evaluation that can have forward reference.
 * There are only two such cases, which is dfdl:outputValueCalc, and
 * dfdl:setVariable expressions (which variables are in-turn used by
 * dfdl:outputValueCalc.
 */
trait SuspendableExpression
  extends Suspension {

  override val isReadOnly = true

  protected def expr: CompiledExpression[AnyRef]

  override def toString = "SuspendableExpression(" + rd.diagnosticDebugName + ", expr=" + expr.prettyExpr + ")"

  protected def processExpressionResult(ustate: UState, v: AnyRef): Unit

  override protected final def doTask(ustate: UState) {
    var v: Maybe[AnyRef] = Nope
    if (!isBlocked) {
      log(LogLevel.Debug, "Starting suspendable expression for %s, expr=%s", rd.diagnosticDebugName, expr.prettyExpr)
    } else {
      this.setUnblocked()
      log(LogLevel.Debug, "Retrying suspendable expression for %s, expr=%s", rd.diagnosticDebugName, expr.prettyExpr)
    }
    while (v.isEmpty && !this.isBlocked) {
      v = expr.evaluateForwardReferencing(ustate, this)
      if (v.isEmpty) {
        Assert.invariant(this.isBlocked)
        log(LogLevel.Debug, "UnparserBlocking suspendable expression for %s, expr=%s", rd.diagnosticDebugName, expr.prettyExpr)
      } else {
        Assert.invariant(this.isDone)
        Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
        log(LogLevel.Debug, "Completed suspendable expression for %s, expr=%s", rd.diagnosticDebugName, expr.prettyExpr)
        processExpressionResult(ustate, v.get)
      }
    }
  }

}

