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
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.infoset.RetryableException
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser

/**
 * SuspendableOperation is used for suspending and retrying things that aren't
 * expressions. Example is an alignmentFill unparser. Until we know the absolute
 * start bit positon, we can't lay down alignment fill bits.
 *
 * This has to be suspended and retried later, but it's not an expression
 * being evaluated that has forward references.
 */
trait SuspendableOperation
  extends Suspension {

  override def rd: TermRuntimeData

  override def toString = "%s for %s".format(Misc.getNameFromClass(this), rd.diagnosticDebugName)

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

  override protected final def doTask(ustate: UState) {
    if (isBlocked) {
      setUnblocked()
      log(LogLevel.Debug, "retrying %s", this)
    }
    while (!isDone && !isBlocked) {
      try {
        val tst = test(ustate)
        if (tst) {
          log(LogLevel.Debug, "test() of %s %s passed", this, tst)
          setDone
        } else {
          log(LogLevel.Debug, "test() of %s %s failed", this, tst)
          val nodeOpt = if (ustate.currentInfosetNodeMaybe.isDefined) ustate.currentInfosetNodeMaybe.get else "No Node"
          block(nodeOpt, ustate.dataOutputStream, 0, this)
        }
      } catch {
        case e: RetryableException => {
          log(LogLevel.Debug, "test() of %s threw %s", this, e)
          val nodeOpt = if (ustate.currentInfosetNodeMaybe.isDefined) ustate.currentInfosetNodeMaybe.get else "No Node"
          block(nodeOpt, ustate.dataOutputStream, 0, e)
        }
      }
      if (!isDone) {
        Assert.invariant(isBlocked)
      }
    }
    if (isDone) {
      log(LogLevel.Debug, "continuation() of %s", this)
      continuation(ustate)
      log(LogLevel.Debug, "continuation() of %s done!", this)

    }
  }
}

trait SuspendableUnparser
  extends Unparser {

  protected def suspendableOperation: SuspendableOperation

  final def unparse(state: UState): Unit = {
    suspendableOperation.run(state)
  }
}

class SuspendableOperationException(m: String)
  extends Diagnostic(Nope, Nope, Nope, Maybe(m)) {
  override def isError = true
  override def modeName = "Unparse"
}
