/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.debugger

import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.exceptions.Assert

abstract class Debugger {
  def init(parser: Parser) {}
  def before(state: PState, parser: Parser) {}
  def after(before: PState, after: PState, parser: Parser) {}
  def beforeRepetition(state: PState, parser: Parser) {}
  def afterRepetition(before: PState, after: PState, parser: Parser) {}
  def startElement(state: PState, parser: Parser) {}
  def fini(parser: Parser) {}
}

object Debugger {

  private var debugger: Debugger = null

  /**
   * For use in test cases. Just a convenience wrapper.
   */
  def withTracing(removeHidden: Boolean = true) = {
    Debugger.setDebugging(true)
    val idbg = debugger.asInstanceOf[InteractiveDebugger] // new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompiler)
    idbg.DebuggerConfig.removeHidden = removeHidden
    Debugger.setDebugger(idbg)
  }

  /**
   * Wrap things to debug with this rather than just calling setDebugging(true).
   * That way it doesn't get turned on for every subsequent test after when
   * batches of tests are being run.
   */
  def withDebugger[T](body: => T) {
    try {
      setDebugging(true)
      body
    } finally {
      setDebugging(false)
    }
  }

  private var areDebugging = false

  def setDebugger(d: Debugger) {
    debugger = d
  }

  def setDebugging(flag: Boolean) {
    if (flag)
      Assert.usage(debugger != null, "Must call setDebugger before enabling debugging with setDebugging.")
    areDebugging = flag
  }

  def getDebugging() = {
    if (areDebugging)
      Assert.usage(debugger != null, "Debugging was enabled but no debugger was set.")
    areDebugging
  }

  def init(parser: Parser) {
    if (areDebugging) { debugger.init(parser) }
  }

  def before(state: PState, parser: Parser) {
    if (areDebugging) { debugger.before(state, parser) }
  }

  def after(before: PState, after: PState, parser: Parser) {
    if (areDebugging) { debugger.after(before, after, parser) }
  }

  def beforeRepetition(state: PState, parser: Parser) {
    if (areDebugging) { debugger.beforeRepetition(state, parser) }
  }

  def afterRepetition(before: PState, after: PState, parser: Parser) {
    if (areDebugging) { debugger.afterRepetition(before, after, parser) }
  }

  def startElement(state: PState, parser: Parser) {
    if (areDebugging) { debugger.startElement(state, parser) }
  }

  def fini(parser: Parser) {
    if (areDebugging) { debugger.fini(parser) }
  }
}
