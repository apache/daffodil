package edu.illinois.ncsa.daffodil.debugger

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.Parser

abstract class Debugger {
  def init(parser: Parser) {}
  def before(state: PState, parser: Parser) {}
  def after(before: PState, after: PState, parser: Parser) {}
  def beforeRepetition(state: PState, parser: Parser) {}
  def afterRepetition(before: PState, after: PState, parser: Parser) {}
}

object Debugger {
  
  private var debugger: Debugger = new InteractiveDebugger()

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
    areDebugging = flag
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
}
