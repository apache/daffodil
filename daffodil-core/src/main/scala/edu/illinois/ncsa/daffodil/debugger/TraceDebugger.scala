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
import edu.illinois.ncsa.daffodil.processors.Success


class TraceDebugger() extends Debugger {
  override def before(state: PState, parser: Parser) {
    dontTrace(parser) { return }
    printState("!!!Before!!!", state, parser)
  }

  override def after(before: PState, after: PState, parser: Parser) {
    dontTrace(parser) { return }
    printStateDelta("!!!After!!!", before, after, parser)
  }

  override def beforeRepetition(state: PState, parser: Parser) {
    println("!!! BeforeIteration !!!" + parser)
  }

  override def afterRepetition(before: PState, after: PState, parser: Parser) {
    println("!!! AfterIteration !!! " + parser)
  }

  def dontTrace(parser: Parser)(body: => Unit) = {
    parser.toString match {
      case "EndSequence" => body
      case "StartSequence" => body
      case "StartChildren" => body
      case "EndChildren" => body
      case _ => // nothing
    }
  }

  def printState(ba: String, state: PState, parser: Parser) {
    println("%s %s %s".format(ba, parser.context, parser))
    println("%s position (bytes) = %d".format(ba, state.bytePos))
    if (state.bitLimit != -1) println("%s limit (bytes) = %d".format(ba, state.bitLimit / 8))
    if (state.discriminator == true) println("%s discriminator true".format(ba))
    if (state.arrayPos != -1) println("%s array index = %d".format(ba, state.arrayPos))
    if (state.groupPos != -1) println("%s group index = %d".format(ba, state.groupPos))
    if (state.childPos != -1) println("%s child index = %d".format(ba, state.childPos))
    println("%s Infoset node = '%s'".format(ba, state.infoset.toBriefXML))
    val loc = state.currentLocation
    println(loc)
  }

  def printStateDelta(ba: String, before: PState, after: PState, parser: Parser) {
    println("%s %s %s".format(ba, parser.context, parser))
    if (after.status != Success) {
      println(after.status)
      return
    }
    var hasDelta = false

    if (before.bytePos != after.bytePos) {
      hasDelta = true
      println("%s position (bytes) = %d".format(ba, after.bytePos))
    }
    if (before.bitLimit != after.bitLimit) println("%s limit (bytes) = %d".format(ba, after.bitLimit / 8))
    if (before.discriminator != after.discriminator)
      println("%s discriminator changed to %s".format(ba, after.discriminator))
    else if (after.discriminator == true)
      println("%s discriminator %s".format(ba, after.discriminator))

    if (before.arrayPos != after.arrayPos) println("%s array index = %d".format(ba, after.arrayPos))
    if (before.groupPos != after.arrayPos) println("%s group index = %d".format(ba, after.groupPos))
    if (before.childPos != after.arrayPos) println("%s child index = %d".format(ba, after.childPos))
    println("%s Infoset node = '%s'".format(ba, after.infoset.toBriefXML))
  }
}

