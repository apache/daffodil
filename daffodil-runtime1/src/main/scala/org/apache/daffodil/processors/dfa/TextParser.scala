/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors.dfa

import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.DelimiterIterator
import org.apache.daffodil.io.DataInputStream

class TextParser(
  override val context: TermRuntimeData)
  extends Parser {

  lazy val name: String = "TextParser"
  lazy val info: String = "" // Nothing additional to add here

  def parse(input: DataInputStream, delimIter: DelimiterIterator, isDelimRequired: Boolean): Maybe[ParseResult] = {

    val lmt = new LongestMatchTracker()

    var m = input.markPos
    delimIter.reset()
    while (delimIter.hasNext()) {
      val d = delimIter.next()
      val reg = TLRegistersPool.getFromPool("TextParser1")
      reg.reset(input, delimIter, m)
      m = input.markPos
      d.run(reg)
      if (reg.status == StateKind.Succeeded) {
        lmt.successfulMatch(reg.matchStartPos, reg.delimString, d, delimIter.currentIndex)
      }
      TLRegistersPool.returnToPool(reg)
    }
    input.resetPos(m)

    val result = {
      if (lmt.longestMatches.isEmpty) {
        if (isDelimRequired) Nope
        else {
          val totalNumCharsRead = 0
          input.getString(totalNumCharsRead)
          One(new ParseResult(Nope, Nope, lmt.longestMatches))
        }
      } else {
        val delim: Maybe[String] = {
          One(lmt.longestMatchedString.toString)
        }
        One(new ParseResult(Nope, delim, lmt.longestMatches))
      }
    }

    TLRegistersPool.pool.finalCheck

    result
  }
}
