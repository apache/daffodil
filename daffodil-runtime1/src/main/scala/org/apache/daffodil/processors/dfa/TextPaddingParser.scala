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

package edu.illinois.ncsa.daffodil.processors.dfa

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.DelimiterIterator
import edu.illinois.ncsa.daffodil.io.DataInputStream
import scala.collection.mutable.ArrayBuffer

class TextPaddingParser(val padChar: Char,
  override val context: TermRuntimeData)
  extends Parser {

  lazy val name: String = "TextPaddingParser"
  lazy val info: String = "padChar='" + padChar + "'"

  val paddingDFA = CreatePaddingDFA(padChar, context)

  def parse(input: DataInputStream, delimIter: DelimiterIterator): Maybe[ParseResult] = {

    val paddingReg: Registers = TLRegistersPool.getFromPool("TextPaddingParser1")

    paddingReg.reset(input, delimIter)

    paddingDFA.run(paddingReg) // Will always succeed.

    val paddingValue = One(paddingReg.resultString.toString)

    TLRegistersPool.returnToPool(paddingReg)
    TLRegistersPool.pool.finalCheck

    One(new ParseResult(paddingValue, Nope, ArrayBuffer()))
  }

}
