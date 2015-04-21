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

import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

class TextParser(
  override val context: TermRuntimeData)
  extends DelimitedParser {

  lazy val name: String = "TextParser"
  lazy val info: String = "" // Nothing additional to add here

  def parse(input: DFDLCharReader, delims: Seq[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {
    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val initialCharPos = input.characterPos

    delims.foreach(d => {
      val reg = new Registers(delims)
      reg.reset(input, 0)

      val dfaStatus = d.run(0, reg)
      dfaStatus.status match {
        case StateKind.Failed => // Continue
        case StateKind.Succeeded => successes += (d -> reg)
        case _ => // Continue
      }
    })

    val lm = longestMatch(successes)
    val result = {
      if (!lm.isDefined) {
        if (isDelimRequired) Nope
        else {
          val totalNumCharsRead = 0
          val numBits: Int = 0
          val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
          One(new ParseResult(Nope, Nope, "", totalNumCharsRead, numBits, nextReader))
        }
      } else {
        val (dfa, r) = lm.get
        val delim: Maybe[String] = {
          One(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        val totalNumCharsRead = r.numCharsRead
        val numBits: Int = context.encodingInfo.knownEncodingStringBitLength(r.delimString.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]

        One(new ParseResult(Nope, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }
}
