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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.processors.PrimParser
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.DelimParseFailure
import edu.illinois.ncsa.daffodil.processors.DelimParseSuccess
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.EncodingInfo

class StringPatternMatchedParser(pattern: String,
  erd: ElementRuntimeData,
  override val encodingInfo: EncodingInfo,
  override val justificationTrim: TextJustificationType.Type,
  override val padChar: String)
  extends PrimParser(erd) with TextReader with HasPadding {

  // The pattern will always be defined

  lazy val dp = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(erd, encodingInfo)
    }
  }

  // TODO: Add parameter for changing CharBuffer size

  val eName = erd.name

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3))
    log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at bit position: %s", eName, start.bitPos)

    // some encodings aren't whole bytes.
    // if (start.bitPos % 8 != 0) { return PE(start, "StringPatternMatched - not byte aligned.") }

    val bytePos = (start.bitPos >> 3).toInt

    log(LogLevel.Debug, "Retrieving reader")

    val reader = getReader(dcharset.charset, start.bitPos, start)

    val result = dp.get.parseInputPatterned(pattern, reader, start)

    val postState = result match {
      case _: DelimParseFailure => {
        // A no match means zero length.  
        // Because we check for Nil first, this is valid and allowed.
        // Since it's zero length, the start state is the end state. 
        start.simpleElement.setDataValue("") // empty string is the value.
        start
      }
      case s: DelimParseSuccess => {
        val endBitPos = start.bitPos + s.numBits
        log(LogLevel.Debug, "StringPatternMatched - Parsed: %s", s.field)
        log(LogLevel.Debug, "StringPatternMatched - Ended at bit position %s", endBitPos)

        val endCharPos = if (start.charPos == -1) s.field.length() else start.charPos + s.field.length()
        val field = trimByJustification(s.field)
        start.simpleElement.setDataValue(field)
        start.withPos(endBitPos, endCharPos, One(s.next))
      }

    }
    postState
  }
}