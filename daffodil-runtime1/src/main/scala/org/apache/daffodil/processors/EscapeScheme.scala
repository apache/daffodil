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

package org.apache.daffodil.processors

import org.apache.daffodil.processors.dfa.CreateFieldDFA
import org.apache.daffodil.processors.dfa.DFADelimiter
import org.apache.daffodil.processors.dfa.CreateDelimiterDFA
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.schema.annotation.props.gen.GenerateEscape
import org.apache.daffodil.util.MaybeChar
import org.apache.daffodil.processors.parsers.DelimiterTextType

sealed abstract class EscapeSchemeParserHelper
case class EscapeSchemeCharParserHelper(val ec: Char, val eec: MaybeChar)
  extends EscapeSchemeParserHelper {

  override def toString() = "<EscapeSchemeChar escapeChar='" + ec +
    "' escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") + "'/>"
}
case class EscapeSchemeBlockParserHelper(val eec: MaybeChar,
  blockStart: String,
  blockEnd: String,
  rd: RuntimeData)
  extends EscapeSchemeParserHelper {
  // Should note there that fieldDFA (not here) is dependent on
  // the whether or not the delimiters are constant or not.
  // As a result, it cannot be generated here.

  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockStart, false)
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockEnd, false)
  val fieldEscDFA = CreateFieldDFA(blockEndDFA, eec)

  override def toString() = "<EscapeSchemeBlock escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") +
    "' blockStart='" + blockStart + "' blockEnd='" + blockEnd + "'/>"
}

sealed abstract class EscapeSchemeUnparserHelper {
  def lookingFor: Array[DFADelimiter]
}
case class EscapeSchemeCharUnparserHelper(val ec: Char, val eec: MaybeChar, extraEscChar: Seq[Char], rd: RuntimeData)
  extends EscapeSchemeUnparserHelper {

  // We need to look for the escapeCharacter and the extraEscapedCharacters
  //
  val escCharDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, ec.toString, false)
  val escEscCharDFA: Maybe[DFADelimiter] = if (eec.isDefined) One(CreateDelimiterDFA(DelimiterTextType.Other, rd, eec.toString, false)) else Nope
  val extraEscCharsDFAs: Array[DFADelimiter] = CreateDelimiterDFA(DelimiterTextType.Other, rd, extraEscChar.map(_.toString), false)

  override val lookingFor = {
    val res: Array[DFADelimiter] =
      if (escEscCharDFA.isDefined) escCharDFA +: escCharDFA +: escEscCharDFA.get +: escEscCharDFA.get +: extraEscCharsDFAs
      else escCharDFA +: escCharDFA +: extraEscCharsDFAs
    res
  }

  override def toString() = "<EscapeSchemeChar escapeChar='" + ec +
    "' escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") + "' extraEscapedChars='" + extraEscChar.mkString(" ") + "'/>"
}
case class EscapeSchemeBlockUnparserHelper(val eec: MaybeChar,
  blockStart: String,
  blockEnd: String,
  private val extraEscChar: Seq[Char],
  generateEscapeBlock: GenerateEscape,
  rd: RuntimeData)
  extends EscapeSchemeUnparserHelper {

  // We need to look for the blockEnd
  //
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockEnd, false)
  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockStart, false)
  val fieldEscDFA = CreateFieldDFA(blockEndDFA, eec)
  val extraEscCharsDFAs: Array[DFADelimiter] = CreateDelimiterDFA(DelimiterTextType.Other, rd, extraEscChar.map(_.toString), false)

  override val lookingFor = blockStartDFA +: extraEscCharsDFAs

  override def toString() = "<EscapeSchemeBlock escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") +
    "' blockStart='" + blockStart + "' blockEnd='" + blockEnd + "' generateEscapeBlock='" + generateEscapeBlock + "'/>"
}
