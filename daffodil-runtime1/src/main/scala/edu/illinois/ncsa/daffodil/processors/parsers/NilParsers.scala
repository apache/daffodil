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

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.PState
import java.nio.ByteBuffer
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.util.OnStack
import java.util.regex.Matcher
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.processors.InfosetSimpleElement

/**
 * Specifically designed to be used inside one of the SpecifiedLength parsers.
 *
 * This grabs a string as long as it can get, depending on the SpecifiedLength context
 * to constrain how much it can get.
 */
final class LiteralNilOfSpecifiedLengthParser(
  override val cookedNilValuesForParse: List[String],
  override val parsingPadChar: Maybe[Char],
  override val justificationTrim: TextJustificationType.Type,
  override val erd: ElementRuntimeData)
  extends PrimParser(erd)
  with StringOfSpecifiedLengthMixin
  with NilMatcherMixin {

  private val eName = erd.name

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + eName + " nilValue='" + cookedNilValuesForParse + "'/>"
  }

  override def parse(start: PState) {

    val field = parseString(start)

    val isFieldEmpty = field.length() == 0

    if (isFieldEmpty && isEmptyAllowed) {
      // Valid!
      start.thisElement.setNilled()
    } else if (isFieldEmpty && !isEmptyAllowed) {
      // Fail!
      PE(start, "%s - Empty field found but not allowed!", eName)
    } else if (isFieldNilLiteral(field)) {
      // Contains a nilValue, Success!
      start.thisElement.setNilled()
    } else {
      // Fail!
      PE(start, "%s - Does not contain a nil literal!", eName)
    }
  }

}
