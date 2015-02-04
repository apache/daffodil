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

import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.dsom.ConstantExpression
import edu.illinois.ncsa.daffodil.processors.ISO8859EncodingInfo

abstract class HexBinaryLengthInBytesParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData)
  extends StringLengthInBytesParser(
    justificationTrim,
    pad,
    erd,
    new ISO8859EncodingInfo(erd)) {

  val hexArray = "0123456789ABCDEF".toCharArray

  override def formatValue(value: String) = {
    val sb = new StringBuilder(value.length * 2)
    for (c <- value) {
      val i = c.toInt
      sb += hexArray(i >>> 4)
      sb += hexArray(i & 0x0F)
    }
    sb.result
  }
}

class HexBinaryFixedLengthInBytesParser(nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val lengthText: String)
  extends HexBinaryLengthInBytesParser(justificationTrim, pad, erd) {

  lazy val parserName = "HexBinaryFixedLengthInBytes"

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
}

class HexBinaryFixedLengthInBitsParser(nBits: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val lengthText: String)
  extends HexBinaryLengthInBytesParser(justificationTrim, pad, erd) {

  lazy val parserName = "HexBinaryFixedLengthInBits"

  def getLength(pstate: PState): (Long, PState) = {
    val nBytes = scala.math.ceil(nBits / 8).toLong
    (nBytes, pstate)
  }
}

class HexBinaryVariableLengthInBytesParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  codepointWidth: Int,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends HexBinaryLengthInBytesParser(justificationTrim, pad, erd)
  with HasVariableLength {

  lazy val parserName = "HexBinaryVariableLengthInBytes"
}
