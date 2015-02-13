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

package edu.illinois.ncsa.daffodil.processors.unparsers

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.processors.DFDLCharCounter
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.InfosetSimpleElement
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import edu.illinois.ncsa.daffodil.util.Misc

abstract class StringLengthUnparser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val encodingInfo: EncodingInfo)
  extends PrimUnparser(erd) with RuntimeEncodingMixin {
  override def toString = String.format("%sParser(%s)", unparserName, lengthText)

  lazy val unparserName: String = Misc.getNameFromClass(this)
  def lengthText: String

  def getLengthInBytes(state: UState): Long
  def unparseValue(start: UState, charset: Charset, nBytes: Long): Unit

  def addRightPadding(str: String): String = ???
  def addLeftPadding(str: String): String = ???
  def addPadding(str: String): String = addRightPadding(addLeftPadding(str))

  def padOrTruncateByJustification(str: String): String = {
    val result = justificationTrim match {
      case TextJustificationType.None => str
      case TextJustificationType.Right => addLeftPadding(str)
      case TextJustificationType.Left => addRightPadding(str)
      case TextJustificationType.Center => addPadding(str)
    }
    result
  }

  def unparse(state: UState): Unit = {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", state.bitPos1b)

    val nBytes = getLengthInBytes(state)
    log(LogLevel.Debug, "Explicit length %s", nBytes)

    if (state.bitPos0b % 8 != 0) { UnparseError(One(erd.schemaFileLocation), One(state), "%s - not byte aligned.", unparserName) }

    try {
      unparseValue(state, dcharset.charset, nBytes)
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented. 
      //
      case m: MalformedInputException => { UnparseError(One(erd.schemaFileLocation), One(state), "%s - MalformedInputException: \n%s", unparserName, m.getMessage()) }
      //
      // Thrown if the length is explicit but are too many bytes/bits to
      // fit within the length.
      //
      case e: IndexOutOfBoundsException => {
        UnparseError(One(erd.schemaFileLocation), One(state), "%s - Too many bits in field: IndexOutOfBounds: \n%s", unparserName, e.getMessage())
      }
    }
  }
}

class StringFixedLengthInBytesFixedWidthCharactersUnparser(
  nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo,
  override val lengthText: String)
  extends StringLengthInBytesUnparser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd,
    encInfo) {

  override def getLengthInBytes(state: UState): Long = nBytes
}

abstract class StringLengthInBytesUnparser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo)
  extends StringLengthUnparser(justificationTrim, pad, erd, encInfo) {

  def formatValue(value: String): String = {
    value
  }

  override def unparseValue(state: UState, charset: Charset, nBytes: Long): Unit = {

    val valueString = state.currentInfosetNode.get.asSimple.dataValueAsString
    val paddedValue = padOrTruncateByJustification(valueString)
    val outStream = state.outStream
    outStream.encode(charset, valueString)
    log(LogLevel.Debug, "Ended at bit position " + outStream.bitPos1b)
  }
}
