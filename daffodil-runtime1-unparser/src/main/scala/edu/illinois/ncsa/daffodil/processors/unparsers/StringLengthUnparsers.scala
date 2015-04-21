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
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.equality._

abstract class StringLengthUnparser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData)
  extends PrimUnparser(erd) {
  override def toString = String.format("%sParser(%s)", unparserName, lengthText)

  lazy val unparserName: String = Misc.getNameFromClass(this)
  def lengthText: String

  def getLengthInBytes(state: UState): Long

  /**
   * Return value is number of characters, and the number of fill bytes
   * needed to fill out any fragment of a character because a whole
   * character will not fit at the end of the length.
   *
   * This should be
   * zero unless the length is specified in lengthUnits of bytes, and
   * the encoding is variable width, or fixed width > 1 byte.
   */
  def getLengthInChars(state: UState): (Long, Int)

  def unparseValue(start: UState, charset: Charset, nBytes: Long): Unit

  final def fillByte(state: UState): Int = erd.fillByte(state, erd.encodingInfo)

  final def addRightPadding(str: String, nChars: Int): String = {
    val pc = pad.get
    val sb = new StringBuilder(nChars, str)
    var i = nChars
    while (i > 0) {
      sb += pc
      i -= 1
    }
    sb.mkString
  }

  final def addLeftPadding(str: String, nChars: Int): String = {
    val pc = pad.get
    val sb = new StringBuilder(str.length + nChars)
    var i = nChars
    while (i > 0) {
      sb += pc
      i -= 1
    }
    sb ++= str
    sb.mkString
  }

  final def addBothPadding(str: String, nChars: Int): String = {
    val pc = pad.get
    val sb = new StringBuilder(str.length + nChars)

    val leftPaddingSize = (nChars / 2) + (nChars % 2)
    var i = leftPaddingSize
    while (i > 0) {
      sb += pc
      i -= 1
    }
    sb ++= str
    val rightPaddingSize = nChars / 2
    i = rightPaddingSize
    while (i > 0) {
      sb += pc
      i -= 1
    }
    sb.mkString
  }

  final def padOrTruncateByJustification(ustate: UState, str: String, nChars: Int): String = {
    if (str.length =#= nChars) str
    else if (str.length < nChars) {
      padByJustification(ustate, str, nChars)
    } else {
      Assert.invariant(str.length > nChars)
      truncateByJustification(ustate, str, nChars)
    }
  }

  private def truncateByJustification(ustate: UState, str: String, nChars: Int): String = {
    Assert.invariant(erd.optTruncateSpecifiedLengthString.isDefined)
    val nCharsToTrim = str.length - nChars
    val result = justificationTrim match {
      case TextJustificationType.None => {
        UnparseError(One(erd.schemaFileLocation), One(ustate), "Trimming is required, but the dfdl:textTrimKind property is 'none'.")
      }
      case TextJustificationType.Right => {
        str.substring(nCharsToTrim)
      }
      case TextJustificationType.Left => {
        str.substring(0, str.length - nCharsToTrim)
      }
      case TextJustificationType.Center => {
        UnparseError(One(erd.schemaFileLocation), One(ustate), "The dfdl:textJustificationType is 'center', but the value requires truncation to fit within the length %n.", nChars)
      }
    }
    result
  }

  private def padByJustification(ustate: UState, str: String, nChars: Int): String = {
    val nCharsToPad = nChars - str.length
    val result = justificationTrim match {
      case TextJustificationType.None => {
        UnparseError(One(erd.schemaFileLocation), One(ustate), "Padding is required, but the dfdl:textPadKind property is 'none'")
      }
      case TextJustificationType.Right => addLeftPadding(str, nCharsToPad)
      case TextJustificationType.Left => addRightPadding(str, nCharsToPad)
      case TextJustificationType.Center => addBothPadding(str, nCharsToPad)
    }
    result
  }

  def unparse(state: UState): Unit = {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", state.bitPos1b)

    val nBytes = getLengthInBytes(state)
    log(LogLevel.Debug, "Explicit length %s", nBytes)

    if (state.bitPos0b % 8 != 0) { UnparseError(One(erd.schemaFileLocation), One(state), "%s - not byte aligned.", unparserName) }

    try {
      unparseValue(state, erd.encodingInfo.knownEncodingCharset.charset, nBytes)
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
  override val lengthText: String)
  extends StringLengthInBytesUnparser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd) {

  override final def getLengthInBytes(state: UState): Long = nBytes

  override final def getLengthInChars(state: UState): (Long, Int) = {
    val encInfo = erd.encodingInfo
    Assert.usage(encInfo.isKnownEncoding)
    Assert.usage(encInfo.knownEncodingIsFixedWidth)
    Assert.usage(encInfo.knownEncodingWidthInBits % 8 =#= 0)
    val encWidthInBytes = encInfo.knownEncodingWidthInBits / 8
    val nBytes = getLengthInBytes(state)
    val nChars = nBytes / encWidthInBytes
    val nLeftOverBytes = (nBytes % encWidthInBytes).toInt
    (nChars, nLeftOverBytes)
  }
}

abstract class StringLengthInBytesUnparser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData)
  extends StringLengthUnparser(justificationTrim, pad, erd) {

  override def unparseValue(state: UState, charset: Charset, nBytes: Long): Unit = {

    val valueString = state.currentInfosetNode.get.asSimple.dataValueAsString
    var (lengthInChars, nFillBytes) = getLengthInChars(state)
    Assert.invariant(lengthInChars <= Int.MaxValue)
    val paddedValue = padOrTruncateByJustification(state, valueString, lengthInChars.toInt)
    val outStream = state.outStream
    outStream.encode(charset, paddedValue)
    if (nFillBytes > 0) {
      val fb = erd.fillByte(state, erd.encodingInfo)
      while (nFillBytes > 0) {
        outStream.writeByte(fillByte(state))
        nFillBytes -= 1
      }
    }
    log(LogLevel.Debug, "Ended at bit position " + outStream.bitPos1b)
  }
}
