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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.processors.FillByteEv
import edu.illinois.ncsa.daffodil.processors.TextTruncationType

class StringOfSpecifiedLengthUnparser(
  val unparsingPadChar: MaybeChar,
  val justificationPad: TextJustificationType.Type,
  val stringTruncationType: TextTruncationType.Type,
  val erd: ElementRuntimeData,
  isForString: Boolean,
  isForPattern: Boolean,
  fillByteEv: FillByteEv)
    extends PrimUnparser {

  override def context = erd
  override lazy val runtimeDependencies = List(erd.encodingInfo.charsetEv)

  final def justificationTrim = justificationPad
  def padChar(state: UState): MaybeChar = unparsingPadChar

  private def getLengthInBits(str: String, state: UState): (Long, Long) = {
    state.withByteArrayOutputStream {
      case (_, dos) =>
        dos.setEncoder(state.dataOutputStream.encoder)
        val nChars = dos.putString(str)
        val nBits = dos.relBitPos0b.toLong
        (nBits, nChars)
    }
  }

  protected def contentString(state: UState) = state.currentInfosetNode.asSimple.dataValueAsString

  override def unparse(state: UState) {
    val maybeAvailableLengthInBits = {
      if (state.bitLimit0b.isDefined) MaybeULong(state.bitLimit0b.get - state.bitPos0b)
      else MaybeULong.Nope
    }
    // this is the length we have to pad to, and fillByte any fragment of a character
    //
    // Now the problem is, we don't know how much to pad (or truncate) the string
    // because until we know how many bits the string's value will take up, we
    // can't figure out how many of the available bits will be remaining to be
    // padded, and/or filled by fillByte.
    //
    // We have to stage the bits of the value just so as to be able to count them
    // Then we can figure out the number of padChars to add because a padChar must
    // be a minimum-width character.
    //
    val dos = state.dataOutputStream
    val valueString = contentString(state)
    val (valueToWrite, nBitsToFill) =
      if (maybeAvailableLengthInBits.isEmpty) {
        //
        // No limit on available space to write to
        // so we write just what we have. No padding, no truncation
        (valueString, 0L)
      } else {
        //
        // We have a limit on what space we can (and should) fill
        // Note: This is an invariant on how this unparser is used.
        // If a limit is set, then we're supposed to fill it.
        //
        val availableLengthInBits = maybeAvailableLengthInBits.get
        val (nBits, _ /* nChars */ ) = getLengthInBits(valueString, state)
        val availableLengthDiff = availableLengthInBits - nBits // can be negative if we need to truncate
        val nBitsToPadOrFill = if (isForPattern) availableLengthDiff.min(0) else availableLengthDiff // no padding/fill when lengthKind="pattern"
        val dcs = erd.encInfo.getDFDLCharset(state)
        val minBitsPerChar = erd.encodingInfo.encodingMinimumCodePointWidthInBits(dcs)
        // padChar must be a minimum-width char
        val nCharsToPad = nBitsToPadOrFill / minBitsPerChar
        val nBitsToFill = nBitsToPadOrFill % minBitsPerChar
        Assert.invariant(nCharsToPad <= Int.MaxValue)
        val pc = padChar(state)
        val paddedValue = padOrTruncateByJustification(state, valueString, pc, valueString.length + nCharsToPad.toInt)
        (paddedValue, nBitsToFill)
      }
    //
    // TODO: if this truncates a specified length string, and it's not a xs:string type, 
    // That's a processing error.
    //
    val nCharsWritten = dos.putString(valueToWrite)
    Assert.invariant(nCharsWritten == valueToWrite.length) // assertion because we figured this out above based on available space.
    if (nBitsToFill > 0) {
      val fb = fillByteEv.evaluate(state).toLong
      var nFillBytes = nBitsToFill.toInt / 8
      if (nFillBytes > 0) {
        while (nFillBytes > 0) {
          Assert.invariant(dos.putLong(fb, 8))
          nFillBytes -= 1
        }
      }
      val nFillBits = nBitsToFill.toInt % 8
      if (nFillBits > 0)
        Assert.invariant(dos.putLong(fb, nFillBits))
    }
  }

  final def addRightPadding(str: String, padChar: MaybeChar, nChars: Int): String = {
    val pc = padChar.get
    val sb = new StringBuilder(nChars, str)
    var i = nChars
    while (i > 0) {
      sb += pc
      i -= 1
    }
    sb.mkString
  }

  final def addLeftPadding(str: String, padChar: MaybeChar, nChars: Int): String = {
    val pc = padChar.get
    val sb = new StringBuilder(str.length + nChars)
    var i = nChars
    while (i > 0) {
      sb += pc
      i -= 1
    }
    sb ++= str
    sb.mkString
  }

  final def addBothPadding(str: String, padChar: MaybeChar, nChars: Int): String = {
    val pc = padChar.get
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

  final def padOrTruncateByJustification(ustate: UState, str: String, padChar: MaybeChar, nChars: Int): String = {
    if (str.length =#= nChars) str
    else if (str.length < nChars) {
      padByJustification(ustate, str, padChar, nChars)
    } else if (isForString) {
      Assert.invariant(str.length > nChars)
      truncateByJustification(ustate, str, nChars)
    } else {
      str
    }
  }

  /**
   * We only truncate strings, and only if textStringJustification is left or right, and only if truncateSpecifiedLengthString is yes.
   */
  private def truncateByJustification(ustate: UState, str: String, nChars: Int): String = {
    Assert.invariant(isForString)
    Assert.invariant(erd.optTruncateSpecifiedLengthString.isDefined)
    val nCharsToTrim = str.length - nChars
    val result = stringTruncationType match {
      case TextTruncationType.None => {
        Assert.invariant(erd.optTruncateSpecifiedLengthString.get =#= false)
        UnparseError(One(erd.schemaFileLocation), One(ustate.currentLocation), "String truncation is required, but the dfdl:truncateSpecifiedLengthString property is 'no'.")
      }
      case TextTruncationType.Right => {
        str.substring(nCharsToTrim)
      }
      case TextTruncationType.Left => {
        str.substring(0, str.length - nCharsToTrim)
      }
    }
    result
  }

  private def padByJustification(ustate: UState, str: String, padChar: MaybeChar, nChars: Int): String = {
    val nCharsToPad = nChars - str.length
    val result = justificationTrim match {
      case TextJustificationType.None => {
        if (isForString)
          UnparseError(One(erd.schemaFileLocation), One(ustate.currentLocation), "Padding is required, but the dfdl:textPadKind property is 'none'")
        else
          str
      }
      case TextJustificationType.Right => addLeftPadding(str, padChar, nCharsToPad)
      case TextJustificationType.Left => addRightPadding(str, padChar, nCharsToPad)
      case TextJustificationType.Center => addBothPadding(str, padChar, nCharsToPad)
    }
    result
  }
}
