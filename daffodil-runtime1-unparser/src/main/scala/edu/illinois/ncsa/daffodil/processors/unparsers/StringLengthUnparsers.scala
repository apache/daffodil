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

class StringOfSpecifiedLengthUnparser(
  val unparsingPadChar: MaybeChar,
  val justificationPad: TextJustificationType.Type,
  val erd: ElementRuntimeData,
  isForString: Boolean,
  isForPattern: Boolean)
  extends PrimUnparser
  with StringLengthMixin {

  override def context = erd
  override lazy val runtimeDependencies = List(erd.encodingInfo.charsetEv)

  final override def justificationTrim = justificationPad
  final override def pad = unparsingPadChar

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
        val cs = erd.encodingInfo.getEncoder(state).charset
        val minBitsPerChar = erd.encodingInfo.encodingMinimumCodePointWidthInBits(cs)
        // padChar must be a minimum-width char
        val nCharsToPad = nBitsToPadOrFill / minBitsPerChar
        val nBitsToFill = nBitsToPadOrFill % minBitsPerChar
        Assert.invariant(nCharsToPad <= Int.MaxValue)
        val paddedValue = padOrTruncateByJustification(state, valueString, valueString.length + nCharsToPad.toInt, isForString)
        (paddedValue, nBitsToFill)
      }
    val nCharsWritten = dos.putString(valueToWrite)
    Assert.invariant(nCharsWritten == valueToWrite.length) // assertion because we figured this out above based on available space.
    var nFillBytes = nBitsToFill.toInt / 8
    val fb = fillByte(state)
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

trait StringLengthMixin {

  def erd: ElementRuntimeData
  def pad: MaybeChar
  def justificationTrim: TextJustificationType.Type

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

  final def padOrTruncateByJustification(ustate: UState, str: String, nChars: Int, isForString: Boolean): String = {
    if (str.length =#= nChars) str
    else if (str.length < nChars) {
      padByJustification(ustate, str, nChars, isForString)
    } else {
      Assert.invariant(str.length > nChars)
      truncateByJustification(ustate, str, nChars, isForString)
    }
  }

  private def truncateByJustification(ustate: UState, str: String, nChars: Int, isForString: Boolean): String = {
    if (isForString) Assert.invariant(erd.optTruncateSpecifiedLengthString.isDefined)
    val nCharsToTrim = str.length - nChars
    val result = justificationTrim match {
      case TextJustificationType.None => {
        if (isForString)
          UnparseError(One(erd.schemaFileLocation), One(ustate.currentLocation), "Trimming is required, but the dfdl:textTrimKind property is 'none'.")
        else str
      }
      case TextJustificationType.Right => {
        str.substring(nCharsToTrim)
      }
      case TextJustificationType.Left => {
        str.substring(0, str.length - nCharsToTrim)
      }
      case TextJustificationType.Center => {
        UnparseError(One(erd.schemaFileLocation), One(ustate.currentLocation), "The dfdl:textJustificationType is 'center', but the value requires truncation to fit within the length %n.", nChars)
      }
    }
    result
  }

  private def padByJustification(ustate: UState, str: String, nChars: Int, isForString: Boolean): String = {
    val nCharsToPad = nChars - str.length
    val result = justificationTrim match {
      case TextJustificationType.None => {
        if (isForString)
          UnparseError(One(erd.schemaFileLocation), One(ustate.currentLocation), "Padding is required, but the dfdl:textPadKind property is 'none'")
        else
          str
      }
      case TextJustificationType.Right => addLeftPadding(str, nCharsToPad)
      case TextJustificationType.Left => addRightPadding(str, nCharsToPad)
      case TextJustificationType.Center => addBothPadding(str, nCharsToPad)
    }
    result
  }
}
