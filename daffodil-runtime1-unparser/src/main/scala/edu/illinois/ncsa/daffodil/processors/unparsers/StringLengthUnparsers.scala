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
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.processors.TextTruncationType

/**
 * Truncates strings to the right length if we're supposed to be truncating.
 */
class StringOfSpecifiedLengthUnparser(
  val stringTruncationType: TextTruncationType.Type,
  val erd: ElementRuntimeData,
  isForString: Boolean,
  isForPattern: Boolean)
  extends PrimUnparser {

  override def context = erd
  override lazy val runtimeDependencies = Nil

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

    //
    // We have to stage the bits of the value just so as to be able to count them
    // Then we can figure out the number of padChars to add because a padChar must
    // be a minimum-width character.
    //
    val dos = state.dataOutputStream
    val valueString = contentString(state)
    val valueToWrite =
      if (stringTruncationType _eq_ TextTruncationType.None)
        valueString
      else {
        val maybeAvailableLengthInBits = state.dataOutputStream.remainingBits

        if (maybeAvailableLengthInBits.isEmpty) {
          //
          // No limit on available space to write to
          // so we write just what we have. No padding, no truncation
          valueString
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
          val nCharsToPad = nBitsToPadOrFill / minBitsPerChar // negative if we need to truncate.
          Assert.invariant(nCharsToPad <= Int.MaxValue)
          val paddedValue = truncateByJustification(state, valueString, valueString.length + nCharsToPad.toInt)
          Assert.invariant(paddedValue.length <= valueString.length)
          paddedValue
        }
      }
    //
    val nCharsWritten = dos.putString(valueToWrite)
    Assert.invariant(nCharsWritten == valueToWrite.length) // assertion because we figured this out above based on available space.
    //
    // Filling of unused bits is done elsewhere now
    //
  }

  /**
   * We only truncate strings, and only if textStringJustification is left or
   * right, and only if truncateSpecifiedLengthString is yes.
   */
  private def truncateByJustification(ustate: UState, str: String, nChars: Int): String = {
    Assert.invariant(isForString)
    Assert.invariant(erd.optTruncateSpecifiedLengthString.isDefined)
    val nCharsToTrim = str.length - nChars
    val result = stringTruncationType match {
      case TextTruncationType.None => {
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

}
