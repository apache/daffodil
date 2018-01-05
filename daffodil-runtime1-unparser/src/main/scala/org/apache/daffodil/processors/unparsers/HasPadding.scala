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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.util.MaybeChar

/**
 * When dfdl:lengthKind is 'explicit' (and dfdl:length is an expression),
 * 'delimited', 'prefixed', 'pattern' the data value is padded to the length given
 * by the XSD minLength facet for type 'xs:string' or
 * dfdl:textOutputMinLength property for other types.
 *
 *
 * dfdl:textOutputMinLength:
 * Only used when dfdl:textPadKind is 'padChar' and dfdl:lengthKind is
 * 'delimited', 'prefixed', 'pattern', 'explicit' (when dfdl:length is an expression)
 * or 'endOfParent', and type is not xs:string
 * Specifies the minimum content length during unparsing for simple types
 * that do not allow the XSD minLength facet to be specified.
 * For dfdl:lengthKind 'delimited', 'pattern' and 'endOfParent' the length units
 * are always characters, for other dfdl:lengthKinds the length units are
 * specified by the dfdl:lengthUnits property.
 * If dfdl:textOutputMinLength is zero or less than the length of the
 * representation text then no padding occurs.
 */
trait PaddingRuntimeMixin {

  def pad: MaybeChar
  def padToLength: Int

  def addRightPadding(str: String): String = {
    val inputLength = str.length

    val res =
      if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
      else {
        val numCharsToAdd = padToLength - inputLength
        addRightPadding(str, numCharsToAdd)
      }
    res
  }

  def addLeftPadding(str: String): String = {
    val inputLength = str.length

    val res =
      if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
      else {
        val numCharsToAdd = padToLength - inputLength
        addLeftPadding(str, numCharsToAdd)
      }
    res
  }

  def addPadding(str: String): String = {
    val inputLength = str.length

    val res = if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
    else {
      val numCharsToAddTotal = padToLength - inputLength
      val numAddToLeft = Math.ceil(numCharsToAddTotal / 2).toInt
      val numAddToRight = numCharsToAddTotal - numAddToLeft

      addRightPadding(addLeftPadding(str, numAddToLeft), numAddToRight)
    }

    res
  }

  private def append(sb: StringBuilder, numCharsToAppend: Int): StringBuilder = {
    for (i <- 1 to numCharsToAppend) { sb.append(pad.get) }
    sb
  }

  private def addRightPadding(str: String, numCharsToPad: Int): String = {
    val sb = append(new StringBuilder(str), numCharsToPad)
    sb.toString
  }
  private def addLeftPadding(str: String, numCharsToPad: Int): String = {
    val sb = append(new StringBuilder, numCharsToPad)
    sb.append(str)
    sb.toString
  }

}