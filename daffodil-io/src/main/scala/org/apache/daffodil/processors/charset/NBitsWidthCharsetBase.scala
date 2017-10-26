/* Copyright (c) 2012-2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors.charset

import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CharsetEncoder

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.io.NonByteSizeCharset
import org.apache.daffodil.io.NonByteSizeCharsetDecoder
import org.apache.daffodil.io.NonByteSizeCharsetEncoder
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.util.MaybeInt

/**
 * Some encodings are not byte-oriented.
 *
 * If we know the correspondence from integers to characters, and we can express that
 * as a string, then everything else can be derived
 */

class NBitsWidthCharset(name: String, decodeString: String,
  override val bitWidthOfACodeUnit: Int,
  override val requiredBitOrder: BitOrder,
  replacementCharCode: Int)
  extends java.nio.charset.Charset(name, Array())
  with NonByteSizeCharset { self =>

  Assert.usage(decodeString.length == math.pow(2, bitWidthOfACodeUnit))
  Assert.usage(bitWidthOfACodeUnit <= 7 && bitWidthOfACodeUnit >= 1)

  def contains(cs: Charset): Boolean = false

  def newDecoder(): CharsetDecoder = new NBitsWidthCharsetDecoder(this)

  def newEncoder(): CharsetEncoder = new NBitsWidthCharsetEncoder(this,
    replacementCharCode)

  //
  // Note: because this array is only 256, these non-bit width encodings
  // can only support characters in the first 256 unicode characters.
  //
  // The largest such array is 128 (for 7 bits)
  //
  // However, unicode FFFD (replacement character) is accepted, and treated
  // as explicit unmapped. Any other unicode > 256 causes an error.
  //
  private lazy val encodeArray: Array[MaybeInt] = {
    val len = decodeString.length
    val arr = new Array[MaybeInt](256)
    (0 to 255).foreach { index =>
      arr(index) = MaybeInt.Nope
    }
    val imap = decodeString zip (0 to (len - 1))
    imap.foreach {
      case (c, index) => {
        if (c < 256)
          arr(c) = MaybeInt(index)
        else if (c == '\uFFFD') {
          // ok. unmapped on purpose.
        } else
          Assert.invariantFailed("Char with code %n found. Character codes must be < 256.".format(c.toInt))
      }
    }
    arr
  }

  def codeToChar(code: Int) = {
    if (code > decodeString.length)
      '\uFFFD' // unicode replacement character
    else
      decodeString.charAt(code)
  }

  def charToCode(char: Char) =
    if (char < 256) encodeArray(char)
    else MaybeInt.Nope
}

sealed trait NBitsWidthEncoderDecoderMixin {

  def cs: NBitsWidthCharset
  final def bitWidthOfACodeUnit = cs.bitWidthOfACodeUnit
  final def requiredBitOrder = cs.requiredBitOrder
}

/**
 * You have to initialize one of these for a specific ByteBuffer because
 * the encoding is N-bits wide, so we need additional state beyond just
 * the byte position and limit that a ByteBuffer provides in order to
 * properly sequence through the data.
 */
class NBitsWidthCharsetDecoder(override val cs: NBitsWidthCharset)
  extends java.nio.charset.CharsetDecoder(cs,
    8.0F / cs.bitWidthOfACodeUnit.toFloat, // average
    8.0F / cs.bitWidthOfACodeUnit.toFloat) // maximum.
  with NonByteSizeCharsetDecoder
  with NBitsWidthEncoderDecoderMixin {

  def output(charCode: Int, out: CharBuffer) {
    out.put(cs.codeToChar(charCode))
  }
}

class NBitsWidthCharsetEncoder(override val cs: NBitsWidthCharset,
  override val replacementChar: Int)
  extends java.nio.charset.CharsetEncoder(cs,
    cs.bitWidthOfACodeUnit.toFloat / 8.0F, // average
    1.0F) // maximum. Note can't use bitWidth/8.0 here because CharsetEncoder base class requires it to be 1 or greater.) // maximum
  with NonByteSizeCharsetEncoder
  with NBitsWidthEncoderDecoderMixin {

  def charToCharCode(char: Char): MaybeInt = {
    cs.charToCode(char)
  }

}
