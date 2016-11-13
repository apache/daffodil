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

package edu.illinois.ncsa.daffodil.processors.charset

import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CharsetEncoder

import edu.illinois.ncsa.daffodil.io.NonByteSizeCharset
import edu.illinois.ncsa.daffodil.io.NonByteSizeCharsetDecoder
import edu.illinois.ncsa.daffodil.io.NonByteSizeCharsetEncoder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.MaybeInt

/**
 * Some encodings are not byte-oriented.
 *
 * X-DFDL-5-BIT-PACKED occupies only 5 bits with each
 * code unit.
 *
 */

trait DFDL5BitPackedCharsetMixin
  extends NonByteSizeCharset {

  val bitWidthOfACodeUnit = 5 // in units of bits
  val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

object DFDL5BitPackedCharset
  extends java.nio.charset.Charset("X-DFDL-5-BIT-PACKED", Array())
  with DFDL5BitPackedCharsetMixin {

  def contains(cs: Charset): Boolean = false

  def newDecoder(): CharsetDecoder = new DFDL5BitPackedDecoder

  def newEncoder(): CharsetEncoder = new DFDL5BitPackedEncoder

  private[charset] def charsPerByte = 8.0F / 5.0F
  private[charset] def bytesPerChar = 1.0F // can't use 5/8 here because CharsetEncoder base class requires it to be 1 or greater.
}

/**
 * You have to initialize one of these for a specific ByteBuffer because
 * the encoding is 5-bits wide, so we need additional state beyond just
 * the byte position and limit that a ByteBuffer provides in order to
 * properly sequence through the data.
 */
class DFDL5BitPackedDecoder
  extends java.nio.charset.CharsetDecoder(DFDL5BitPackedCharset,
    DFDL5BitPackedCharset.charsPerByte, // average
    DFDL5BitPackedCharset.charsPerByte) // maximum
  with NonByteSizeCharsetDecoder
  with DFDL5BitPackedCharsetMixin {

  private val decodeString = "01234567ABCDEFGHJKLMNPQRSTUVWXYZ"

  def output(charCode: Int, out: CharBuffer) {
    out.put(decodeString(charCode))
  }
}

class DFDL5BitPackedEncoder
  extends java.nio.charset.CharsetEncoder(DFDL5BitPackedCharset,
    DFDL5BitPackedCharset.bytesPerChar, // average
    DFDL5BitPackedCharset.bytesPerChar) // maximum
  with NonByteSizeCharsetEncoder
  with DFDL5BitPackedCharsetMixin {

  val replacementChar = 0x1D

  def charToCharCode(char: Char): MaybeInt = {

    if (char >= '0' && char <= '7') MaybeInt(char - 48)
    else if (char >= 'A' && char <= 'H') MaybeInt(char - 57)
    else if (char >= 'J' && char <= 'N') MaybeInt(char - 58)
    else if (char >= 'P' && char <= 'Z') MaybeInt(char - 59)
    else if (char == 'I') MaybeInt(1)
    else if (char == 'O') MaybeInt(0)
    else MaybeInt.Nope
  }

}
