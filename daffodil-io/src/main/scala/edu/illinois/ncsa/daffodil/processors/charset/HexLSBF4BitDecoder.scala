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
 * X-DFDL-HEX-LSBF occupies only 4 bits with each
 * code unit.
 *
 */

trait HexLSBF4BitCharsetMixin
  extends NonByteSizeCharset {

  val bitWidthOfACodeUnit = 4 // in units of bits
  val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

object HexLSBF4BitCharset
  extends java.nio.charset.Charset("X-DFDL-HEX-LSBF", Array())
  with HexLSBF4BitCharsetMixin {

  def contains(cs: Charset): Boolean = false

  def newDecoder(): CharsetDecoder = new HexLSBF4BitDecoder

  def newEncoder(): CharsetEncoder = new HexLSBF4BitEncoder

  private[charset] def charsPerByte = 8.0F / 4.0F
  private[charset] def bytesPerChar = 1.0F // can't use 4/8 here because CharsetEncoder base class requires it to be 1 or greater.
}

/**
 * You have to initialize one of these for a specific ByteBuffer because
 * the encoding is 4-bits wide, so we need additional state beyond just
 * the byte position and limit that a ByteBuffer provides in order to
 * properly sequence through the data.
 */
class HexLSBF4BitDecoder
  extends java.nio.charset.CharsetDecoder(HexLSBF4BitCharset,
    HexLSBF4BitCharset.charsPerByte, // average
    HexLSBF4BitCharset.charsPerByte) // maximum
  with NonByteSizeCharsetDecoder
  with HexLSBF4BitCharsetMixin {

  private val decodeString = "0123456789ABCDEF"

  def output(charCode: Int, out: CharBuffer) {
    out.put(decodeString(charCode))
  }
}

class HexLSBF4BitEncoder
  extends java.nio.charset.CharsetEncoder(HexLSBF4BitCharset,
    HexLSBF4BitCharset.bytesPerChar, // average
    HexLSBF4BitCharset.bytesPerChar) // maximum
  with NonByteSizeCharsetEncoder
  with HexLSBF4BitCharsetMixin {

  val replacementChar = 0x00

  def charToCharCode(char: Char): MaybeInt = {
    // Convert hex values 0 through F to decimal values 0 through 15
    // char '0' through '9' are decimal values 48 through 57, so subtract 48
    // char 'A' through 'F' are decimal values 65 through 70, so subtract 55
    // char 'a' through 'f' are decimal values 92 through 102, so subtract 87
    if (char >= '0' && char <= '9') MaybeInt(char - 48)
    else if (char >= 'A' && char <= 'F') MaybeInt(char - 55)
    else if (char >= 'a' && char <= 'f') MaybeInt(char - 87)
    else MaybeInt.Nope
  }

}
