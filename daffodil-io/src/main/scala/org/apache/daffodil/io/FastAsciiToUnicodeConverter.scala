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

package edu.illinois.ncsa.daffodil.io

import java.nio.ByteBuffer

/**
 * Highly optimized converter for Ascii to Unicode
 */
object FastAsciiToUnicodeConverter {

  def convert(bb: ByteBuffer) = {
    val cb = ByteBuffer.allocate(2 * bb.limit)
    val cbChar = cb.asCharBuffer()
    //
    // Go after data in the largest chunks we can (Long)
    // so as to eliminate per-byte/char bounds checks
    //
    val bbBytesOfWholeLongWords = ((bb.limit >> 3) << 3).toLong
    val numBytesTrailingFragment = bb.limit - bbBytesOfWholeLongWords

    val bLong = bb.asLongBuffer()
    val cbLong = cb.asLongBuffer()
    1 to bLong.limit foreach { i =>
      val bbl = bLong.get()
      val long1: Int = (bbl >> 32).toInt & 0xFFFFFFFF
      val long2: Int = bbl.toInt & 0xFFFFFFFF
      val cbl1 = convertLong(long1)
      val cbl2 = convertLong(long2)
      cbLong.put(cbl1)
      cbLong.put(cbl2)
    }

    1 to numBytesTrailingFragment.toInt foreach { j =>
      val pos = bb.limit - j
      val byte = bb.get(pos)
      val char = convertByte(byte)
      cbChar.put(pos, char)
    }

    cb.asCharBuffer()
  }

  val UnicodeReplacementCharacter = 0xFFFD.toChar
  /**
   * Convert a single byte of ascii to unicode.
   * If the MSBit is set (negative byte) then that's
   * not a legal character code so produce the unicode
   * replacement character.
   */
  @inline
  def convertByte(byte: Byte) = {
    if (byte < 0) UnicodeReplacementCharacter
    else byte.toChar
  }

  @inline
  def convertInt(int: Int) = {
    val i = int & 0xFF
    if (i > 127) UnicodeReplacementCharacter
    else i.toChar
  }

  @inline
  def convertLong(bytes: Int): Long = {
    val int1 = bytes & 0xFF
    val int2 = (bytes >> 8) & 0xFF
    val int3 = (bytes >> 16) & 0xFF
    val int4 = (bytes >> 24) & 0xFF
    val char1 = convertInt(int1)
    val char2 = convertInt(int2)
    val char3 = convertInt(int3)
    val char4 = convertInt(int4)
    val res = (char4.toLong << 48) | (char3.toLong << 32) | (char2.toLong << 16) | char1
    res
  }

}