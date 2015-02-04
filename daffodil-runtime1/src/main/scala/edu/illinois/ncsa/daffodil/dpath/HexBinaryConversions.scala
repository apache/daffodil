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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert

object HexBinaryConversions {

  def toByteArray(b: Byte): Array[Byte] = Array(b)
  def toByteArray(s: Short): Array[Byte] = {
    val upper = ((s >> 8) & 0x00FF).toByte
    val lower = (s & 0x00FF).toByte
    Array[Byte](upper, lower)
  }
  def toByteArray(i: Integer): Array[Byte] = {
    val byte0 = ((i >> 24) & 0x000000FF).toByte
    val byte1 = ((i >> 16) & 0x000000FF).toByte
    val byte2 = ((i >> 8) & 0x000000FF).toByte
    val byte3 = (i & 0x000000FF).toByte

    Array[Byte](byte0, byte1, byte2, byte3)
  }
  def toByteArray(l: Long): Array[Byte] = {
    val i0: Integer = ((l >> 32) & 0xFFFFFFFF).toInt
    val i1: Integer = (l & 0xFFFFFFFF).toInt
    val arr0 = toByteArray(i0)
    val arr1 = toByteArray(i1)

    arr0 ++ arr1
  }

}