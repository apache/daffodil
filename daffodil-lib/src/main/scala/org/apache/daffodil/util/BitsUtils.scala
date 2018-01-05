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

package org.apache.daffodil.util

object BitsUtils {

  /**
   * From DFDL Spec. Sept 2013 draft 1.0.4, Section 13.7.1.4
   *
   * The value of a bit does not depend on the alignment of that bit,
   * but only where it appears in the bit string, and the byte order when
   * the length of the bit string is greater than 1 byte.
   *
   * Implements 2^N exponentiation with shifting 1 << N.
   */
  def littleEndianBitValue(bitPosition: Int, bitStringLength: Int) = {
    assert(bitPosition >= 1) // one based
    assert(bitStringLength >= 1)
    assert(bitStringLength >= bitPosition) // bit pos within the bit string length
    val numBitsInFinalPartialByte = bitStringLength % 8
    val numBitsInWholeBytes = bitStringLength - numBitsInFinalPartialByte
    val bitPosInByte = ((bitPosition - 1) % 8) + 1
    val widthOfActiveBitsInByte =
      if (bitPosition <= numBitsInWholeBytes)
        8 else numBitsInFinalPartialByte
    val placeValueExponentOfBitInByte = widthOfActiveBitsInByte - bitPosInByte
    val bitValueInByte = 1 << placeValueExponentOfBitInByte
    val byteNumZeroBased = (bitPosition - 1) / 8
    val scaleFactorForBytePosition = 1 << (8 * byteNumZeroBased)
    val bitValue = bitValueInByte * scaleFactorForBytePosition
    bitValue
  }

}