/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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