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

package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

/**
 * By "non byte sized" we mean some number of bits less than 8.
 */
trait NonByteSizeCharset {
  def bitWidthOfACodeUnit: Int // in units of bits
  def requiredBitOrder: BitOrder
}

/**
 * Mixin for Decoders for Charsets which support initial bit offsets so that
 * their character codepoints need not be byte-aligned.
 */
trait NonByteSizeCharsetDecoder
  extends NonByteSizeCharset {

  private var startBitOffset = 0
  private var startBitOffsetHasBeenSet = false
  private var startBitOffsetHasBeenUsed = false
  private var maybeBitLimitOffset0b: MaybeULong = MaybeULong.Nope

  final def setInitialBitOffset(bitOffset0to7: Int) {
    Assert.usage(!startBitOffsetHasBeenSet, "Already set. Cannot set again until decoder is reset().")
    Assert.usage(bitOffset0to7 <= 7 && bitOffset0to7 >= 0)
    startBitOffset = bitOffset0to7
    startBitOffsetHasBeenSet = true
  }

  final def setFinalByteBitLimitOffset0b(bitLimitOffset0b: MaybeULong) {
    maybeBitLimitOffset0b = bitLimitOffset0b
  }

  final protected def getFinalByteBitLimitOffset0b() = maybeBitLimitOffset0b

  final protected def getStartBitOffset() = {
    if (startBitOffsetHasBeenUsed) 0 // one time we return the value. After that 0 until a reset.
    else {
      startBitOffsetHasBeenUsed = true
      startBitOffset
    }
  }

  final protected def resetStartBit() {
    startBitOffsetHasBeenUsed = false
    startBitOffset = 0
    startBitOffsetHasBeenSet = false
  }
}


trait NonByteSizeCharsetEncoder
  extends NonByteSizeCharset
