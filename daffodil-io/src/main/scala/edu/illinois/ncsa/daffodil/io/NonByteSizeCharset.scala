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
