package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.MaybeULong

/**
 * By "non byte sized" we mean some number of bits less than 8.
 */
trait NonByteSizeCharset {
  def bitWidthOfACodeUnit: Int // in units of bits
}

/**
 * Mixin for Charsets which support initial bit offsets so that
 * their character codepoints need not be byte-aligned.
 */
trait NonByteSizeCharsetEncoderDecoder
  extends NonByteSizeCharset {
  def setInitialBitOffset(bitOffset0to7: Int): Unit
  def setFinalByteBitLimitOffset0b(bitLimitOffset0b: MaybeULong): Unit
}
