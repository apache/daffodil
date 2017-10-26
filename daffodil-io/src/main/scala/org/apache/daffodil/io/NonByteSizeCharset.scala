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

package org.apache.daffodil.io

import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import java.nio.ByteBuffer
import java.nio.CharBuffer
import org.apache.daffodil.util.Bits
import java.nio.charset.CoderResult
import java.nio.charset.CodingErrorAction
import org.apache.daffodil.util.MaybeInt
import org.apache.daffodil.util.Maybe

/**
 * By "non byte sized" we mean some number of bits less than 8.
 */
trait NonByteSizeCharset {
  def bitWidthOfACodeUnit: Int // in units of bits
  def requiredBitOrder: BitOrder

  protected def checks() {}

  protected final lazy val init = {
    checks()
  }
}

/**
 * Mixin for Decoders for Charsets which support initial bit offsets so that
 * their character codepoints need not be byte-aligned.
 */
trait NonByteSizeCharsetDecoder
  extends NonByteSizeCharset { self: java.nio.charset.CharsetDecoder =>

  private var startBitOffset0b = 0
  private var startBitOffsetHasBeenSet = false
  private var startBitOffsetHasBeenUsed = false

  /**
   * Stores number of bits in the fragment byte at the end of the data.
   *
   * The frag byte value is an additional byte 1 past the end of the
   * byte buffer's last active position. That is, it is at the limit location.
   *
   * This limit location must exist. I.e., the limit must be less than the capacity.
   */
  private var maybeBitLimitOffset0b: MaybeULong = MaybeULong.Nope
  private var priorByte: MaybeInt = MaybeInt.Nope
  private var fragByte_ : MaybeInt = MaybeInt.Nope // populated from bytebuffer's final fragment byte if there is one.

  protected override def implReset() {
    resetStartBit()
    maybeBitLimitOffset0b = MaybeULong.Nope
    priorByte = MaybeInt.Nope
    priorByteBitCount = 0
    fragByte_ = MaybeInt.Nope
  }

  private def getFragByte(in: ByteBuffer): Int = {
    Assert.usage(this.maybeBitLimitOffset0b.isDefined &&
      this.maybeBitLimitOffset0b.get > 0)
    Assert.usage(in.limit() < in.capacity()) // room for 1 more byte.
    Assert.usage(in.remaining() == 0) // only call this after consuming all bytes.
    if (fragByte_.isEmpty) {
      val savedLimit = in.limit()
      in.limit(savedLimit + 1)
      val finalByte = in.get(savedLimit)
      in.limit(savedLimit)
      fragByte_ = MaybeInt(finalByte)
    }
    fragByte_.get
  }

  /**
   * Bit-oriented decoders have to be passed a standard byteBuffer and CharBuffer
   * via the normal Java decodeLoop API. However, as we are bit oriented, there's the
   * possibility that there is a fragment of a byte at the end, and there
   * can be an offset within the first byte for the start.
   *
   * So to get that information over to the decoder, given that the decodeLoop
   * API doesn't pass it, we have setters.
   *
   * So a decoder is stateful. They're always stateful, so this isn't changing
   * that. We're just adding yet more state.
   */

  final def setInitialBitOffset(bitOffset0to7: Int) {
    Assert.usage(!startBitOffsetHasBeenSet, "Already set. Cannot set again until decoder is reset().")
    Assert.usage(bitOffset0to7 <= 7 && bitOffset0to7 >= 0)
    startBitOffset0b = bitOffset0to7
    startBitOffsetHasBeenSet = true
  }

  /**
   * If set, then it means there IS one more byte (at least) at the end of
   * the ByteBuffer which holds the fragment byte. This final byte is past the
   * limit of the ByteBuffer.
   */
  final def setFinalByteBitLimitOffset0b(bitLimitOffset0b: MaybeULong) {
    maybeBitLimitOffset0b = bitLimitOffset0b
  }

  final protected def getFinalByteBitLimitOffset0b() = maybeBitLimitOffset0b

  /**
   * Calling this returns the startBitOffset0b, but also sets flags so
   * that it won't be processed again. It's a once-only thing until a reset.
   */
  final protected def takeStartBitOffset() = {
    if (startBitOffsetHasBeenUsed) 0 // one time we return the value. After that 0 until a reset.
    else {
      startBitOffsetHasBeenUsed = true
      startBitOffset0b
    }
  }

  final protected def resetStartBit() {
    startBitOffsetHasBeenUsed = false
    startBitOffset0b = 0
    startBitOffsetHasBeenSet = false
  }

  /**
   * When there is a prior byte, this gives the number of bits from it
   * that have not yet been consumed. Zero when there is no prior byte.
   *
   * Value ranges from 1 to 7.
   *
   * There is a case where it is set to 8, which is when we put
   * a new whole byte from the input into it, and then immediately
   * remove a charcode from it.
   */
  private var priorByteBitCount = 0

  /*
   * These predicates make reasoning about the various combinations
   * easier.
   *
   * For the input, we can have data (whole bytes), a fragment of a byte,
   * or nothing.
   *
   * For the output, we either have room for a character, nor not.
   *
   * For the decoder itself, we have whether there is a prior byte
   * of data (generally part of a byte) or not.
   */

  /**
   * No whole bytes are available
   */
  @inline private final def NoData(in: ByteBuffer) = !YesData(in) && NoFrag

  /**
   * At least 1 whole byte is available
   */
  @inline private final def YesData(in: ByteBuffer) = in.hasRemaining()

  /**
   * Space for at least 1 character of output
   */
  @inline private final def YesSpace(out: CharBuffer) = out.hasRemaining()

  /**
   * No partially consumed prior byte
   */
  private final def NoPrior = {
    val res = !YesPrior
    if (res) Assert.invariant(priorByteBitCount == 0)
    res
  }

  /**
   * There is a partially consumed prior byte. Contains at least 1 bit.
   *
   * When we first create a priorByte it may contain 8 bits, but that
   * is a transient condition. When this is called it should be 1 to 7
   */
  private final def YesPrior = {
    val res = priorByte.isDefined
    if (res) Assert.invariant(priorByteBitCount > 0 && priorByteBitCount <= 7)
    res
  }

  @inline private def takeCharCodeFromPriorByte() =
    takeBitsFromPriorByte(bitWidthOfACodeUnit)

  private def takeBitsFromPriorByte(nBits: Int) = {
    Assert.usage(nBits > 0)
    Assert.invariant(priorByte.isDefined)
    val cnt = priorByteBitCount
    Assert.invariant(cnt >= nBits)
    val pb = priorByte.get
    val mask = 0xFF >> (8 - nBits)
    val isolatingShift =
      if (this.requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
        cnt - nBits
      } else {
        // LSBF
        8 - cnt
      }
    val charCode =
      (pb >>> isolatingShift) & mask
    priorByteBitCount = priorByteBitCount - nBits
    if (priorByteBitCount == 0)
      priorByte = MaybeInt.Nope
    charCode
  }

  @inline private def takeCharCodeFromFragByte(in: ByteBuffer): Int =
    takeBitsFromFragByte(in, bitWidthOfACodeUnit)

  private def takeBitsFromFragByte(in: ByteBuffer, nBits: Int): Int = {
    val fb = getFragByte(in)
    val nBitsInFrag = this.maybeBitLimitOffset0b.get
    Assert.invariant(nBitsInFrag >= nBits)
    var bits = 0
    var newFB = 0
    val mask = 0xFF >> (8 - nBits)
    if (this.requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
      // MSBF
      bits = (fb >>> (8 - nBits)) & mask
      newFB = (fb << nBits) & 0xFF
    } else {
      // LSBF
      bits = fb & mask
      newFB = (fb >>> nBits) & 0xFF
    }
    val newLimit = nBitsInFrag - nBits
    if (newLimit > 0)
      maybeBitLimitOffset0b = MaybeULong(newLimit)
    else
      maybeBitLimitOffset0b = MaybeULong.Nope
    fragByte_ =
      if (maybeBitLimitOffset0b.isEmpty)
        MaybeInt.Nope
      else
        MaybeInt(newFB)
    bits
  }

  /**
   * No partial fragment byte at the end of the data (after all whole bytes)
   */
  private def NoFrag = {
    val res = maybeBitLimitOffset0b.isEmpty
    if (res)
      Assert.invariant(fragByte_.isEmpty)
    res
  }

  /**
   * There is a fragment byte after all whole bytes. Contains at least 1 bit.
   */
  private def YesFrag(in: ByteBuffer) = {
    val res = maybeBitLimitOffset0b.isDefined && !in.hasRemaining()
    if (res) {
      val lim = maybeBitLimitOffset0b.get
      if (lim == 0) Assert.invariant(fragByte_.isEmpty)
      Assert.invariant(lim >= 0 && lim <= 7)
    }
    res
  }

  /**
   *  Returns false if not enough bits are in the data to handle the start offset.
   */
  private def handleStartOffset(in: ByteBuffer, out: CharBuffer): Boolean = {
    val bitOffset = takeStartBitOffset()
    if (bitOffset != 0) {
      Assert.invariant(NoPrior)
      // if there's a start offset, then we're just starting.
      // we have to offset into the first byte if there is one.
      if (YesData(in)) {
        priorByte = MaybeInt(Bits.asUnsignedByte(in.get()))
        priorByteBitCount = 8
        this.takeBitsFromPriorByte(bitOffset)
        // fall through to main loop below, but now we have a prior byte, and no start offset.
        true
      } else if (YesFrag(in)) {
        // no whole bytes of data. But there could still be a frag byte
        if (this.maybeBitLimitOffset0b.get >= bitOffset) {
          // we have to have enough bits in the frag.
          // TODO: TBD should the above be a return UNDERFLOW instead of abort?
          takeBitsFromFragByte(in, bitOffset)
          true
        } else false
      } else {
        false
      }
    } else
      true
  }

  final def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult = {
    init
    //
    // Now we have to adjust for the starting bit offset
    // Grab first byte so we can start at an offset into it.
    //
    if (!handleStartOffset(in, out)) {
      CoderResult.UNDERFLOW // not enough data to handle the starting offset
    } else {
      //
      // Once around this loop per character decoded.
      //
      // Note that we can enter this loop at any state. E.g., we could have pulled
      // a byte from the input, decoded some bits from it (to go with earlier bits)
      // but still have more to go in that character, but have to return from this
      // loop due to charBuf overflow. When we re-enter this loop we
      // presumably will have more charBuf space, and we have to pick up where
      // we left off.
      //
      // States:
      //    YesPrior or NoPrior - byte currently being consumed
      //    Output: YesSpace or NoSpace
      //    Input: YesData (more whole bytes), YesFrag (partial final byte), NoData (no more data of any kind)
      //
      var rc: Maybe[CoderResult] = Maybe.Nope
      while (rc.isEmpty) {
        rc = attemptToDecodeOne(in, out)
      }
      rc.get
    }
  }

  /**
   * Attempt to decode a single character.
   *
   * Returns Maybe.Nope to indicate successful decode.
   * Returns One(CoderResult.UNDERFLOW) or One(CoderResult.OVERFLOW) to
   * indicate it was unable to decode.
   */
  private val underflow = Maybe(CoderResult.UNDERFLOW)
  private val overflow = Maybe(CoderResult.OVERFLOW)

  // package private - so we can call for unit testing. But this is not
  // part of the public interface of these decoders.
  //
  private[io] def attemptToDecodeOne(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    if (YesPrior)
      attemptToDecodeOneYesPrior(in, out)
    else
      attemptToDecodeOneNoPrior(in, out)
  }

  private def attemptToDecodeOneYesPrior(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    //
    // YesPrior Cases - where there is a prior byte, and it has enough
    // bits to produce a character just from there.
    //
    if (priorByteBitCount >= bitWidthOfACodeUnit) {
      if (YesSpace(out)) {
        // there is space in the charbuffer, produce a character
        val charCode = this.takeCharCodeFromPriorByte() // priorByteBitCount gets smaller.
        output(charCode, out)
        Maybe.Nope
      } else {
        overflow
      }
    } else //
    //
    // YesPrior Cases - but prior byte alone is not enough, but YesSpace
    //
    if (YesSpace(out)) {
      attemptToDecodeOneYesPriorYesSpace(in, out)
    } else {
      attemptToDecodeOneYesPriorNoSpace(in, out)
    }
  }

  private def attemptToDecodeOneYesPriorYesSpace(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    if (YesData(in))
      attemptToDecodeOneYesPriorYesSpaceYesData(in, out)
    else if (YesFrag(in))
      attemptToDecodeOneYesPriorYesSpaceYesFrag(in, out)
    else if (NoData(in))
      attemptToDecodeOneYesPriorYesSpaceNoData(in, out)
    else
      Assert.impossible("All cases should have been exhausted")
  }

  private lazy val mask = 0xFF >>> (8 - bitWidthOfACodeUnit)

  def attemptToDecodeOneYesPriorYesSpaceYesData(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    // we need more bits from the next byte. At least 1 bit comes from
    // priorByte. We will produce a character and go around again, because
    // a whole byte from input always provides enough bits.
    val nNeededBits = bitWidthOfACodeUnit - priorByteBitCount
    Assert.invariant(nNeededBits >= 1)
    val currentByte = Bits.asUnsignedByte(in.get())
    val nPriorBits = priorByteBitCount
    val priorBits = takeBitsFromPriorByte(priorByteBitCount)
    val nCurrentBits = (bitWidthOfACodeUnit - nPriorBits)

    val charCode =
      if (requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
        val priorBitsInPosition = priorBits << nCurrentBits
        val currentBits = currentByte >>> (8 - nCurrentBits)
        val cc = (priorBitsInPosition | currentBits) & mask
        cc
      } else {
        // LSBF
        val priorBitsInPosition = priorBits
        val currentBits = (currentByte << nPriorBits) & mask
        val cc = (priorBitsInPosition | currentBits) & mask
        cc
      }
    priorByte = MaybeInt(currentByte)
    priorByteBitCount = 8 - nCurrentBits // priorByte will become the unconsumed bits from data byte.
    output(charCode, out)
    Maybe.Nope
  }

  def attemptToDecodeOneYesPriorYesSpaceYesFrag(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    if (priorByteBitCount + this.maybeBitLimitOffset0b.get >= bitWidthOfACodeUnit) {
      // Case where combining prior with frag gets us enough bits for a character
      // We will produce a character and go around again.
      val nPriorBits = priorByteBitCount
      val priorBits = takeBitsFromPriorByte(priorByteBitCount)
      val nCurrentBits = (bitWidthOfACodeUnit - nPriorBits)
      val currentBits = takeBitsFromFragByte(in, nCurrentBits)
      val charCode =
        if (requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
          val priorBitsInPosition = priorBits << nCurrentBits
          val cc = (priorBitsInPosition | currentBits) & mask
          cc
        } else {
          // LSBF
          val currentBitsInPosition = currentBits << nPriorBits
          val cc = (priorBits | currentBitsInPosition) & mask
          cc
        }
      priorByte = MaybeInt.Nope
      priorByteBitCount = 0
      output(charCode, out)
      // No more prior byte, and frag byte gets smaller.
      Maybe.Nope
    } else {
      // Not enough bits left period combining prior and frag.
      underflow
    }
  }

  def attemptToDecodeOneYesPriorYesSpaceNoData(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    //
    // Keep in mind the invariant. There is NOT enoug bits in the prior for a character
    //
    Assert.invariant(priorByteBitCount < bitWidthOfACodeUnit)
    underflow
  }

  private def attemptToDecodeOneNoPrior(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    if (YesSpace(out))
      attemptToDecodeOneNoPriorYesSpace(in, out)
    else
      attemptToDecodeOneNoPriorNoSpace(in, out)
  }

  private def attemptToDecodeOneNoPriorYesSpace(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    //
    //////////////////////////////////
    // NoPrior Cases - but YesSpace
    //////////////////////////////////
    //
    if (YesData(in)) {
      //
      // Fresh start or we hit a clean byte boundary
      //
      priorByte = MaybeInt(Bits.asUnsignedByte(in.get()))
      priorByteBitCount = 8
      val currentCharCode = takeCharCodeFromPriorByte()
      output(currentCharCode, out)
      Maybe.Nope
    } else if (YesFrag(in)) {
      // There is a partial frag byte available in the data, and that may provide
      // sufficient bits to decode a character. If enough, decode one & loop
      // reducing the size of the frag.
      //
      val fragLen = maybeBitLimitOffset0b.get
      if (fragLen >= bitWidthOfACodeUnit) {
        val charCode = this.takeCharCodeFromFragByte(in)
        output(charCode, out)
        // frag gets smaller, or disappears.
        Maybe.Nope
      } else {
        // frag is insufficient to create a character
        underflow
      }
    } else if (NoData(in)) {
      underflow
    } else {
      Assert.impossible("All cases are supposed to have been exhausted.")
    }
  }

  private def attemptToDecodeOneNoPriorNoSpace(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    //
    //////////////////////////////////
    // NoPrior, but NoSpace
    //
    // We'll never produce a character here
    // We always return with some CoderResult status.
    //////////////////////////////////
    if (YesData(in)) {
      overflow
    } else if (YesFrag(in)) {
      val fragLen = maybeBitLimitOffset0b.get
      if (fragLen >= bitWidthOfACodeUnit) {
        // if frag has enough for a character, then overflow.
        overflow
      } else {
        underflow
      }
    } else if (NoData(in)) {
      underflow
    } else {
      Assert.impossible("All cases are supposed to have been exhausted.")
    }
  }

  private def attemptToDecodeOneYesPriorNoSpace(in: ByteBuffer, out: CharBuffer): Maybe[CoderResult] = {
    ///////////////////////////////
    // YesPrior but NoSpace Cases
    ///////////////////////////////
    if (YesData(in)) {
      overflow
    } else if (YesFrag(in)) {
      if (priorByteBitCount + this.maybeBitLimitOffset0b.get >= bitWidthOfACodeUnit) {
        overflow
      } else {
        // Not enough bits left combining prior and frag.
        // Note that we prefer underflow to overflow. There's no
        // character to produce here, so we haven't run out of space in the
        // output, we're unable to produce one.
        underflow
      }
    } else if (NoData(in)) {
      underflow
    } else {
      Assert.impossible("All cases are supposed to have been exhausted.")
    }
  }

  def output(charCode: Int, out: CharBuffer)
}

trait NonByteSizeCharsetEncoder
  extends NonByteSizeCharset { self: java.nio.charset.CharsetEncoder =>

  def replacementChar: Int

  private var partialByte: Int = 0
  private var partialByteLenInBits: Int = 0

  protected override def implReset() {
    partialByte = 0
    partialByteLenInBits = 0
  }

  def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult = {
    init
    while (true) {
      val inHasRemainingData = in.hasRemaining()
      val outHasRemainingSpace = out.hasRemaining()

      if (!inHasRemainingData) {
        if (partialByteLenInBits > 0) {
          if (!outHasRemainingSpace) {
            // no remaining input, but there's a partial byte we need to write,
            // and nowhere to write it to
            return CoderResult.OVERFLOW
          } else {
            // no remaining input, but there's a partial byte, write the partial
            // byte (includes padding) and finish
            out.put(partialByte.toByte)
            partialByte = 0
            partialByteLenInBits = 0
            return CoderResult.UNDERFLOW
          }
        } else {
          // no remaining input, and no partial byte, nothing left to
          // encode/write, finish
          return CoderResult.UNDERFLOW
        }
      } else {
        if (!outHasRemainingSpace) {
          // there's input to encode, but nowhere to write it to
          return CoderResult.OVERFLOW
        }

        // there's data to encode and enough space to write a byte, encode the
        // character

        val charCode = charToCharCode(in.get())

        val charCodeToWrite =
          if (charCode.isDefined) {
            charCode.get
          } else {
            // character must fit in the bit width of a code unit, unmappable error
            val unmappableAction = self.unmappableCharacterAction
            if (unmappableAction == CodingErrorAction.REPLACE) {
              // CharsetEncoder, which handles character replacement, assumes
              // that the replacement character is made up of full bytes. That
              // isn't the case for this charset, so we will just manually
              // replace the character ourselves and not let CharsetEncoder
              // know there was ever a problem.
              replacementChar
            } else {
              Assert.invariant(unmappableAction == CodingErrorAction.REPORT)
              // Just report the unmappable character. Note that we should back
              // up the position of the CharBuffer since we read a character
              // but it was invalid. The charset encoder that called encodeLoop
              // will handle skipping the invalid character. Note that the
              // character is only skipped in CodingErrorAction.IGNORE and
              // REPLACE. We handle REPLACE ourselves, and IGNORE isn't
              // supported by DFDL, so this *shouldn't* matter, but it might
              // someday if IGNORE is supported.
              in.position(in.position() - 1)
              return CoderResult.unmappableForLength(1)
            }
          }

        if (partialByteLenInBits == 0) {
          // no partial byte exists, make this the partial byte
          if (requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
            partialByte = charCodeToWrite << (8 - bitWidthOfACodeUnit)
          } else {
            partialByte = charCodeToWrite
          }
          partialByteLenInBits = bitWidthOfACodeUnit
        } else if ((partialByteLenInBits + bitWidthOfACodeUnit) >= 8) {
          // This character's bits will fill up the byte. Some bits might be left over.
          // There's a partial byte, add enough bits to make it a full byte and
          // write it, then save whatever is remaining (could be 0 bits
          // remaining) as a partial byte
          val nUsedBits = 8 - partialByteLenInBits
          if (requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
            val nLeftOverBits = bitWidthOfACodeUnit - nUsedBits
            Assert.invariant(nLeftOverBits >= 0)
            partialByte |= (charCodeToWrite >> nLeftOverBits) & 0xFF
            out.put(partialByte.toByte)
            val leftOverMask = (1 << nLeftOverBits) - 1
            val newPartialByte = (charCodeToWrite & leftOverMask) << (8 - nLeftOverBits)
            partialByte = newPartialByte & 0xFF
            partialByteLenInBits = nLeftOverBits
          } else {
            // LSBF
            partialByte |= (charCodeToWrite << partialByteLenInBits) & 0xFF
            out.put(partialByte.toByte)
            partialByte = charCodeToWrite >> nUsedBits
            partialByteLenInBits = bitWidthOfACodeUnit - nUsedBits
          }
        } else {
          // there's a partial byte but there won't be enough bits to make it a full byte
          if (requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
            partialByte |= (charCodeToWrite << (8 - partialByteLenInBits - bitWidthOfACodeUnit)) & 0xFF
          } else {
            // LSBF
            partialByte |= (charCodeToWrite << partialByteLenInBits) & 0xFF
          }
          partialByteLenInBits = bitWidthOfACodeUnit + partialByteLenInBits
        }

        //
        // Verify invariant that the unused bits of the partialByte are always 0
        //
        Assert.invariant({
          val unusedPartialByteMask: Int =
            if (requiredBitOrder eq BitOrder.MostSignificantBitFirst)
              (1 << (8 - partialByteLenInBits)) - 1
            else
              (-1 << partialByteLenInBits) & 0xFF
          val unusedPartialByteBits = partialByte & unusedPartialByteMask
          unusedPartialByteBits == 0
        })
      }
    }

    Assert.impossible("Incorrect return from encodeLoop")
  }

  protected def charToCharCode(char: Char): MaybeInt
}
