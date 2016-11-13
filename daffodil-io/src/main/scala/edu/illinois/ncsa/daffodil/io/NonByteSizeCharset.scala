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
import java.nio.ByteBuffer
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.util.Bits
import java.nio.charset.CoderResult
import java.nio.charset.CodingErrorAction
import edu.illinois.ncsa.daffodil.util.MaybeInt

/**
 * By "non byte sized" we mean some number of bits less than 8.
 */
trait NonByteSizeCharset {
  def bitWidthOfACodeUnit: Int // in units of bits
  def requiredBitOrder: BitOrder
  protected lazy val leastSigBitMask: Int = 0xFF >> (8 - bitWidthOfACodeUnit)
  protected lazy val mostSigBitMask: Int = 0xFF << (8 - bitWidthOfACodeUnit)
}

/**
 * Mixin for Decoders for Charsets which support initial bit offsets so that
 * their character codepoints need not be byte-aligned.
 */
trait NonByteSizeCharsetDecoder
  extends NonByteSizeCharset { self: java.nio.charset.CharsetDecoder =>

  private var startBitOffset = 0
  private var startBitOffsetHasBeenSet = false
  private var startBitOffsetHasBeenUsed = false
  private var maybeBitLimitOffset0b: MaybeULong = MaybeULong.Nope
  private var priorByte: MaybeInt = MaybeInt.Nope

  protected override def implReset() {
    resetStartBit()
    priorByte = MaybeInt.Nope
    priorByteBitCount = 0
  }

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

    /**
   * When there is a prior byte, this gives the number of bits from it
   * that have not yet been consumed. Zero when there is no prior byte.
   *
   * Value ranges from 1 to 7.
   */
  private var priorByteBitCount = 0

  /////////////////////////////////////////////////////////////////////////
  //
  // The logic for the inner while(true) below used to look something like the
  // following. Although it was much more readable, it caused many Tuple4
  // allocations, which is bad for performance in this inner loop. Because of
  // this, it was replace with conditionals. The logic should be the same.
  //
  // private final val NoData = false
  // private final val YesData = true
  // private final val NoSpace = false
  // private final val YesSpace = true
  // private final val NoPrior = false
  // private final val YesPrior = true
  //
  // while (true) {
  //   (hasPriorByte, priorByteBitCount, in.hasRemaining(), out.hasRemaining()) match {
  //     //
  //     // Fresh start, and also every 56 bits we hit a clean
  //     // byte boundary again
  //     //
  //     case (NoPrior, 0, YesData, YesSpace) => {
  //       val currentByte = Bits.asUnsignedByte(in.get())
  //       // println("no prior byte, current byte = %s".format(asBits(currentByte)))
  //
  //       priorByte = currentByte
  //       priorByteBitCount = 8 - bitWidthOfACodeUnit
  //       hasPriorByte = true
  //
  //       val currentCharCode = currentByte & 0x7F // we take the least significant bits first.
  //
  //       output(currentCharCode)
  //
  //     }
  //     case (NoPrior, 0, NoData, _) => {
  //       // There may have been a partial byte available in the data, and that may provide
  //       // sufficient bits to decode a character.
  //       //
  //       handlePossibleFinalFragmentByte()
  //       return CoderResult.UNDERFLOW
  //     }
  //     case (NoPrior, 0, _, NoSpace) => return CoderResult.OVERFLOW
  //     case (NoPrior, n, YesData, _) => {
  //       // This happens if we're starting the decode loop at a startBitOffset that is non-zero.
  //       // we basically grab one byte and pretend it's the prior byte.
  //       priorByte = Bits.asUnsignedByte(in.get())
  //       hasPriorByte = true
  //     }
  //     case (NoPrior, n, NoData, _) => return CoderResult.UNDERFLOW
  //     case (YesPrior, 0, _, _) => Assert.invariantFailed("priorByteBitCount should not be 0 when there is a prior byte")
  //     case (YesPrior, n, _, _) if (n > 7) => Assert.invariantFailed("priorByteBitCount should be from 1 to 7 when there is a prior byte")
  //     case (YesPrior, 7, _, YesSpace) => {
  //       // Case where we previously used only 1 bit from the prior byte
  //       // so we can produce the next character from the remaining 7 bits of this byte.
  //       // We don't need more input.
  //       // println("prior byte = %s, no current byte needed, priorByteBitCount = %d".format(asBits(priorByte), priorByteBitCount))
  //
  //       val currentByte = priorByte
  //       val currentCharCode = (currentByte & 0xFE) >> 1 // most significant bits for remainder
  //       output(currentCharCode)
  //
  //       hasPriorByte = false
  //       priorByte = 0
  //       priorByteBitCount = 0
  //     }
  //     case (YesPrior, n, NoData, YesSpace) => {
  //       // We have a partial character code in prior byte, but there are no more bytes to be had from the
  //       // ByteBuffer so we can't complete it without more data.
  //       //
  //       // However, there may have been a partial byte available in the data, and that may provide
  //       // sufficient bits to decode a character.
  //       //
  //       handlePossibleFinalFragmentByte()
  //       return CoderResult.UNDERFLOW
  //     }
  //     case (YesPrior, n, YesData, YesSpace) => {
  //       // Straddling bytes. We need another input byte to make up a full character.
  //       val currentByte = Bits.asUnsignedByte(in.get())
  //       handleByte(currentByte)
  //     }
  //     case (_, _, _, NoSpace) => return CoderResult.OVERFLOW
  //   }
  // }
  //
  /////////////////////////////////////////////////////////////////////////

  final def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult = {

    //
    // Now we have to adjust for the starting bit offset
    //
    val bitOffset = getStartBitOffset()
    if (bitOffset != 0) {
      priorByteBitCount = 8 - bitOffset
    }

    while (true) {
      //
      // Fresh start or we hit a clean byte boundary
      //

      val inHasRemainingData = in.hasRemaining()
      val outHasRemainingSpace = out.hasRemaining()

      if (!priorByte.isDefined) {
        if (priorByteBitCount == 0) {
          if (inHasRemainingData && outHasRemainingSpace) {
            priorByte = MaybeInt(Bits.asUnsignedByte(in.get()))
            priorByteBitCount = 8 - bitWidthOfACodeUnit

            val currentCharCode = priorByte.get & leastSigBitMask // we take the least significant bits first.

            output(currentCharCode, out)
          } else if (!inHasRemainingData) {
            // There may have been a partial byte available in the data, and that may provide
            // sufficient bits to decode a character.
            //
            handlePossibleFinalFragmentByte(in, out)

            if(priorByteBitCount < bitWidthOfACodeUnit) {
              // Not enough to read another character
              return CoderResult.UNDERFLOW
            }
          } else if (!outHasRemainingSpace) {
            return CoderResult.OVERFLOW
          }
        } else {
          if (inHasRemainingData) {
            // This happens if we're starting the decode loop at a startBitOffset that is non-zero.
            // we basically grab one byte and pretend it's the prior byte.
            priorByte = MaybeInt(Bits.asUnsignedByte(in.get()))
          } else {
            return CoderResult.UNDERFLOW
          }
        }
      } else {
        if (priorByteBitCount == 0 || priorByteBitCount > 7) Assert.invariantFailed("priorByteBitCount should be from 1 to 7 when there is a prior byte")

        if (priorByteBitCount == bitWidthOfACodeUnit && outHasRemainingSpace) {
          // Case where the remaining bits from the prior byte are enough to produce exactly one character.
          // We don't need more input.
          // println("prior byte = %s, no current byte needed, priorByteBitCount = %d".format(Bits.asBits(priorByte.get), priorByteBitCount))

          val currentCharCode = (priorByte.get & mostSigBitMask) >> (8 - bitWidthOfACodeUnit) // most significant bits for remainder

          output(currentCharCode, out)

          priorByte = MaybeInt.Nope
          priorByteBitCount = 0

        } else if(priorByteBitCount > bitWidthOfACodeUnit && outHasRemainingSpace) {
          // Case where there are enough bits from the prior byte to produce another character,
          // while still having leftover bits. We don't need more input.
          // println("prior byte = %s, no current byte needed, priorByteBitCount = %d".format(Bits.asBits(priorByte.get), priorByteBitCount))

          handleByteWithSufficientBits(priorByte.get, out)

          if(!inHasRemainingData && priorByteBitCount < bitWidthOfACodeUnit) {
            // Not enough to read another character
            return CoderResult.UNDERFLOW
          }

        } else if (outHasRemainingSpace) {
          if (!inHasRemainingData) {
            // We have a partial character code in prior byte, but there are no more bytes to be had from the
            // ByteBuffer so we can't complete it without more data.
            //
            // However, there may have been a partial byte available in the data, and that may provide
            // sufficient bits to decode a character.
            //
            // See test packed5Bit1 for an example that exercises this branch
            //
            // println("prior byte = %s, no bytes remaining, priorByteBitCount = %d".format(Bits.asBits(priorByte.get), priorByteBitCount))
            handlePossibleFinalFragmentByte(in, out)
            if(priorByteBitCount < bitWidthOfACodeUnit) {
              // Not enough to read another character
              return CoderResult.UNDERFLOW
            }
          } else {
            // Straddling bytes. We need another input byte to make up a full character.
            val currentByte = Bits.asUnsignedByte(in.get())
            handleByte(currentByte, out)
          }
        } else {
          return CoderResult.OVERFLOW
        }
      }
    } // end while loop

    Assert.impossible("Incorrect return from decodeLoop.")
  }

  def handlePossibleFinalFragmentByte(in: ByteBuffer, out: CharBuffer) = {
    if (getFinalByteBitLimitOffset0b.isDefined) {
      val bitLimOffset0b = getFinalByteBitLimitOffset0b.get
      if (bitLimOffset0b > 0) {
        // there is a final partial byte (which is beyond the in.remaining()
        Assert.invariant(in.capacity > in.limit())
        val nBitsLeft = priorByteBitCount + bitLimOffset0b
        if (nBitsLeft >= bitWidthOfACodeUnit) {
          // There are enough bits there for another character
          val savedLimit = in.limit()
          in.limit(savedLimit + 1)
          val finalByte = in.get(savedLimit)
          in.limit(savedLimit)
          handleByte(Bits.asUnsignedByte(finalByte), out)
          if (nBitsLeft - bitWidthOfACodeUnit < bitWidthOfACodeUnit) {
            priorByteBitCount = 0
          }
        }
      }
    }
  }

  // This will create a character using any leftover bits from the previous byte
  // plus as many bits as needed from the current byte.
  private def handleByte(currentByte: Int, out: CharBuffer) {
    //
    // This code is specific to bit order least-significant-bit-first
    //
    // println("handleByte: prior byte = %s, current byte = %s, priorByteBitCount = %d".format(Bits.asBits(priorByte.get), Bits.asBits(currentByte), priorByteBitCount))

    val currentByteBitCount = bitWidthOfACodeUnit - priorByteBitCount
    val currentByteMask = 0xFF >> (8 - currentByteBitCount)
    val currentBitsAlone = (currentByte & currentByteMask) << priorByteBitCount
    val currentCharCode = getPriorBitsInPosition | currentBitsAlone
    priorByte = MaybeInt(currentByte)
    priorByteBitCount = 8 - currentByteBitCount
    Assert.invariant(priorByteBitCount > 0)
    Assert.invariant(priorByteBitCount <= 7)
    output(currentCharCode, out)
  }

  // This is used when the currentByte has enough unused bits to create another character.
  private def handleByteWithSufficientBits(currentByte: Int, out: CharBuffer) {
    //
    // This code is specific to bit order least-significant-bit-first
    //
    // println("handleByteWithSufficientBits: current byte = %s, priorByteBitCount = %d".format(Bits.asBits(currentByte), priorByteBitCount))

    val currentByteMask = 0xFF >> (8 - bitWidthOfACodeUnit)
    val currentCharCode = (getPriorBitsInPosition & currentByteMask)
    priorByteBitCount = priorByteBitCount - bitWidthOfACodeUnit
    Assert.invariant(priorByteBitCount > 0)
    Assert.invariant(priorByteBitCount <= 7)
    output(currentCharCode, out)
  }

  // Gets the previously unused bits from prior byte and shifts them so they are the significant bits.
  def getPriorBitsInPosition(): Int = {
    if (priorByte.isDefined) {
      val shift = 8 - priorByteBitCount
      val priorMask = 0xFF << shift
      val priorBits = priorByte.get & priorMask // keeps MSBs we're going to use
      priorBits >> shift
    } else {
      0
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
          partialByte = charCodeToWrite
          partialByteLenInBits = bitWidthOfACodeUnit
        } else if((partialByteLenInBits + bitWidthOfACodeUnit) >= 8) {
          // there's a partial byte, add enough bits to make it a full byte and
          // write it, then save whatever is remaining (could be 0 bits
          // remaining) as a partial byte

          partialByte |= (charCodeToWrite << partialByteLenInBits) & 0xFF
          out.put(partialByte.toByte)

          val usedBits = 8 - partialByteLenInBits
          partialByte = charCodeToWrite >> usedBits
          partialByteLenInBits = bitWidthOfACodeUnit - usedBits

        } else {
          // there's a partial byte but there won't be enough bits to make it a full byte
          partialByte |= (charCodeToWrite << partialByteLenInBits) & 0xFF

          partialByteLenInBits = bitWidthOfACodeUnit + partialByteLenInBits
        }

      }
    }

    Assert.impossible("Incorrect return from encodeLoop")
  }

  protected def charToCharCode(char: Char): MaybeInt
}
