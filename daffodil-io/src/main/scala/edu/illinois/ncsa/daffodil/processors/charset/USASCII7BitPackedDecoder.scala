/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

import java.nio.charset.{ Charset, CoderResult, CharsetDecoder, CharsetEncoder }
import java.nio.ByteBuffer
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Bits
import scala.language.postfixOps
import edu.illinois.ncsa.daffodil.io.NonByteSizeCharsetEncoderDecoder
import edu.illinois.ncsa.daffodil.util.MaybeULong

/**
 * Some encodings are not byte-oriented.
 *
 * X-DFDL-US-ASCII-7-BIT-PACKED occupies only 7 bits with each
 * code unit.
 *
 * There are 6 bit and 5 bit encodings in use as well. (One can even think of hexadecimal as
 * a 4-bit encoding of 16 possible characters - might be a cool way to
 * implement packed decimals of various sorts.)
 */

trait USASCII7BitPackedEncoderDecoderMixin
  extends NonByteSizeCharsetEncoderDecoderImpl {

  val bitWidthOfACodeUnit = 7 // in units of bits

  /**
   * Returns a string of '1' and '0' characters for a single unsigned byte.
   *
   * This is for debugging
   */
  def asBits(unsignedByte: Int): String = {
    val hex = "%02x".format(unsignedByte)
    Misc.hex2Bits(hex)
  }
}

/**
 * Mixin for Charsets which support initial bit offsets so that
 * their character codepoints need not be byte-aligned.
 */
trait NonByteSizeCharsetEncoderDecoderImpl
  extends NonByteSizeCharsetEncoderDecoder {

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

object USASCII7BitPackedCharset
  extends java.nio.charset.Charset("X-DFDL-US-ASCII-7-BIT-PACKED", Array("US-ASCII-7-BIT-PACKED"))
  with USASCII7BitPackedEncoderDecoderMixin {

  def contains(cs: Charset): Boolean = false

  def newDecoder(): CharsetDecoder = new USASCII7BitPackedDecoder

  def newEncoder(): CharsetEncoder = new USASCII7BitPackedEncoder

  private[charset] def charsPerByte = 8.0F / 7.0F
  private[charset] def bytesPerChar = 1.0F // can't use 7/8 here because CharsetEncoder base class requires it to be 1 or greater.
}

/**
 * You have to initialize one of these for a specific ByteBuffer because
 * the encoding is 7-bits wide, so we need additional state beyond just
 * the byte position and limit that a ByteBuffer provides in order to
 * properly sequence through the data.
 */
class USASCII7BitPackedDecoder
  extends java.nio.charset.CharsetDecoder(USASCII7BitPackedCharset,
    USASCII7BitPackedCharset.charsPerByte, // average
    USASCII7BitPackedCharset.charsPerByte) // maximum
  with USASCII7BitPackedEncoderDecoderMixin {

  override def implReset() {
    // println("Reset")
    resetStartBit()
    hasPriorByte = false
    priorByte = 0
    priorByteBitCount = 0
  }

  private var priorByte = 0
  private var hasPriorByte = false

  /**
   * When there is a prior byte, this gives the number of bits from it
   * that have not yet been consumed. Zero when there is no prior byte.
   *
   * Value ranges from 1 to 7. Can never be 8.
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
    val bitOffset0to7 = getStartBitOffset()
    if (bitOffset0to7 != 0) {
      priorByteBitCount = 8 - bitOffset0to7
    }

    while (true) {
      //
      // Fresh start, and also every 56 bits we hit a clean
      // byte boundary again
      //

      val inHasRemainingData = in.hasRemaining()
      val outHasRemainingSpace = out.hasRemaining()

      if (!hasPriorByte) {
        if (priorByteBitCount == 0) {
          if (inHasRemainingData && outHasRemainingSpace) {
            priorByte = Bits.asUnsignedByte(in.get())
            priorByteBitCount = 8 - bitWidthOfACodeUnit
            hasPriorByte = true

            val currentCharCode = priorByte & 0x7F // we take the least significant bits first.

            output(currentCharCode, out)
          } else if (!inHasRemainingData) {
            // There may have been a partial byte available in the data, and that may provide
            // sufficient bits to decode a character.
            //
            handlePossibleFinalFragmentByte(in, out)
            return CoderResult.UNDERFLOW
          } else if (!outHasRemainingSpace) {
            return CoderResult.OVERFLOW
          }
        } else {
          if (inHasRemainingData) {
            // This happens if we're starting the decode loop at a startBitOffset that is non-zero.
            // we basically grab one byte and pretend it's the prior byte.
            priorByte = Bits.asUnsignedByte(in.get())
            hasPriorByte = true
          } else {
            return CoderResult.UNDERFLOW
          }
        }
      } else {
        if (priorByteBitCount == 0 || priorByteBitCount > 7) Assert.invariantFailed("priorByteBitCount should be from 1 to 7 when there is a prior byte")

        if (priorByteBitCount == 7 && outHasRemainingSpace) {
          // Case where we previously used only 1 bit from the prior byte
          // so we can produce the next character from the remaining 7 bits of this byte.
          // We don't need more input.
          // println("prior byte = %s, no current byte needed, priorByteBitCount = %d".format(asBits(priorByte), priorByteBitCount))

          val currentCharCode = (priorByte & 0xFE) >> 1 // most significant bits for remainder
          output(currentCharCode, out)

          hasPriorByte = false
          priorByte = 0
          priorByteBitCount = 0
        } else if (outHasRemainingSpace) {
          if (!inHasRemainingData) {
            // We have a partial character code in prior byte, but there are no more bytes to be had from the
            // ByteBuffer so we can't complete it without more data.
            //
            // However, there may have been a partial byte available in the data, and that may provide
            // sufficient bits to decode a character.
            //
            handlePossibleFinalFragmentByte(in, out)
            return CoderResult.UNDERFLOW
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

  private def output(charCode: Int, out: CharBuffer) {
    // println("charcode = %2x".format(charCode))
    val char = charCode.toChar
    out.put(char)
  }

  private def handlePossibleFinalFragmentByte(in: ByteBuffer, out: CharBuffer) = {
    if (getFinalByteBitLimitOffset0b.isDefined) {
      val bitLimOffset0b = getFinalByteBitLimitOffset0b.get
      if (bitLimOffset0b > 0) {
        // there is a final partial byte (which is beyond the in.remaining()
        Assert.invariant(in.capacity > in.limit())
        if (priorByteBitCount + bitLimOffset0b >= bitWidthOfACodeUnit) {
          // There are enough bits there for another character
          val savedLimit = in.limit()
          in.limit(savedLimit + 1)
          val finalByte = in.get(savedLimit)
          in.limit(savedLimit)
          handleByte(Bits.asUnsignedByte(finalByte), out)
          // That must be the last byte, so fall through to the return Underflow.
        }
      }
    }
  }

  private def handleByte(currentByte: Int, out: CharBuffer) {
    //
    // This code is specific to bit order least-significant-bit-first
    //
    // println("prior byte = %s, current byte = %s, priorByteBitCount = %d".format(asBits(priorByte), asBits(currentByte), priorByteBitCount))
    val priorBitsInPosition =
      if (hasPriorByte) {
        val shift = 8 - priorByteBitCount
        val priorMask = 0xFF << shift
        val priorBits = priorByte & priorMask // keeps MSBs we're going to use
        priorBits >> shift
      } else {
        0
      }
    val currentByteBitCount = bitWidthOfACodeUnit - priorByteBitCount
    val currentByteMask = 0xFF >> (8 - currentByteBitCount)
    val currentBitsAlone = (currentByte & currentByteMask) << priorByteBitCount
    val currentCharCode = priorBitsInPosition | currentBitsAlone
    priorByte = currentByte
    hasPriorByte = true
    priorByteBitCount = 8 - currentByteBitCount
    Assert.invariant(priorByteBitCount > 0)
    Assert.invariant(priorByteBitCount <= 7)
    output(currentCharCode, out)
  }

}

class USASCII7BitPackedEncoder
  extends java.nio.charset.CharsetEncoder(USASCII7BitPackedCharset,
    USASCII7BitPackedCharset.bytesPerChar, // average
    USASCII7BitPackedCharset.bytesPerChar) // maximum
  with USASCII7BitPackedEncoderDecoderMixin {

  // TODO: make this efficient. Right now it is inflating things to
  // strings of "0" and "1". However, the only use is TDML currently.
  def encodeLoop(cb: CharBuffer, bb: ByteBuffer): CoderResult = {
    val bits =
      if (cb.length == 0) Seq.empty
      else {
        val charsAsBits = (1 to cb.length).map { x =>
          val charCode = cb.get()
          val all8Bits = asBits(charCode.toInt)
          val just7Bits = all8Bits.slice(1, 8)
          just7Bits
        }
        charsAsBits
      }
    Assert.invariant(!cb.hasRemaining)
    val len = bits.map { _.length } sum
    val padBits = if ((len % 8) == 0) "" else "0" * (8 - (len % 8))
    //
    // This is specific to bitOrder leastSignificantBitFirst
    //
    val bitsAsFullBytes = (bits.map { _.reverse }.mkString + padBits).reverse
    //
    val bytes = bitsAsFullBytes.sliding(8, 8).map { Integer.parseInt(_, 2).toByte }.toArray.reverse
    bb.put(bytes)
    CoderResult.UNDERFLOW
  }
}
