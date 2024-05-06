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

package org.apache.daffodil.io.processors.charset

import java.nio.CharBuffer
import java.nio.LongBuffer

import org.apache.daffodil.io.DataInputStream.NotEnoughDataException
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThinException
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.util.MaybeChar

class BitsCharsetDecoderMalformedException(val malformedBits: Int) extends ThinException

class BitsCharsetDecoderUnalignedCharDecodeException(val bitPos1b: Long) extends ThinException {
  def bitAlignment1b = bitPos1b % 8
  def bytePos1b = ((bitPos1b - 1) / 8) + 1

  override def getMessage(): String = {
    s"Charset not byte aligned. bitAlignment1b=${bitAlignment1b}, bitPos1b=${bitPos1b}, bytePos1b=${bytePos1b}."
  }
}

trait BitsCharsetDecoderState

abstract class BitsCharsetDecoder {

  /**
   * Decode a single character
   *
   * This should read data via the InputSourceDataInputStream in whatever manner is most
   * efficient, as long as at the end of the decode the bitPosition0b is set to
   * to end of the character.
   *
   * If there was a decode error, the bit position should be set to the end of
   * malformed bits and a BitsCharsetDecoderMalformedException should be thrown
   * specifying how many bits were malformed.
   */
  protected def decodeOneChar(dis: InputSourceDataInputStream, finfo: FormatInfo): Char

  /**
   * Decode multiple characters into a CharBuffer, keeping track of the
   * bit positions after each Char decode
   *
   * Decodes at most chars.remaining() characters in the chars CharBuffer. If
   * bitPositions is provided, for each decoded character the bitPosition0b
   * where the character decode operation finished is stored in the
   * bitPositions LongBuffer. Upon return of the decode operation, the
   * bitPosition0b of the InputSourceDataInputStream will be the end of the last
   * successful character decode operation. Returns the number of successfully
   * decode characters.
   */
  final def decode(
    dis: InputSourceDataInputStream,
    finfo: FormatInfo,
    chars: CharBuffer,
    bitPositions: LongBuffer = null
  ): Int = {
    Assert.invariant(bitPositions == null || (chars.remaining <= bitPositions.remaining))

    var keepDecoding = true
    val charsToDecode = chars.remaining
    var numDecoded = 0

    while (keepDecoding && numDecoded < charsToDecode) {
      val maybeChar = decodeOneHandleMalformed(dis, finfo)
      if (maybeChar.isDefined) {
        chars.put(maybeChar.get)
        if (bitPositions != null) {
          bitPositions.put(dis.bitPos0b)
        }
        numDecoded += 1
      } else {
        keepDecoding = false
      }
    }
    numDecoded
  }

  /**
   * Attempts to decode a single char, handling error encoding policy
   */
  @inline private def decodeOneHandleMalformed(
    dis: InputSourceDataInputStream,
    finfo: FormatInfo
  ): MaybeChar = {
    try {
      val c = decodeOneChar(dis, finfo)
      MaybeChar(c)
    } catch {
      case e: BitsCharsetDecoderMalformedException => {
        if (e.malformedBits == 0) {
          // ran out of data, return nothing and end the decode
          MaybeChar.Nope
        } else {
          finfo.encodingErrorPolicy match {
            case EncodingErrorPolicy.Replace => {
              MaybeChar(0xfffd.toChar)
            }
            case EncodingErrorPolicy.Error => {
              // back up to before the malformed data occurred
              // dis.setBitPosition(dis.bitPos0b - e.malformedBits)
              // MaybeChar.Nope // TODO: should this rethrow the exception instead? So callers of decode must determine how to handle decode errors
              Assert.nyi("dfdl:encodingErrorPolicy=\"error\"")
            }
          }
        }
      }
    }
  }

  def reset(): Unit
}

/**
 * Base class for byte based decoders
 *
 * Provides methods to get a single byte. Also
 * handles logic related to error encoding policy and the replacement
 * characters. Implementing class only need to use the provided methods to get
 * a byte(s) and convert to a char and perform validation on the code point.
 */
abstract class BitsCharsetDecoderByteSize extends BitsCharsetDecoder {

  // gets the next byte from the data, returns an int in the range 0 to 255
  @inline protected final def getByte(
    dis: InputSourceDataInputStream,
    bitsConsumedSoFar: Int
  ): Int = {
    if (!dis.isDefinedForLength(8)) {
      throw new BitsCharsetDecoderMalformedException(bitsConsumedSoFar)
    }
    if (!dis.isAligned(8)) {
      throw new BitsCharsetDecoderUnalignedCharDecodeException(dis.bitPos1b)
    }
    // read directly from the input source. This should be faster, but makes
    // assumptions that data is aligned. This should always succeed due to
    // the above check
    val byte = dis.inputSource.get()
    // need to update the bitPosition since the inputsource maintains its own
    // position
    dis.setBitPos0b(dis.bitPos0b + 8)
    byte
  }

  override def reset(): Unit = {
    // do nothing
  }
}

/**
 * Some encodings need state, but only for the storing of a low surrogate
 * pair. This encapsulates that logic. When a class extends this class, it ust
 * implement deocodeOneUnicodeChar, which should decode one char, and if there
 * is a high/low surrogate pair it should call setLowSurrgoate on the low and
 * return the high.
 */
abstract class BitsCharsetDecoderCreatesSurrogates extends BitsCharsetDecoderByteSize {

  class BitsCharsetDecoderSurrogateState(
    var lowSurrogate: MaybeChar = MaybeChar.Nope
  ) extends BitsCharsetDecoderState

  protected var state = new BitsCharsetDecoderSurrogateState()

  final protected override def decodeOneChar(
    dis: InputSourceDataInputStream,
    finfo: FormatInfo
  ): Char = {
    if (state.lowSurrogate.isDefined) {
      val low = state.lowSurrogate.get
      state.lowSurrogate = MaybeChar.Nope
      low
    } else {
      decodeOneUnicodeChar(dis, finfo)
    }
  }

  protected def decodeOneUnicodeChar(dis: InputSourceDataInputStream, finfo: FormatInfo): Char

  protected def setLowSurrogate(low: Char): Unit = state.lowSurrogate = MaybeChar(low)

  override def reset(): Unit = state.lowSurrogate = MaybeChar.Nope
}

final class BitsCharsetNonByteSizeDecoder(charset: BitsCharsetNonByteSize)
  extends BitsCharsetDecoder {

  protected def decodeOneChar(dis: InputSourceDataInputStream, finfo: FormatInfo): Char = {
    val code =
      try {
        dis.getUnsignedLong(charset.bitWidthOfACodeUnit, finfo).toInt
      } catch {
        case e: NotEnoughDataException => throw new BitsCharsetDecoderMalformedException(0)
      }
    charset.codeToChar(code)
  }

  override def reset(): Unit = {
    // do nothing
  }
}
