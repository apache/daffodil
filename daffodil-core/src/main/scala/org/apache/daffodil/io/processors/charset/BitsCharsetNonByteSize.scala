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

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CoderResult
import java.nio.charset.CodingErrorAction
import java.nio.charset.{ Charset => JavaCharset }
import java.nio.charset.{ CharsetDecoder => JavaCharsetDecoder }
import java.nio.charset.{ CharsetEncoder => JavaCharsetEncoder }

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.MaybeInt

/**
 * Some encodings are not byte-oriented.
 *
 * If we know the correspondence from integers to characters, and we can express that
 * as a string, then everything else can be derived
 *
 * This class is explicitly not a java.nio.charset.Charset.
 * It is a BitsCharset, which is not a compatible type with a java.nio.charset.Charset
 * on purpose so we don't confuse the two.
 *
 * The problem is that java.nio.charset.Charset is designed in such a way that
 * one cannot implement a proxy class that redirects methods to another class.
 * This is due to all the final methods on the class.
 *
 * So instead we do the opposite. We implement our own BitsCharset API,
 * but implement the behavior in terms of a proxy JavaCharsetDecoder and proxy
 * JavaCharsetEncoder that drive the decodeLoop and encodeLoop. This way
 * we don't have to re-implement all the error handling and flush/end logic.
 */

trait BitsCharsetNonByteSize extends BitsCharset {

  protected def decodeString: String

  def replacementCharCode: Int

  def averageCharsPerByte() = averageCharsPerBit() * 8.0f
  def averageCharsPerBit() = 1.0f / bitWidthOfACodeUnit
  def maxCharsPerByte() = maxCharsPerBit() * 8.0f
  def maxCharsPerBit() = 1.0f / bitWidthOfACodeUnit
  def averageBytesPerChar() = 1 / averageCharsPerByte()
  def averageBitsPerChar() = 1 / averageCharsPerBit()
  def maxBytesPerChar() = 1 / maxCharsPerByte()
  def maxBitsPerChar() = 1 / maxCharsPerBit()

  Assert.usage(decodeString.length == (1 << bitWidthOfACodeUnit))
  Assert.usage(bitWidthOfACodeUnit <= 8 && bitWidthOfACodeUnit >= 1)

  override def newDecoder(): BitsCharsetDecoder = new BitsCharsetNonByteSizeDecoder(this)

  override def newEncoder(): BitsCharsetEncoder =
    new BitsCharsetNonByteSizeEncoder(this, replacementCharCode)

  override def mandatoryBitAlignment = 1

  override def maybeFixedWidth = MaybeInt(bitWidthOfACodeUnit)

  //
  // Note: because this array is only 256, these non-bit width encodings
  // can only support characters in the first 256 unicode characters.
  //
  // The largest such array is 128 (for 7 bits)
  //
  // However, unicode FFFD (replacement character) is accepted, and treated
  // as explicit unmapped. Any other unicode > 256 causes an error.
  //
  private lazy val encodeArray: Array[MaybeInt] = {
    val len = decodeString.length
    val arr = new Array[MaybeInt](256)
    (0 to 255).foreach { index =>
      arr(index) = MaybeInt.Nope
    }
    val imap = decodeString.zip(0 to (len - 1))
    imap.foreach {
      case (c, index) => {
        if (c < 256)
          arr(c) = MaybeInt(index)
        else if (c == '\uFFFD') {
          // ok. unmapped on purpose.
        } else
          Assert.invariantFailed(
            "Char with code %n found. Character codes must be < 256.".format(c.toInt)
          )
      }
    }
    arr
  }

  def codeToChar(code: Int) = {
    decodeString.charAt(code)
  }

  def charToCode(char: Char) =
    if (char < 256) encodeArray(char)
    else MaybeInt.Nope
}

/**
 * Hijack a JavaCharsetEncoder to drive the encodeLoop.
 *
 * This avoids us reimplementing all the error handling and flush/end logic.
 *
 * TODO: Similar to our decoders, we should create custom encoders. Then we
 *  wouldn't need all this complex code related to proxying java charsets.
 */
protected final class ProxyJavaCharsetEncoder(
  cs: JavaCharset,
  real: BitsCharsetNonByteSizeEncoder
) extends JavaCharsetEncoder(cs, 1.0f, 1.0f) {
  override def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult =
    real.encodeLoop(in, out)
  // The default implementation of isLegalReplacement tries to create a decoder
  // and decode the replacement bits and validate that the replace works.
  // However, our decoders are custom and do not implement the Java API, so the
  // default implementation can't decode the bytes. But all of our replacement
  // chars are set by us and cannot change, so we know they are valid. So
  // essentially disable this check so that it does not try to get a decoder.
  override def isLegalReplacement(repl: Array[Byte]): Boolean = true
}

final class BitsCharsetNonByteSizeEncoder(
  override val bitsCharset: BitsCharsetNonByteSize,
  replacementChar: Int
) extends BitsCharsetEncoder {

  final def bitWidthOfACodeUnit: Int = bitsCharset.bitWidthOfACodeUnit
  final def requiredBitOrder: BitOrder = bitsCharset.requiredBitOrder
  def averageCharsPerBit(): Float = bitsCharset.averageCharsPerBit()
  def averageCharsPerByte(): Float = bitsCharset.averageCharsPerByte()
  def maxCharsPerBit(): Float = bitsCharset.maxCharsPerBit()
  def maxCharsPerByte(): Float = bitsCharset.maxCharsPerByte()
  def averageBitsPerChar(): Float = bitsCharset.averageBitsPerChar()
  def averageBytesPerChar(): Float = bitsCharset.averageBytesPerChar()
  def maxBitsPerChar(): Float = bitsCharset.maxBitsPerChar()
  def maxBytesPerChar(): Float = bitsCharset.maxBytesPerChar()

  def isMandatoryAlignmentNeeded(): Boolean = false

  /*
   * We delegate all the normal charset behavior used to customize and control
   * errors of an encoder to the proxy encoder.
   *
   * The proxy encoder calls back to this object to do the encodeLoop.
   */

  /**
   * Hyjack a JavaCharset to drive our code.
   *
   * We want to avoid re-implementing all the error handling and flush/end logic.
   */
  val thisEncoder = this

  protected object ProxyJavaCharset extends JavaCharset("proxyCharset", Array()) {
    override def newEncoder(): JavaCharsetEncoder =
      new ProxyJavaCharsetEncoder(this, thisEncoder)
    override def newDecoder(): JavaCharsetDecoder =
      Assert.usageError("newDecoder method not to be called on " + this)
    override def contains(jcs: JavaCharset): Boolean =
      Assert.usageError("contains method not to be called on " + this)
  }

  private lazy val proxy = new ProxyJavaCharsetEncoder(ProxyJavaCharset, this)

  def malformedInputAction(): CodingErrorAction = proxy.malformedInputAction()
  def unmappableCharacterAction(): CodingErrorAction = proxy.unmappableCharacterAction()
  def onMalformedInput(action: CodingErrorAction) = { proxy.onMalformedInput(action); this }
  def onUnmappableCharacter(action: CodingErrorAction) = {
    proxy.onUnmappableCharacter(action); this
  }
  def replacement() = proxy.replacement()
  def replaceWith(newReplacement: Array[Byte]) = { proxy.replaceWith(newReplacement); this }

  def encode(in: CharBuffer, out: ByteBuffer, endOfInput: Boolean) =
    proxy.encode(in, out, endOfInput)
  def flush(out: ByteBuffer) = proxy.flush(out)

  private var partialByte: Int = 0
  private var partialByteLenInBits: Int = 0

  def reset() = {
    partialByte = 0
    partialByteLenInBits = 0
    proxy.reset()
    isReset = true
    this
  }

  def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult = {
    isReset = false
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
            val unmappableAction = unmappableCharacterAction()
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
            partialByte |= (charCodeToWrite >> nLeftOverBits) & 0xff
            out.put(partialByte.toByte)
            val leftOverMask = (1 << nLeftOverBits) - 1
            val newPartialByte = (charCodeToWrite & leftOverMask) << (8 - nLeftOverBits)
            partialByte = newPartialByte & 0xff
            partialByteLenInBits = nLeftOverBits
          } else {
            // LSBF
            partialByte |= (charCodeToWrite << partialByteLenInBits) & 0xff
            out.put(partialByte.toByte)
            partialByte = charCodeToWrite >> nUsedBits
            partialByteLenInBits = bitWidthOfACodeUnit - nUsedBits
          }
        } else {
          // there's a partial byte but there won't be enough bits to make it a full byte
          if (requiredBitOrder eq BitOrder.MostSignificantBitFirst) {
            partialByte |= (charCodeToWrite << (8 - partialByteLenInBits - bitWidthOfACodeUnit)) & 0xff
          } else {
            // LSBF
            partialByte |= (charCodeToWrite << partialByteLenInBits) & 0xff
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
              (-1 << partialByteLenInBits) & 0xff
          val unusedPartialByteBits = partialByte & unusedPartialByteMask
          unusedPartialByteBits == 0
        })
      }
    }

    Assert.impossible("Incorrect return from encodeLoop")
  }

  def charToCharCode(char: Char): MaybeInt = {
    bitsCharset.charToCode(char)
  }
}
