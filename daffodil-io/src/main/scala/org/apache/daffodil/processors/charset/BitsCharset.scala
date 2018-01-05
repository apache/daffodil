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
package edu.illinois.ncsa.daffodil.processors.charset

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.CoderResult
import java.nio.charset.CodingErrorAction
import java.nio.charset.{ CharsetEncoder => JavaCharsetEncoder }
import java.nio.charset.{ CharsetDecoder => JavaCharsetDecoder }
import java.nio.charset.{ StandardCharsets => JavaStandardCharsets }
import java.nio.charset.{ Charset => JavaCharset }
import java.nio.CharBuffer
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.MaybeInt

/**
 * Charset enhanced with features allowing it to work with Daffodil's Bit-wise
 * DataInputStream and DataOutputStream.
 *
 * Daffodil uses BitsCharset as its primary abstraction for dealing with
 * character sets, which enables it to support character sets where the code
 * units are smaller than 1 byte.
 *
 * Note that BitsCharset is NOT derived from java.nio.charset.Charset, nor
 * are BitsCharsetDecoder or BitsCharsetEncoder derived from
 * java.nio.charset.CharsetDecoder or CharsetEncoder respectively. This is because
 * these Java classes have many final methods that make it impossible for us
 * to implement what we need by extending them. Hence, a BitsCharset is its own
 * interface that has some similarities to the Java charset-related classes.
 *
 * For regular byte-centric charsets a BitsCharset is layered on top of
 * Java's java.nio.charset.Charset.
 */
trait BitsCharset extends Serializable {
  final override def hashCode = name.hashCode
  final override def equals(other: Any) = other match {
    case bcs: BitsCharset => this.name == bcs.name
    case _ => false
  }
  def name: String
  def bitWidthOfACodeUnit: Int // in units of bits
  def requiredBitOrder: BitOrder
  def mandatoryBitAlignment: Int
  def newDecoder(): BitsCharsetDecoder
  def newEncoder(): BitsCharsetEncoder

  def maybeFixedWidth: MaybeInt

  final def padCharWidthInBits = {
    if (maybeFixedWidth.isDefined)
      maybeFixedWidth.get
    else {
      this match {
        case StandardBitsCharsets.UTF_8 => 8
        case _ => Assert.invariantFailed("Getting pad char width for unsupported variable-width charset: " + name)
      }
    }
  }
}

trait IsResetMixin {
  private final var isReset_ : Boolean = true
  /**
   * True if the decoder has not decoded anything since the last reset call.
   * False if decodeLoop has been called.
   *
   * Use to control things that we only want to check once per reset of the
   * decoder.
   */
  final def isReset = isReset_
  /**
   * Allow assignment to isReset only in derived classes
   */
  protected final def isReset_=(v: Boolean): Unit = isReset_ = v
}

trait BitsCharsetDecoder
  extends IsResetMixin {
  def bitsCharset: BitsCharset
  def setInitialBitOffset(offset: Int): Unit
  def setFinalByteBitLimitOffset0b(bitLimitOffset0b: MaybeULong): Unit
  def averageCharsPerByte(): Float
  def maxCharsPerByte(): Float
  def averageCharsPerBit(): Float
  def maxCharsPerBit(): Float
  def replacement(): String
  def replaceWith(newReplacement: String): BitsCharsetDecoder
  def flush(out: CharBuffer): CoderResult
  def reset(): BitsCharsetDecoder

  /**
   * Used to determine if the data input stream must be aligned (if not already)
   * for this encoding. Based on whether the coder has been reset. If the
   * coder has not been reset, it is assumed we are in the middle of decoding
   * many characters, and so no mandatory alignment is needed. However, if the
   * coder was reset, then it is assumed that we may be unaligned at the start
   * of decoding characters, and so we must check if we are mandatory aligned.
   */
  def isMandatoryAlignmentNeeded(): Boolean
  def malformedInputAction(): CodingErrorAction
  def onMalformedInput(action: CodingErrorAction): BitsCharsetDecoder
  def unmappableCharacterAction(): CodingErrorAction
  def onUnmappableCharacter(action: CodingErrorAction): BitsCharsetDecoder
  def decode(in: ByteBuffer, out: CharBuffer, endOfInput: Boolean): CoderResult
  protected def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult

  final def decode(in: ByteBuffer): CharBuffer = {
    var n = scala.math.ceil(in.remaining() * averageCharsPerByte()).toInt
    var out = CharBuffer.allocate(n)

    if ((n == 0) && (in.remaining() == 0)) out
    else {
      reset()
      var break = false
      while (!break) {
        var cr =
          if (in.hasRemaining())
            decode(in, out, true)
          else
            CoderResult.UNDERFLOW
        if (cr.isUnderflow())
          cr = flush(out)
        if (cr.isUnderflow())
          break = true
        else if (cr.isOverflow()) {
          n = 2 * n + 1; // Ensure progress; n might be 0!
          val o = CharBuffer.allocate(n)
          out.flip()
          o.put(out)
          out = o
        } else
          cr.throwException()
      }
      out.flip()
      out
    }
  }
}

abstract class BitsCharsetEncoder
  extends IsResetMixin {
  def bitsCharset: BitsCharset
  def averageBytesPerChar(): Float
  def maxBytesPerChar(): Float
  def averageBitsPerChar(): Float
  def maxBitsPerChar(): Float
  def replacement(): Array[Byte]
  def replaceWith(newReplacement: Array[Byte]): BitsCharsetEncoder
  def flush(out: ByteBuffer): CoderResult
  def reset(): BitsCharsetEncoder

  /**
   * Used to determine if the data input stream must be aligned (if not already)
   * for this encoding. Based on whether the coder has been reset. If the
   * coder has not been reset, it is assumed we are in the middle of encoding
   * many characters, and so no mandatory alignment is needed. However, if the
   * coder was reset, then it is assumed that we may be unaligned at the start
   * of encoding characters, and so we must check if we are mandatory aligned.
   */
  def isMandatoryAlignmentNeeded(): Boolean
  def malformedInputAction(): CodingErrorAction
  def onMalformedInput(action: CodingErrorAction): BitsCharsetEncoder
  def unmappableCharacterAction(): CodingErrorAction
  def onUnmappableCharacter(action: CodingErrorAction): BitsCharsetEncoder
  def encode(in: CharBuffer, out: ByteBuffer, endOfInput: Boolean): CoderResult
  protected def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult
}

/**
 * Implements BitsCharset based on encapsulation of a regular JavaCharset.
 */
final class BitsCharsetWrappingJavaCharset(nameArg: String)
  extends BitsCharset {

  javaCharset // Force javaCharset to be evaluated to ensure it's valid at compile time.
  // It's a lazy val so it will be evaluated when de-serialized
  @transient lazy val name = javaCharset.name
  @transient lazy val javaCharset = JavaCharset.forName(nameArg)
  @transient final override lazy val maybeFixedWidth = CharsetUtils.maybeEncodingFixedWidth(this)

  override def newDecoder() = new BitsCharsetWrappingJavaCharsetDecoder(this, javaCharset.newDecoder())
  override def newEncoder() = new BitsCharsetWrappingJavaCharsetEncoder(this, javaCharset.newEncoder())

  override def bitWidthOfACodeUnit: Int = 8
  override def requiredBitOrder = BitOrder.MostSignificantBitFirst // really none, as these are mandatory aligned to byte boundary.
  override def mandatoryBitAlignment = 8

}

/**
 * Implements BitsCharsetDecoder by encapsulation of a standard
 * JavaCharsetDecoder.
 */
final class BitsCharsetWrappingJavaCharsetDecoder(override val bitsCharset: BitsCharsetWrappingJavaCharset, dec: JavaCharsetDecoder)
  extends BitsCharsetDecoder {

  def setInitialBitOffset(offset: Int): Unit = Assert.usageError("Not to be called.")
  def setFinalByteBitLimitOffset0b(bitLimitOffset0b: MaybeULong): Unit = Assert.usageError("Not to be called.")

  def averageCharsPerByte() = dec.averageCharsPerByte()
  def averageCharsPerBit() = averageCharsPerByte() / 8.0F
  def maxCharsPerByte() = dec.maxCharsPerByte()
  def maxCharsPerBit() = maxCharsPerByte() / 8.0F
  def replacement() = dec.replacement()
  def replaceWith(newReplacement: String) = {
    Assert.usage(isReset)
    dec.replaceWith(newReplacement); this
  }
  def flush(out: CharBuffer) = {
    Assert.usage(!isReset)
    dec.flush(out)
  }
  def reset() = {
    dec.reset();
    isReset = true
    this
  }
  def isMandatoryAlignmentNeeded() = isReset

  def malformedInputAction() = dec.malformedInputAction()
  def onMalformedInput(action: CodingErrorAction) = {
    Assert.usage(isReset)
    dec.onMalformedInput(action);
    this
  }
  def unmappableCharacterAction() = dec.malformedInputAction()
  def onUnmappableCharacter(action: CodingErrorAction) = {
    Assert.usage(isReset)
    dec.onUnmappableCharacter(action);
    this
  }
  def decode(in: ByteBuffer, out: CharBuffer, endOfInput: Boolean) = {
    isReset = false
    dec.decode(in, out, endOfInput)
  }
  protected def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult =
    Assert.usageError("Not to be called.")

}

/**
 * Implements BitsCharsetEncoder by encapsulating a standard JavaCharsetEncoder
 */
final class BitsCharsetWrappingJavaCharsetEncoder(override val bitsCharset: BitsCharsetWrappingJavaCharset, enc: JavaCharsetEncoder)
  extends BitsCharsetEncoder {

  def setInitialBitOffset(offset: Int): Unit = Assert.usageError("Not to be called.")
  def averageBytesPerChar() = enc.averageBytesPerChar()
  def averageBitsPerChar() = averageBytesPerChar() / 8.0F
  def maxBytesPerChar() = enc.maxBytesPerChar()
  def maxBitsPerChar() = maxBytesPerChar() / 8.0F
  def replacement() = enc.replacement()
  def replaceWith(newReplacement: Array[Byte]) = {
    Assert.usage(isReset)
    enc.replaceWith(newReplacement);
    this
  }
  def flush(out: ByteBuffer) = {
    Assert.usage(!isReset)
    enc.flush(out)
  }
  def reset() = {
    enc.reset();
    isReset = true
    this
  }
  def isMandatoryAlignmentNeeded = isReset
  def malformedInputAction() = enc.malformedInputAction()
  def onMalformedInput(action: CodingErrorAction) = {
    Assert.usage(isReset)
    enc.onMalformedInput(action);
    this
  }
  def unmappableCharacterAction() = enc.malformedInputAction()
  def onUnmappableCharacter(action: CodingErrorAction) = {
    Assert.usage(isReset)
    enc.onUnmappableCharacter(action);
    this
  }
  def encode(in: CharBuffer, out: ByteBuffer, endOfInput: Boolean) = {
    isReset = false
    enc.encode(in, out, endOfInput)
  }
  protected def encodeLoop(in: CharBuffer, out: ByteBuffer) =
    Assert.usageError("Not to be called")
}

/**
 * Provides BitsCharset objects corresponding to the usual java charsets found
 * in StandardCharsets.
 */
object StandardBitsCharsets {
  val UTF_8 = new BitsCharsetWrappingJavaCharset(JavaStandardCharsets.UTF_8.name)
  val UTF_16BE = new BitsCharsetWrappingJavaCharset(JavaStandardCharsets.UTF_16BE.name)
  val UTF_16LE = new BitsCharsetWrappingJavaCharset(JavaStandardCharsets.UTF_16LE.name)
  val ISO_8859_1 = new BitsCharsetWrappingJavaCharset(JavaStandardCharsets.ISO_8859_1.name)
}
