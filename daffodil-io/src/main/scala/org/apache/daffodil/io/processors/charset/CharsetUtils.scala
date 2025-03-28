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
import java.nio.charset.StandardCharsets

import org.apache.daffodil.io.LocalBufferMixin
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MaybeInt

object CharsetUtils {

  /**
   * Call instead of Charset.forName to obtain Daffodil's less-than-byte-sized
   * encodings as well as the standard ones. This will return the charset if it exists or null
   */
  def getCharset(name: String): BitsCharset = {
    val cs = BitsCharsetDefinitionRegistry.find(name.toUpperCase).getOrElse(null)
    if (cs == null)
      null
    else
      cs.charset
  }

  def supportedEncodingsString = BitsCharsetDefinitionRegistry.supportedEncodingsString

  /**
   * Subtle bug in decoders in Java 7 when there is room for only 1
   * character in the CharBuffer.
   *
   * While we could just test for Java 8, which doesn't have this bug,
   * it is worthwhile to keep this in case we end up trying to support
   * Java 7 at some point in the future.
   */
  lazy val hasJava7DecoderBug = {
    val decoder = StandardCharsets.UTF_8.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val bb = ByteBuffer.allocate(6)
    bb.put(-16.toByte) // invalid first utf-8 byte
    bb.limit(6).position(0)
    val cb = CharBuffer.allocate(1)
    val cr = decoder.decode(bb, cb, true)
    if (
      cr.isOverflow && // This is the bug!
      cb.position() == 0 &&
      bb.position() == 0
    ) true
    else if (cr.isError) false // no bug
    //    else if (cr.isOverflow && // This is what *should* happen if CodingErrorAction.REPLACE is used.
    //      cb.position == 1 &&
    //      bb.position == 1 &&
    //      cb.get(0) == this.unicodeReplacementChar) false
    else
      Assert.invariantFailed("Unexpected decoder behavior. " + cr)
  }

  val unicodeReplacementChar = '\uFFFD'
}

sealed abstract class CoderInfo(
  val encodingMandatoryAlignmentInBits: Int,
  val maybeCharWidthInBits: MaybeInt
)

case class DecoderInfo(
  coder: BitsCharsetDecoder,
  encodingMandatoryAlignmentInBitsArg: Int,
  maybeCharWidthInBitsArg: MaybeInt
) extends CoderInfo(encodingMandatoryAlignmentInBitsArg, maybeCharWidthInBitsArg)

case class EncoderInfo(
  coder: BitsCharsetEncoder,
  replacingCoder: BitsCharsetEncoder,
  reportingCoder: BitsCharsetEncoder,
  encodingMandatoryAlignmentInBitsArg: Int,
  maybeCharWidthInBitsArg: MaybeInt
) extends CoderInfo(encodingMandatoryAlignmentInBitsArg, maybeCharWidthInBitsArg)

trait EncoderDecoderMixin extends LocalBufferMixin {

  /**
   * The reason for these caches is that otherwise to
   * get a decoder you have to take the Charset and call
   * newDecoder which allocates. We always want the same one for a given
   * thread using Daffodil to parse/unparse with a particular PState/UState.
   *
   * Using java.util.HashMap because scala hash maps return option types, which
   * might be allocated objects. Use of java hash maps insures this does not allocate
   * except when adding a new not-seen-before encoder or decoder.
   */

  //
  // TODO: Cleanup - change below to use NonAllocatingMap to improve code style.
  // And fix comment above.
  //
  private lazy val decoderCache = new java.util.HashMap[BitsCharset, DecoderInfo]
  private lazy val encoderCache = new java.util.HashMap[BitsCharset, EncoderInfo]

  private def derivations(charset: BitsCharset) = {
    val tuple = charset match {
      case nbsc: BitsCharsetNonByteSize => {
        val encodingMandatoryAlignmentInBits = 1
        val maybeCharWidthInBits = MaybeInt(nbsc.bitWidthOfACodeUnit)
        (encodingMandatoryAlignmentInBits, maybeCharWidthInBits)
      }
      case _ => {
        val encodingMandatoryAlignmentInBits = 8
        val encoder = charset.newEncoder()
        val maxBytes = encoder.maxBytesPerChar()
        if (maxBytes == encoder.averageBytesPerChar()) {
          val maybeCharWidthInBits = MaybeInt((maxBytes * 8).toInt)
          (encodingMandatoryAlignmentInBits, maybeCharWidthInBits)
        } else {
          val maybeCharWidthInBits = MaybeInt.Nope
          (encodingMandatoryAlignmentInBits, maybeCharWidthInBits)
        }
      }
    }
    tuple
  }

  def getDecoder(charset: BitsCharset) = getDecoderInfo(charset).coder
  def getDecoderInfo(charset: BitsCharset) = {
    // threadCheck()
    var entry = decoderCache.get(charset)
    if (entry eq null) {
      val coder = charset.newDecoder()
      val (encodingMandatoryAlignmentInBits, maybeCharWidthInBits) = derivations(charset)
      entry = DecoderInfo(coder, encodingMandatoryAlignmentInBits, maybeCharWidthInBits)
      decoderCache.put(charset, entry)
    }
    entry
  }

  def getEncoder(charset: BitsCharset) = getEncoderInfo(charset).coder
  def getEncoderInfo(charset: BitsCharset) = {
    // threadCheck()
    var entry = encoderCache.get(charset)
    if (entry eq null) {
      val coder = charset.newEncoder()
      val replacingCoder = charset.newEncoder()
      val reportingCoder = charset.newEncoder()
      replacingCoder.onMalformedInput(CodingErrorAction.REPLACE)
      replacingCoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
      reportingCoder.onMalformedInput(CodingErrorAction.REPORT)
      reportingCoder.onUnmappableCharacter(CodingErrorAction.REPORT)
      val (encodingMandatoryAlignmentInBits, maybeCharWidthInBits) = derivations(charset)
      entry = EncoderInfo(
        coder,
        replacingCoder,
        reportingCoder,
        encodingMandatoryAlignmentInBits,
        maybeCharWidthInBits
      )
      encoderCache.put(charset, entry)
    }
    entry
  }

  /**
   * Given a string and charset, compute how many bits it contains.
   *
   * This only has to scan the string if the charset is a variable-width
   * encoding. Otherwise this is just constant time.
   */
  final def lengthInBits(str: String, bitsCharset: BitsCharset): Long = {
    if (str.length == 0) return 0
    val mew = bitsCharset.maybeFixedWidth
    if (mew.isDefined) {
      val w = mew.get
      w * str.length
    } else {
      // charset is variable width encoding
      //
      // We want to get a byteBuffer of the maximum size possible
      // for this string (utf-8 is the extreme example with 4-bytes/character
      // possible  actually cesu can do 6 bytes per character.
      //
      // we want a temporary byte buffer of this size. We don't need to heap
      // allocate one.
      //
      // If no encoding error occurs, then nbytes * 8 is the answer
      //
      // If a decoding error occurs, then we either signal an error
      // So that we can implement encodingErrorPolicy,
      // or we have to redo the encoding, but in a mode where errors will be
      // replaced by something. And just take the resulting length.
      //
      // in a mode where the errors are ignored
      withLocalByteBuffer { lbb =>
        withLocalCharBuffer { cbb =>
          val cb = cbb.getBuf(str.length)
          cb.append(str)
          cb.flip()
          val encoder = getEncoder(bitsCharset)

          // it is assumed here that the encoder's behavior with respect to
          // dfdl:encodingErrorPolicy is already set up.
          // So it will replace broken or unmapped characters, or we'll throw
          // because of them.
          //
          encoder.reset()
          val nBytes = math.ceil(str.length * encoder.maxBytesPerChar()).toLong
          val bb = lbb.getBuf(nBytes)

          val cr = encoder.encode(cb, bb, true)
          cr match {
            case CoderResult.UNDERFLOW => // ok. Normal termination
            case CoderResult.OVERFLOW =>
              Assert.invariantFailed("byte buffer wasn't big enough to accomodate the string")
            case _ if cr.isMalformed() => cr.throwException()
            case _ if cr.isUnmappable() => cr.throwException()
          }
          bb.flip()
          val lengthInBytes = bb.remaining()
          lengthInBytes * 8
        }
      }
    }
  }

  /**
   * Truncates the string to what will fit.
   *
   * This leaves a potential fragment space. This space might be a few bits
   * (non byte-sized space), or more than one byte if say the string is utf-8,
   * the last character needs 3 bytes, but only 2 bytes + a few more bits are available.
   *
   * That unused space is supposed to get filled by the RightFillUnparser with fillbyte
   */
  final def truncateToBits(str: String, bitsCharset: BitsCharset, nBits: Long): String = {
    Assert.usage(nBits >= 0)
    if (str.length == 0) return str
    if (nBits == 0) return ""
    val mew = bitsCharset.maybeFixedWidth
    if (mew.isDefined) {
      val w = mew.get
      val nChars = nBits / w
      if (nChars > str.length)
        str
      else
        str.substring(0, nChars.toInt)
    } else {
      // charset is variable width encoding
      //
      // We want to get a byteBuffer
      withLocalByteBuffer { lbb =>
        val nBytes = nBits / 8
        val bb = lbb.getBuf(nBytes)
        withLocalCharBuffer { cbb =>
          val cb = cbb.getBuf(str.length)
          cb.append(str)
          cb.flip()
          val encoder = getEncoder(bitsCharset)
          //
          // it is assumed here that the encoder's behavior with respect to
          // dfdl:encodingErrorPolicy is already set up.
          // So it will replace broken or unmapped characters, or we'll throw
          // because of them.
          //
          encoder.reset()
          val cr = encoder.encode(cb, bb, true)
          val truncString: String =
            cr match {
              case CoderResult.UNDERFLOW => {
                // all fits in nBits
                str
              }
              case CoderResult.OVERFLOW => {
                // couldn't fit everything, what we did take from the
                // cb is the subset of characters we want.
                cb.flip()
                val truncString = cb.toString()
                truncString
              }
              case _ if cr.isMalformed() || cr.isUnmappable => {
                cr.throwException()
                Assert.impossible()
              }
            }
          truncString
        }
      }
    }
  }

}

class CharacterSetAlignmentError(
  csName: String,
  requiredAlignmentInBits: Int,
  alignmentInBitsWas: Int
) extends Exception(
    "Character set %s requires %s alignment (bits), but alignment was %s (bits)"
      .format(csName, requiredAlignmentInBits, alignmentInBitsWas)
  )
