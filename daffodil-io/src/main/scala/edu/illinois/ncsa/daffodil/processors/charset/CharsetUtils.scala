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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.nio.charset.Charset
import java.nio.charset.IllegalCharsetNameException
import java.io.UnsupportedEncodingException
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CodingErrorAction
import edu.illinois.ncsa.daffodil.io.NonByteSizeCharset
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.io.LocalBufferMixin
import java.nio.charset.CharsetDecoder
import java.nio.charset.CoderResult
import java.nio.charset.CharsetEncoder

/*
 * These are needed because the ordinary java/scala Charset, Encoder, and Decoder objects
 * are not serializable. So we must go back to the string from which they can be created
 * in order to serialize these.
 */

/**
 * Serializable Charset
 */
case class DFDLCharset(val charsetName: String) extends Serializable {
  import java.nio.charset.StandardCharsets

  charset // Force charset to be evaluted to ensure it's valid at compile time.  It's a lazy val so it will be evaluated when de-serialized
  @transient lazy val charset = CharsetUtils.getCharset(charsetName)
  @transient lazy val maybeFixedWidth = CharsetUtils.maybeEncodingFixedWidth(charset)

  def padCharWidthInBits = {
    if (maybeFixedWidth.isDefined)
      maybeFixedWidth.get
    else {
      charset match {
        case StandardCharsets.UTF_8 => 8
        case _ => Assert.invariantFailed("unsupported charset: " + charset)
      }
    }
  }
}

///**
// * Serializable Encoder
// */
//class DFDLEncoder(private val dfdlCharset: DFDLCharset) extends Serializable {
//  @transient private lazy val encoder_ = dfdlCharset.charset.newEncoder()
//
//  def encoder = {
//    encoder_.reset() // each time this is asked for it gets reset.
//    encoder_
//  }
//}
//
///**
// * Serializable Decoder
// */
//class DFDLDecoder(private val dfdlCharset: DFDLCharset) extends Serializable {
//  @transient private lazy val decoder_ = dfdlCharset.charset.newDecoder()
//
//  def decoder = {
//    decoder_.reset() // each time this is asked for it gets reset.
//    decoder_
//  }
//}

object CharsetUtils {

  // keep in case we need to put this check back in temporarily
  // private val noWSNoLowerCasePatern = """[^\sa-z]+""".r.pattern // one or more characters, not whitespace, not lower case.

  def getCharset(charsetName: String): Charset = {
    Assert.usage(charsetName != null);
    //    {
    //      // TODO: expensive check, remove if unnecessary
    //      //
    //      // However, encodingEv should guarantee that we already get an upper case token.
    //      // So we shouldn't need to test this.
    //      val m = noWSNoLowerCasePatern.matcher(charsetName)
    //      Assert.usage(m.matches)
    //    }
    // There is no notion of a default charset in DFDL.
    // So this can be val.
    val csn: String = charsetName

    val cs = try {
      val cs =
        if (csn == "US-ASCII-7-BIT-PACKED" || // deprecated name
          csn == "X-DFDL-US-ASCII-7-BIT-PACKED") // new official name
          USASCII7BitPackedCharset
        else Charset.forName(csn)
      One(cs)
    } catch {
      case e: IllegalCharsetNameException => Nope
    }
    if (cs.isEmpty) throw new UnsupportedEncodingException(csn)
    cs.value
  }

  /**
   * Subtle bug in decoders in Java 7 when there is room for only 1
   * character in the CharBuffer.
   *
   * While we could just test for Java 8, which doesn't have this bug,
   * it is worthwhile to keep this in case we end up trying to support
   * Java 7 at some point in the future.
   */
  lazy val hasJava7DecoderBug = {
    val decoder = Charset.forName("utf-8").newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val bb = ByteBuffer.allocate(6)
    bb.put(-16.toByte) // invalid first utf-8 byte
    bb.limit(6).position(0)
    val cb = CharBuffer.allocate(1)
    val cr = decoder.decode(bb, cb, true)
    if (cr.isOverflow && // This is the bug!
      cb.position == 0 &&
      bb.position == 0) true
    else if (cr.isError) false // no bug
    //    else if (cr.isOverflow && // This is what *should* happen if CodingErrorAction.REPLACE is used.
    //      cb.position == 1 &&
    //      bb.position == 1 &&
    //      cb.get(0) == this.unicodeReplacementChar) false
    else
      Assert.invariantFailed("Unexpected decoder behavior. " + cr)
  }

  val unicodeReplacementChar = '\uFFFD'

  private lazy val UTF32 = CharsetUtils.getCharset("UTF-32")
  private lazy val UTF32BE = CharsetUtils.getCharset("UTF-32BE")
  private lazy val UTF32LE = CharsetUtils.getCharset("UTF-32LE")

  /**
   * Tells us the encoding's fixed width if it is fixed.
   * Nope if not fixed width
   */
  final def maybeEncodingFixedWidth(charset: Charset): MaybeInt = {
    import java.nio.charset.StandardCharsets
    val res: Int = charset match {
      case nbs: NonByteSizeCharset => nbs.bitWidthOfACodeUnit
      case StandardCharsets.US_ASCII => 8
      case StandardCharsets.UTF_8 => return MaybeInt.Nope
      case StandardCharsets.UTF_16 |
        StandardCharsets.UTF_16BE |
        StandardCharsets.UTF_16LE => 16 // Note dfdl:utf16Width='variable' not supported
      case StandardCharsets.ISO_8859_1 => 8
      case UTF32 | UTF32BE | UTF32LE => 32
      case _ => Assert.usageError("unknown charset: " + charset.name)
    }
    MaybeInt(res)
  }

}

trait EncoderDecoderMixin
  extends LocalBufferMixin {
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
  private lazy val decoderCache = new java.util.HashMap[Charset, CharsetDecoder]
  private lazy val encoderCache = new java.util.HashMap[Charset, CharsetEncoder]

  def getDecoder(charset: Charset): CharsetDecoder = {
    // threadCheck()
    var decoder = decoderCache.get(charset)
    if (decoder eq null) {
      decoder = charset.newDecoder()
      decoderCache.put(charset, decoder)
    }
    decoder
  }

  def getEncoder(charset: Charset): CharsetEncoder = {
    // threadCheck()
    var encoder = encoderCache.get(charset)
    if (encoder eq null) {
      encoder = charset.newEncoder()
      encoderCache.put(charset, encoder)
    }
    encoder
  }

  /**
   * Given a string and charset, compute how many bits it contains.
   *
   * This only has to scan the string if the charset is a variable-width
   * encoding. Otherwise this is just constant time.
   */
  final def lengthInBits(str: String, dcharset: DFDLCharset): Long = {
    if (str.length == 0) return 0
    val mew = dcharset.maybeFixedWidth
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
      val cs = dcharset.charset

      withLocalByteBuffer { lbb =>
        withLocalCharBuffer { cbb =>
          val cb = cbb.getBuf(str.length)
          cb.append(str)
          cb.flip()
          val encoder = getEncoder(cs)

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
            case CoderResult.UNDERFLOW => //ok. Normal termination
            case CoderResult.OVERFLOW => Assert.invariantFailed("byte buffer wasn't big enough to accomodate the string")
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
  final def truncateToBits(str: String, dcharset: DFDLCharset, nBits: Long): String = {
    Assert.usage(nBits >= 0)
    if (str.length == 0) return str
    if (nBits == 0) return ""
    val mew = dcharset.maybeFixedWidth
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
      val cs = dcharset.charset

      withLocalByteBuffer { lbb =>
        val nBytes = nBits / 8
        val bb = lbb.getBuf(nBytes)
        withLocalCharBuffer { cbb =>
          val cb = cbb.getBuf(str.length)
          cb.append(str)
          cb.flip()
          val encoder = getEncoder(cs)
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
              case _ if cr.isMalformed() => {
                cr.throwException()
                ""
              }
              case _ if cr.isUnmappable() => {
                cr.throwException()
                ""
              }
            }
          truncString
        }
      }
    }
  }

}

class CharacterSetAlignmentError(csName: String, requiredAlignmentInBits: Int, alignmentInBitsWas: Int)
  extends Exception("Character set %s requires %s alignment (bits), but alignment was %s (bits)".
    format(csName, requiredAlignmentInBits, alignmentInBitsWas))
