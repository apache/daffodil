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
package org.apache.daffodil.io

import java.math.{ BigInteger => JBigInt }
import java.util.regex.Matcher

import org.apache.daffodil.lib.exceptions.ThinException
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Poolable

import passera.unsigned.ULong

/*
 * TODO:
 *
 * Q: Do we want raw and must-be-aligned versions of the data-accessing methods? - i.e.,
 * that don't check for alignment to a proper byte boundary (but require it) and
 * that throw if not sufficient bits?
 *
 * A: Only if we find this pattern of usage to be needed repeatedly. Another situation where we might want
 * these is if the existing methods, because they do not enforce alignment, end up having to do checking
 * that is undesirable for performance reasons.
 */

/**
 * This trait defines the low level API called by Daffodil's Parsers.
 * <p>
 * It has features to support
 * <ul>
 * <li>backtracking
 * <li>regex pattern matching using Java Pattern regexs (for lengthKind pattern and pattern asserts)
 * <li>character-by-character access as needed by our DFA delimiter/escaping
 * <li>very efficient access to small binary data (64-bits or smaller)
 * <li>alignment and skipping
 * <li>encodingErrorPolicy 'error' and 'replace'
 * <li>convenient use of zero-based values because java/scala APIs for I/O are all zero-based
 * <li>convenient use of 1-based values because DFDL is 1-based, so debug/trace and such all want to be 1-based values.
 * </ul>
 * A goal is that this API does not allocate objects as I/O operations are performed unless
 * boxed objects are being returned. For example getSignedLong(...) should not allocate anything
 * per call; however, getSignedBigInt(...) does, because a BigInt is a heap-allocated object.
 * <p>
 * Internal buffers and such may be dropped/resized/reallocated as needed during
 * method calls. The point is not that you never allocate. It's that the per-I/O operation
 * overhead does not require object allocation for every data-accessing method call.
 * <p>
 * Similarly, text data can be retrieved into a char buffer, and the char buffer can
 * provide a limit on size (available capacity of the char buffer) in characters. The text can be
 * examined in the char buffer, or a string can be created from the char buffer's contents when
 * needed.
 * <p>
 * This API is very stateful, and not-thread-safe i.e., each thread must have its own object.
 * Some of this is inherent in this API style, and some is inherited from the underlying
 * objects this API uses (such as CharsetDecoder).
 * <p>
 * This API is also intended to support some very highly optimized implementations. For example,
 * if the schemas is all text, and the encoding is known to be iso-8859-1, then there is no notion of a
 * decode error, and every byte value, extended to a Char value, *is* the Unicode codepoint.
 * No decoder needs to be used in this case and this API becomes a quite thin layer on top of a
 * java.io.BufferedStream.
 * <p>
 * Terminology:
 *
 *     Available Data - this is the data that is between the current bit position, and
 *     some limit. The limit can either be set (via setBitLimit calls), or it can be
 *     limited by tunable values, or implementation-specific upper limits, or it can simply
 *     be the end of the data stream.
 *
 *     Different kinds of DataInputStreams can have different limits. For example, a
 *     File-based DataInputStream may have no limit on the forward speculation distance,
 *     because the file can be randomly accessed if necessary. Contrasting that with a
 *     data stream that is directly connected to a network socket may have a upper limit
 *     on the amount of data that it is willing to buffer.
 *
 *     None of this is a commitment that this API will in fact have multiple specialized
 *     implementations. It's just a possibility for the future.
 * <p>
 * Implementation Note:
 *     It is the implementation of this interface which implements the Bucket Algorithm
 *     as described on the Daffodil Wiki. All of that bucket stuff is beneath this
 *     API.
 * <p>
 * In general, this API tries to return a value rather than throw exceptions whenever the
 * behavior may be very common. This leaves it up to the caller to decide whether or
 * not to throw an exception, and avoids the overhead of try-catch blocks. The exception
 * to this rule are the methods that involve character decoding for textual data. These
 * methods may throw CharacterCodingException when the encoding error policy is 'error'.
 *
 */

object DataInputStream {

  case class NotEnoughDataException(nBits: Long) extends ThinException

  /**
   * Backtracking
   * <p>
   * The mark and reset system is more sophisticated than that
   * of java's BufferedInputStream, which allows only a single outstanding mark.
   * <p>
   * This trait enables a stack of mark values to be created and reset, respecting
   * stack ordering, that is, they are nested locations and must be created and released
   * in an order consistent with stack ordering.
   * <p>
   * The mark contains additional state beyond just the position (which is maintained
   * at bit granularity). All the other state aspects (decoder, bit order, etc.)
   * are also maintained by a mark.
   * <p>
   * Implementation note: The oldest/deepest mark is expected to be built on top of
   * a java BufferedInputStream mark.
   * <p>
   * Use of mark/reset should eliminate any need for random-access setters of
   * the bit position.
   */
  trait Mark extends Poolable {
    def bitPos0b: Long
  }

  /**
   * For mini-marks that just mark/reset the position
   */
  type MarkPos = Long
  object MarkPos {
    def NoMarkPos: MarkPos = -1L
  }

  /**
   * An Iterator[Char] with additional peek and peek2 methods.
   */
  trait CharIterator extends Iterator[Char] {

    def reset(): Unit

    def setFormatInfo(fi: FormatInfo): Unit

    /**
     * Looks ahead 1 into the data. Returns -1.toChar if there is no
     * character to peek to.
     *
     * This does not advance the position in the data stream.
     */
    def peek(): Char

    /**
     * Looks ahead 2 into the data. Returns -1.toChar if there is no
     * character to peek to.
     *
     * This does not advance the position in the data stream.
     */
    def peek2(): Char
  }

}

trait DataInputStream extends DataStreamCommon with IOUsingMacrosMixin {
  import DataInputStream._

  /**
   * The position is maintained at bit granularity.
   */
  def bitPos0b: Long
  final def bitPos1b: Long = bitPos0b + 1

  /**
   * The byte position excludes any partial byte. So if the bit position
   * is not on a byte boundary, then the byte position is as if the bit position
   * was rounded down to the next byte boundary.
   * <p>
   * These are convenience methods only.
   */
  final def bytePos0b: Long = bitPos0b >> 3
  final def bytePos1b: Long = bitPos1b >> 3

  /**
   * The bit limit is Nope if there is no imposed limit other than end of data.
   * <p>
   * The bitLimit1b is the value of the first bitPos1b beyond the end of the data.
   * Valid bit positions are less than, but not equal to, the bit limit.
   * <p>
   * If bitLimit0b is defined, then there IS that much data available at least.
   */
  def bitLimit0b: MaybeULong
  final def bitLimit1b: MaybeULong =
    if (bitLimit0b.isEmpty) MaybeULong.Nope else MaybeULong(bitLimit0b.get + 1)

  def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit

  /**
   * Sets the bit limit to an absolute value and returns true.
   * Returns false if the new bit limit is beyond the existing bit limit range.
   */
  def setBitLimit0b(bitLimit0b: MaybeULong): Boolean
  final def setBitLimit1b(bitLimit1b: MaybeULong): Boolean = {
    val newLimit = if (bitLimit1b.isDefined) MaybeULong(bitLimit1b.get - 1) else MaybeULong.Nope
    setBitLimit0b(newLimit)
  }

  /**
   * Returns a mark value. It saves the current state of the input stream such
   * that it can be restored by calling reset(mark).
   * <p>
   * The state includes the bit position, the bit limit,
   * and all the other parts of the state for which there are setters.
   * <p>
   * Multiple calls to mark will create distinct mark values.
   * <p>
   * The requestorID argument is a string for debugging pool-management bugs. It
   * identifies the code that is requesting the mark/reset. If resets are not done
   * then at end of execution the non-reset requestorIDs are issued in an abort.
   * <p>
   * Implementation Note: A mark is probably just an integer offset into an array
   * that is treated like a stack (top of stack at higher index value locations of
   * the array). So marking and resetting does not need to imply allocating
   * structures, though it could be implemented that way.
   * <p>
   * In other words marking and resetting do not imply allocation of objects.
   */
  def mark(requestorID: String): Mark

  /**
   * Resets the current state of the input stream to the position it had
   * when the mark was created. All marks taken later than the mark argument are
   * implicitly discarded. The mark passed as an argument is also discarded by
   * this operation.
   * <p>
   * If the bit position has advanced from the mark far enough to exceed
   * implementation defined maximum extent, or a tunable limit on the maximum extent,
   * then the mark is referred to as an invalid mark. A call to reset to
   * an invalid mark causes an IOException to be thrown.
   */
  def reset(mark: Mark): Unit

  /**
   * Discards a mark. Old marks must be discarded when there is no possibility
   * that a reset(...) to them will be needed. This reclaims any storage needed
   * by the mark. (Think of it as popping the stack of marks back to, and including
   * the supplied mark.)
   * <p>
   * Any mark newer than the supplied argument mark is also discarded.
   * <p>
   * Marks must be discarded because otherwise stack locations may grow slowly
   * over time, but without limit (memory leak).
   */
  def discard(mark: Mark): Unit

  /**
   * Determines whether the input stream has this much more data.
   *
   * Does not advance the position.
   *
   * On a network input stream, this may block to determine if the stream
   * contains enough data or is at end-of-data.
   */
  def isDefinedForLength(nBits: Long): Boolean

  /**
   * Returns true if the input stream has at least 1 bit of data.
   *
   * Does not advance the position.
   *
   * Returns true immediately if the input stream has available data that
   * has not yet been consumed.
   *
   * On a network input stream, this may block to determine if the stream
   * contains data or is at end-of-data.
   *
   * This is used when parsing multiple elements from a stream to see if there
   * is data or not before calling parse().
   *
   * It may also be used after a parse() operation that is intended to consume
   * the entire data stream (such as for a file) to determine if all data has
   * been consumed or some data is left-over.
   *
   * This is equivalent to calling isDefinedForLength(1)
   */
  def hasData(): Boolean

  /**
   * Returns a byte array containing the bits between the current bit position
   * and that position plus bitLengthFrom1.
   * <p>
   * The byte array result is constructed using the currently set bit order and
   * byte order. The returned byte array is always in most significant bit
   * first bit order and little endian byte ordr.
   * <p>
   * If the data stream does not have bitLengthFrom1 remaining bits,
   * NotEnoughDataException is thrown. Calls should be preceded by calls to isDefinedForLength
   * to check if sufficient bits are available. Alternatively one can catch the exception,
   * but that is likely less performant.
   * <p>
   * Usage: The smallest value of bitLengthFrom1 is 1.
   * <p>
   * If the bitLengthFrom1 is not a multiple of 8, the final byte will be
   * padded with zeros to make a full byte.
   */
  def getByteArray(bitLengthFrom1: Int, finfo: FormatInfo): Array[Byte]

  /**
   * Same as getByteArray(Int, FormatInfo), except it accepts the array as a
   * parameter and will fill that array with bytes. This can help to improve
   * performance if an array can be reused to avoid allocations. But the caller
   * must be careful upon return, since the array will not be cleared and may
   * be bigger than the number of bits filled. It is the responsibility of the
   * caller to know which bits in the array were set based on the bit length
   * and format info.
   */
  def getByteArray(bitLengthFrom1: Int, finfo: FormatInfo, array: Array[Byte]): Unit

  /**
   * Returns a long integer containing the bits between the current bit position
   * and that position plus the bitLength.
   * <p>
   * The long integer result is constructed using the currently set bit order
   * and byte order. The returned value is always non-negative.
   * <p>
   * This call is expected to be used for extracting data for all unsigned
   * integer types up to UnsignedLong with 64 bits length. Calling code converts
   * into Byte, Short, or Int types if smaller size integers are required.
   * <p>
   * Usage: The bitLength must be between 1 and 64 inclusive.
   * <p>
   * If the data stream does not have bitLengthFrom1To64 remaining bits,
   * NotEnoughDataException is thrown. Calls should be preceded by calls to isDefinedForLength
   * to check if sufficient bits are available. Alternatively one can catch the exception,
   * but that is likely less performant.
   */
  def getUnsignedLong(bitLengthFrom1To64: Int, finfo: FormatInfo): ULong

  /**
   * Similar, but returns a negative value if the most-significant bit of the
   * data is 1. That is, the most significant bit is treated as a twos-complement
   * sign bit for the data.
   * <p>
   * If the bitLength is 1, then the value returned will be 1 or 0 depending on
   * the bit value. That is, if there is only 1 bit, it is treated as non-negative.
   * <p>
   * If the bit length is 2 or greater, then the most significant
   * bit will be interpreted as a sign bit for a twos-complement representation.
   * (A 2-bit signed integer can represent the four values 1, 0, -1, -2 as bits 01, 00, 11, 10 respectively)
   * <p>
   * Usage: The maximum number of bits is 64.
   *
   * If the data stream does not have bitLengthFrom1To64 remaining bits,
   * NotEnoughDataException is thrown. Calls should be preceded by calls to isDefinedForLength
   * to check if sufficient bits are available. Alternatively one can catch the exception,
   * but that is likely less performant.
   */
  def getSignedLong(bitLengthFrom1To64: Int, finfo: FormatInfo): Long

  /**
   * Constructs a big integer from the data. The current bit order and byte order are used.
   * <p>
   * The result will never be negative.
   * <p>
   * If the data stream does not have bitLengthFrom1 remaining bits,
   * NotEnoughDataException is thrown. Calls should be preceded by calls to isDefinedForLength
   * to check if sufficient bits are available. Alternatively one can catch the exception,
   * but that is likely less performant.
   * <p>
   * Usage: The smallest value of bitLengthFrom1 is 1.
   * <p>
   * It is recommended that getUnsignedLong be used for any bit length 64 or less, as
   * that method does not require a heap allocated object to represent the value.
   */
  def getUnsignedBigInt(bitLengthFrom1: Int, finfo: FormatInfo): JBigInt

  /**
   * Constructs a big integer from the data. The current bit order and byte order are used.
   * <p>
   * The result will be negative if the most significant bit is set.
   * <p>
   * If the data stream does not have bitLengthFrom1 remaining bits,
   * NotEnoughDataException is thrown. Calls should be preceded by calls to isDefinedForLength
   * to check if sufficient bits are available. Alternatively one can catch the exception,
   * but that is likely less performant.
   *
   * If the bitLength is 1, then the value returned will be 1 or 0 depending on
   * the bit value. That is, if there is only 1 bit, it is treated as non-negative.
   * <p>
   * Usage: The smallest value of bitLengthFrom1 is 1.
   * <p>
   * It is recommended that getSignedLong be used for any bit length 64 or less, as
   * that method does not require a heap allocated object to represent the value.
   */
  def getSignedBigInt(bitLengthFrom1: Int, finfo: FormatInfo): JBigInt

  /**
   * Float and Double
   * <p>
   * These are constructed per the currently set BinaryFloatRep.
   * <p>
   * Throws NotEnoughDataException if there are not 32 bits or 64 bits (respectively) available.
   * Consider first calling isDefinedForLength before calling this in order to avoid the
   * possibility of a throw.
   */
  def getBinaryFloat(finfo: FormatInfo): Float
  def getBinaryDouble(finfo: FormatInfo): Double

  /**
   * Returns One(string) if any (up to nChars) are available, Nope otherwise.
   *
   * Throws a CharacterCodingException if the encoding error policy is 'error'
   * and a decode error is detected within nChars.
   */
  def getSomeString(nChars: Long, finfo: FormatInfo): Maybe[String]

  /**
   * Skips N characters and returns true, adjusting the bitPos0b based on
   * parsing them. Returns false if there is not enough data
   * to skip all N characters.
   */
  def skipChars(nChars: Long, finfo: FormatInfo): Boolean

  /**
   * Matches a regex Matcher against a prefix of the data stream.
   * <p>
   * Advances the stream to the bit position following the match.
   * <p>
   * If there is no match, the bit position is unchanged.
   * <p>
   * The matcher will be reset, possibly multiple times during this operation.
   * <p>
   * After this method returns, the matcher's hitEnd and requireEnd methods
   * are interpreted relative to the end of the available data. See the
   * definition of available data above.
   * <p>
   * Result is false if there is no match.
   * <p>
   * If the encodingErrorPolicy is 'error' then an attempt to match
   * that encounters data which causes a decoding error will throw a
   * CharacterCodingException.
   * <p>
   * Note that this is an exception to the way other methods of this API
   * work in that an exception is thrown. It is generally expected that
   * character decode errors are rare/exceptional situations.
   * <p>
   * Implementation Note: pre-buffering of data cannot cause these
   * CharacterCodingExceptions. It is only when the pattern match actually
   * must consume another character that the error is thrown.
   * <p>
   * This implementation will take advantage of the behavior of fillCharBuffer
   * in that the matcher will consume data from a char buffer and see if a match
   * can succeed completely before a decode error occurs. The
   * match will only try to consume more if the matcher
   * does not yet know if the match will succeed or fail, or the match
   * could be longer if more data was available. So it is only when a match tries
   * to consume more that it will encounter the decode error.
   * <p>
   * If the encodingErrorPolicy is 'replace' then the matching may
   * encounter Unicode replacement characters, and these may be
   * incorporated into a successful match. This has strong performance implications
   * in that regular expressions like ".*" will match an unlimited length
   * in the data stream. In the presence of backtracking this can result in large parts
   * of the data stream being decoded multiple times.
   * <p>
   * This API does not use a CharBuffer because there is no way to avoid allocation of
   * strings by the underlying Matcher and regular expression API upon which this is
   * built.
   */
  def lookingAt(matcher: Matcher, finfo: FormatInfo): Boolean

  /**
   * As characters are iterated, the underlying bit position changes.
   * <p>
   * This does not construct a new iterator, it provides access to
   * iterator behavior of the DataInputStream. In other words, there is no
   * separate state of the iterator from the DataInputStream.
   * <p>
   * If any of the characteristics of the stream are changed (such as encoding,
   * or bit order), then the iterator begins decoding characters using that new
   * information immediately.
   * <p>
   * If next() is called after hasNext() has returned false, or when hasNext() would
   * return false, then an IllegalStateException is thrown.
   * <p>
   * The behavior if characteristics are changed between
   * a call to hasNext() and the subsequent call to next() is unspecified, but such
   * usage is an error. If detected an unspecified RuntimeException will be thrown.
   * However, the implementation may not check for this condition for performance
   * reasons.
   * <p>
   * If encodingErrorPolicy is 'error', and a malformed character representation is
   * encountered in the data then both hasNext() and next() on this iterator will
   * throw CharacterCodingException.
   * <p>
   * Note that this is an exception to the way other methods of this API
   * work in that an exception is thrown. It is generally expected that
   * character decode errors are rare/exceptional situations.
   * <p>
   * If encodingErrorPolicy is 'replace' then hasNext() will only return false
   * once the end of the available data has been reached. The next() method may return
   * a Unicode replacement character as a replacement for an actual decode error, or
   * if the encoding is a Unicode encoding, then a unicode replacement character may
   * actually exist in the data stream. It is not possible to distinguish these two
   * situations using this API when encodingErrorPolicy is 'replace'.
   * <p>
   * If insufficient bits are available to decode a character, then this is
   * treated as a malformed character, per above.
   * <p>
   * (Note: This iterator replaces the use of scala Reader[Char] in the low levels of the I/O
   * system. Reader[Char] can still be used, by encapsulating this iterator to create
   * the chars. However, the only need for scala Reader[Char] was due to use
   * of the scala combinator RegexParsers functionality, which is replaced by
   * the lookingAt method above. The DFA stuff doesn't actually
   * need a Reader[Char]. It would be happy with this iterator.)
   */
  def asIteratorChar: DataInputStream.CharIterator

  /**
   * Save the bitPos0b for restore later
   */
  def markPos: MarkPos
  def resetPos(m: MarkPos): Unit

  /**
   * Closes any underlying I/O streams/channels that are part of the implementation
   * of this and frees related resources.
   */
  def close(): Unit
}
