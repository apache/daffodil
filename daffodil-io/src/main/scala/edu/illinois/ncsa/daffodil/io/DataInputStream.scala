package edu.illinois.ncsa.daffodil.io

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.util.regex.Matcher
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import passera.unsigned.ULong
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.LocalStack
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import edu.illinois.ncsa.daffodil.util.MaybeULong

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

  case class NotEnoughDataException(nBits: Long) extends Exception with ThinThrowable

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
  trait Mark {
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

object DataStreamCommon {
  /*
   * These limits will come from tunables, or just hard implementation-specific
   * thresholds.
   */
  trait Limits {
    def maximumSimpleElementSizeInBytes: Long
    def maximumSimpleElementSizeInCharacters: Long
    def maximumForwardSpeculationLengthInBytes: Long
    def maximumRegexMatchLengthInCharacters: Long
    def defaultInitialRegexMatchLimitInChars: Long
  }

}

trait DataStreamCommon extends LocalBufferMixin {
  import DataStreamCommon._
  def limits: Limits

  /**
   * Allow tuning of these thresholds and starting values. These could,
   * in principle, be tuned differently for different elements, thereby
   * keeping limits small when the schema component can be determined to
   * only require small space, but enabling larger limits/starting values
   * when a component has larger needs.
   *
   * These could be cached on, say,
   * the ElementRuntimeData object for each element, or some other kind
   * of dynamic cache.
   */
  def setLimits(newLimits: Limits): Unit

  // def setEncodingMandatoryAlignment(bitAlignment: Int): Unit
  def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit

  /**
   * Use Nope for variable-width encodings.
   */
  def setMaybeUTF16Width(maybeUTF16Width: Maybe[UTF16Width]): Unit
  def setBinaryFloatRep(binaryFloatRep: BinaryFloatRep): Unit

  /*
   * Note that when character encodings are not byte-centric (e.g., 7, 6, 5, or 4 bits)
   * then the bit order *is* used by the character decoding to determine which
   * side of a byte is first.
   */
  def setBitOrder(bitOrder: BitOrder): Unit

  /* Note that the byte order for UTF-16 and UTF-32 encodings is
   * not taken from this setByteOrder call, but by use of the
   * UTF-16BE, UTF-16LE, UTF-32BE and UTF-32LE encodings, or
   * by use of the dfdl:unicodeByteOrderMark property.
   * <p>
   * Note that when character encodings are not byte-centric (e.g., 7, 6, 5, or 4 bits)
   * then the byte order *is* used by the character decoding when a character
   * code unit spans a byte boundary.
   */
  def setByteOrder(byteOrder: ByteOrder): Unit
  def byteOrder: ByteOrder

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
  final def bitLimit1b: MaybeULong = if (bitLimit0b.isEmpty) MaybeULong.Nope else MaybeULong(bitLimit0b.get + 1)

  /**
   * Returns number of bits remaining (if a limit is defined). Nope if not defined.
   */

  def remainingBits: MaybeULong

  /**
   * Convenience methods that temporarily set and (reliably) restore the bitLimit.
   * The argument gives the limit length. Note this is a length, not a bit position.
   *
   * This is added to the current bit position to get the limiting bit position
   * which is then set as the bitLimit when
   * the body is evaluated. On return the bit limit is restored to its
   * prior value.
   * <p>
   * The return value is false if the new bit limit is beyond the existing bit limit range.
   * Otherwise the return value is true.
   * <p>
   * The prior value is restored even if an Error/Exception is thrown. (ie., via a try-finally)
   * <p>
   * These are intended for use implementing specified-length types (simple or complex).
   * <p>
   * Note that length limits in lengthUnits Characters are not implemented
   * this way. See fillCharBuffer(cb) method.
   */
  @inline final def withBitLengthLimit(lengthLimitInBits: Long)(body: => Unit): Boolean = {
    val savedLengthLimit = bitLimit0b
    if (!setBitLimit0b(MaybeULong(bitPos0b + lengthLimitInBits))) false
    else {
      try {
        body
      } finally {
        resetBitLimit0b(savedLengthLimit)
      }
      true
    }
  }

  protected[io] def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit

  /**
   * Sets the bit limit to an absolute value and returns true.
   * Returns false if the new bit limit is beyond the existing bit limit range.
   */
  def setBitLimit0b(bitLimit0b: MaybeULong): Boolean
  final def setBitLimit1b(bitLimit1b: MaybeULong): Boolean = {
    val newLimit = if (bitLimit1b.isDefined) MaybeULong(bitLimit1b.get - 1) else MaybeULong.Nope
    setBitLimit0b(newLimit)
  }

  /*
   * Methods for moving through data.
   */

  /**
   * advances the bit position to the specified alignment.
   * <p>
   * Note that the bitAlignment1b argument is 1-based.
   * <p>
   * Passing 0 as the argument is a usage error.
   * <p>
   * Passing 1 as the argument performs no alignment, as any bit position
   * is 1-bit aligned.
   * <p>
   * For any other value, the bit position (1-based) is advanced to
   * the next multiple of that argument value.
   * <p>
   * False is returned if there are insufficient available bits to achieve
   * the alignment.
   */

  def align(bitAlignment1b: Int): Boolean

  /**
   * For assertion checking really. Optimizations should remove the need for most
   * alignment operations. This can be used in assertions that check that this
   * is working properly.
   * <p>
   * Note that the bitAlignment1b argument is 1-based.
   * <p>
   * Passing 0 as the argument is a usage error.
   * <p>
   * Passing 1 as the argument performs no alignment, as any bit position
   * is 1-bit aligned.
   */
  def isAligned(bitAlignment1b: Int): Boolean

  /**
   * Advances the bit position by nBits. If nBits aren't available this
   * returns false. Otherwise it returns true.
   */
  def skip(nBits: Long): Boolean

  /**
   * Debugging flag. If set then performance may be reduced, but
   * historic and upcoming data may be viewed using the pastData and futureData
   * methods.
   *
   * This should be set at the beginning of execution. If it is set after data has
   * been accessed then IllegalStateException is thrown.
   */
  def areDebugging: Boolean
  def setDebugging(setting: Boolean): Unit

  /**
   * Access to historic (past data) and upcoming data for
   * purposes of display in a trace or debugger.
   *
   * If areDebugging is false, these throw IllegalStateException
   */
  def pastData(nBytesRequested: Int): ByteBuffer
  def futureData(nBytesRequested: Int): ByteBuffer

  /**
   * Called once after each parse operation to verify final invariants for
   * the implementation.
   *
   * Use to perform checks such as that data structures held in pools are
   * all returned before end of parse.
   */
  def validateFinalStreamState: Unit
}

trait DataInputStream
  extends DataStreamCommon {
  import DataInputStream._

  /*
   * Setters for all the text and binary characteristics.
   * <p>
   * These are set rather than passed to various operations because
   * they will, most often, be very slow changing relative to the
   * operations that actually perform data motion.
   * <p>
   * If any of these are not set as part of initialization,
   * then IllegalStateException is
   * thrown when any of the methods that access data are called, or
   * any of the mark/reset/discard methods are called.
   */

  /**
   * Sets the character set decoder.
   * <p>
   * If the `charWidthInBits` is less
   * than 8, then the decoder must be an instance of
   * `NonByteSizeCharsetEncoderDecoder` (a trait mixed into such decoders)
   * so that an initial bit offset can be set if necessary.
   */
  def getDecoder: CharsetDecoder
  def setDecoder(decoder: CharsetDecoder): Unit

  /**
   * Returns a mark value. It saves the current state of the input stream such
   * that it can be restored by calling reset(mark).
   * <p>
   * The state includes the bit position, the bit limit,
   * and all the other parts of the state for which there are setters.
   * <p>
   * Multiple calls to mark will create distinct mark values.
   * <p>
   * Implementation Note: A mark is probably just an integer offset into an array
   * that is treated like a stack (top of stack at higher index value locations of
   * the array). So marking and resetting does not need to imply allocating
   * structures, though it could be implemented that way.
   * <p>
   * In other words marking and resetting do not imply allocation of objects.
   */
  def mark: Mark

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
   * Does not advance the position
   */
  def isDefinedForLength(nBits: Long): Boolean

  /**
   * Returns Nope if no more data is possible (end of data), otherwise returns the number of
   * bytes transferred.
   * <p>
   * Set the position and limit of the byte buffer if you want to obtain only a small amount
   * of data. The maximum amount transferred will be the difference between the position() and
   * the limit() of the byte buffer.
   * <p>
   * Upon return, the byte buffer is not 'flipped' by this call. To read out the data
   * that was just written to the byte buffer by this method using relative getter calls
   * the caller must flip the byte buffer.
   * <p>
   * If the data source ends in the middle of a byte (possible for bit-oriented data)
   * then that partial final byte can be transferred. Bits past the end of the partial byte
   * will be transferred along with the partial byte.
   * <p>
   * The final bit position of the DataInputStream excludes any of these additional bits
   * that are transferred as part of a partial final byte. Hence, if this method is
   * called and end of data is encountered, then upon return if any bytes are transferred
   * then bitPos0b == bitLimit0b (if defined)
   */
  def fillByteBuffer(bb: ByteBuffer): MaybeInt

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
  def getUnsignedLong(bitLengthFrom1To64: Int): ULong

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
  def getSignedLong(bitLengthFrom1To64: Int): Long

  /**
   * Constructs a big integer from the data. The current bit order and byte order are used.
   * <p>
   * The result will never be negative.
   * <p>
   * If the data stream does not have bitLengthFrom1 remaining bits, Nope is returned.
   * <p>
   * Usage: The smallest value of bitLengthFrom1 is 1.
   * <p>
   * It is recommended that getUnsignedLong be used for any bit length 64 or less, as
   * that method does not require a heap allocated object to represent the value.
   */
  def getUnsignedBigInt(bitLengthFrom1: Int): Maybe[BigInt]

  /**
   * Constructs a big integer from the data. The current bit order and byte order are used.
   * <p>
   * The result will be negative if the most significant bit is set.
   * <p>
   * If the data stream does not have bitLengthFrom1 remaining bits, Nope is returned.
   * If the bitLength is 1, then the value returned will be 1 or 0 depending on
   * the bit value. That is, if there is only 1 bit, it is treated as non-negative.
   * <p>
   * Usage: The smallest value of bitLengthFrom1 is 1.
   * <p>
   * It is recommended that getSignedLong be used for any bit length 64 or less, as
   * that method does not require a heap allocated object to represent the value.
   */
  def getSignedBigInt(bitLengthFrom1: Int): Maybe[BigInt]

  /**
   * Float and Double
   * <p>
   * These are constructed per the currently set BinaryFloatRep.
   * <p>
   * Throws NotEnoughDataException if there are not 32 bits or 64 bits (respectively) available.
   * Consider first calling isDefinedForLength before calling this in order to avoid the
   * possibility of a throw.
   */
  def getBinaryFloat(): Float
  def getBinaryDouble(): Double

  /**
   * Fill a charBuffer with characters.
   * <p>
   * Returns the number of chars delivered. Nope if end of data stream.
   * <p>
   * The bit position is advanced to immediately after the representation of the
   * delivered characters.
   * <p>
   * Set the position and limit of the char buffer if you want to retrieve only
   * a small number of characters. For example, a single character can
   * be retrieved if the char buffer has only one character remaining.
   * <p>
   * If the encodingErrorPolicy is 'error' and at least 1 character
   * has been delivered into the char buffer, then this will return
   * successfully, as if it stopped immediately before encountering a
   * decoding error. If, however, zero characters have been delivered
   * into the char buffer, then a decoding error will throw
   * a CharacterCodingException or will be replaced by a Unicode replacement
   * character.
   * <p>
   * Note that this is an exception to the way other methods of this API
   * work in that an exception is thrown. It is generally expected that
   * character decode errors are rare/exceptional situations.
   * <p>
   * If the encodingErrorPolicy is 'replace' then the result may contain
   * Unicode replacement characters. The bit position is advanced to
   * after the representation of all the characters, including after the
   * non-decodable data bits that result in Unicode replacement character(s).
   * <p>
   * Note that the characters may be any width in bits including a variable
   * width (such as for utf-8 which has from 1 to 4 bytes per character).
   * <p>
   * Retrieving text limited by size in bits or bytes can be achieved by
   * setting the bitLimit to a position N bytes (or bits) greater than the current
   * position, then calling this method with a charBuffer having sufficient
   * capacity that the char buffer available size will not be reached before the
   * bitLimit. Having performed the read, the bitLimit can then be restored to
   * its prior value. See the withBitLengthLimit method.
   * <p>
   * In theory at least, this method can be used to do some parsing without
   * ever allocating a string. E.g., checking for a specifc delimiter character.
   * The pattern match can occur directly against the charbuffer to determine if
   * the text matches the literal nil syntax. If so we set the nilled flag in the
   * infoset element and never create a string.
   * <p>
   * The char buffer is not 'flipped' by this method. To read the characters
   * that are placed into the char buffer by this method using relative getter calls
   * the caller of this method must flip the char buffer.
   * <p>
   * When characters are not made up of complete bytes, but fragments of a byte, then
   * when data ends in the middle of a byte, a character can be decoded from the
   * partial-final byte, if enough bits are available from the prior byte and the
   * partial final byte.
   * <p>
   * Implementation Note: a specialized 4-bit encoding which maps 4 bits to
   * 0-9A-F can be used to treat packed decimal representations like text strings.
   */
  def fillCharBuffer(cb: CharBuffer): MaybeULong

  /**
   * Returns true if it fills all remaining space in the char buffer.
   *
   * Convenience method since this idiom is so common due to the
   * way fillCharBuffer works to return early when decode errors are
   * encountered.
   */
  protected final def fillCharBufferLoop(cb: CharBuffer): Boolean = {
    var maybeN: MaybeULong = MaybeULong(0)
    var total: Long = 0
    val nChars = cb.remaining
    while (maybeN.isDefined && total < nChars) {
      maybeN = fillCharBuffer(cb)
      if (maybeN.isDefined) total += maybeN.get.toLong
    }
    total == nChars
  }

  /**
   * Returns One(string) if nChars are available, Nope otherwise.
   *
   * Throws a CharacterCodingException if the encoding error policy is 'error'
   * and a decode error is detected within nChars.
   */
  final def getString(nChars: Long): Maybe[String] = {
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(nChars)
      val gotAll = fillCharBufferLoop(cb)
      val res = if (!gotAll) Nope
      else Maybe(cb.flip.toString)
      res
    }
  }

  /**
   * Returns One(string) if any (up to nChars) are available, Nope otherwise.
   *
   * Throws a CharacterCodingException if the encoding error policy is 'error'
   * and a decode error is detected within nChars.
   */
  final def getSomeString(nChars: Long): Maybe[String] = {
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(nChars)
      fillCharBufferLoop(cb)
      if (cb.position() == 0) Nope
      else Maybe(cb.flip.toString)
      // TODO: Performance - we need to copy here. Consider return type of Maybe[StringBuilder]
      // as that will allow for non-copying trim and other manipulations of the string
      // without further copyies.
    }
  }

  /**
   * Skips N characters and returns true, adjusting the bitPos0b based on
   * parsing them. Returns false if there is not enough data
   * to skip all N characters.
   */
  def skipChars(nChars: Long): Boolean

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
   * A tuneable limit maximumRegexMatchLengthInCharacters can be set to limit
   * the maximum size of a match. If this tunable maximum is hit, or an implementation
   * specific absolute maximum is hit, then after return of this method the matcher will
   * have the hitEnd and requireEnd values as if the end of the data stream had been
   * reached.
   * <p>
   * This API does not use a CharBuffer because there is no way to avoid allocation of
   * strings by the underlying Matcher and regular expression API upon which this is
   * built.
   */
  def lookingAt(matcher: Matcher, initialRegexMatchLimitInChars: Long = limits.defaultInitialRegexMatchLimitInChars): Boolean

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
}
