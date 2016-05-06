package edu.illinois.ncsa.daffodil.io

import passera.unsigned.ULong
import java.nio.charset.CharsetEncoder
import java.nio.ByteBuffer
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.util.MaybeULong

/**
 * There is an asymmetry between DataInputStream and DataOutputStream with respect to the
 * positions and limits in the bit stream.
 *
 * For the DataInputStream, we have this concept of the current bitPos0b,
 * and optionally there may be abound called bitLimit0b. There are 1b
 * variants of these.
 *
 * For parsing, these are always absolute values, that is they contain bit
 * position relative the ultimate start of the input stream where parsing
 * began.
 *
 * For DataOutputStream, we have slighly different concepts.
 *
 * There are absolute and relative variants. The absolute bitPosOb or
 * absBitPos0b is symmetric to the parser's bitPos0b. It's the position
 * relative to the ultimate start of the output stream.
 *
 * However, we often do not know this value. So the UState and
 * DataOutputStream have a maybeAbsBitPos0b which can be MaybeULong.Nope if
 * the value isn't known.
 *
 * In addition we have the relative or relBitPos0b. This is relative to the
 * start of whatever buffer we are doing unparsing into.
 *
 * When unparsing, we often have to unparse into a buffer where the
 * ultimate actual absolute position isn't yet known, but we have to do the
 * unparsing anyway, for example so that we can measure exactly how long
 * something is.
 *
 * Conversely, sometimes we simply must have the absolute output bit
 * position, for example, when computing the number of bits to insert to
 * achieve the required alignment.
 *
 * Hence we have relBitPos0b - always known and is a value >= 0, and we
 * have maybeAbsBitPos0b which is a MaybeULong. If known it is >=0.
 *
 * Corresponding to bit position we have bit limit, which is measured in
 * the same 0b or 1b units, but is *always* a maybe type, because even in
 * the case where we know the absolute position, we still may or may not
 * have any limit in place. Hence the UState and DataOutputStream have a
 *
 * maybeRelBitLimit0b
 *
 * and
 *
 * maybeAbsBitLimit0b.
 *
 * One invariant is this: when the absolute bit pos is known, then it is
 * the same as the relative bit pos. Similarly when the absolute bit limit
 * is known, then the relative bit limit is known and is equal.
 *
 */
trait DataOutputStream extends DataStreamCommon {

  def relBitPos0b: ULong

  def relBitPos1b: ULong = ULong(relBitPos0b + 1L)

  def maybeAbsBitPos0b: MaybeULong
  def maybeAbsBitPos1b: MaybeULong

  /**
   * Besides setting the relBitPos, it also maintains the value of
   * the absolute bit pos, if it is known.
   */
  def setRelBitPos0b(newRelBitPos0b: ULong): Unit

  /**
   * Absolute bit limit zero based
   *
   * If defined it is the position 1 bit past the last bit location that can be written.
   * So if we at starting at bitPos0b of 0, and we allow only 100 bits, then the bit positions are
   * 0 to 99, and the bit limit is 100.
   */
  def maybeAbsBitLimit0b: MaybeULong

  /**
   * Relative bit limit zero based
   */
  def maybeRelBitLimit0b: MaybeULong

  /**
   * sets, but also maintains the absolute bit limit, if that is defined.
   */
  def setMaybeRelBitLimit0b(newMaybeRelBitLimit0b: MaybeULong): Boolean

  def resetMaybeRelBitLimit0b(savedBitLimit0b: MaybeULong): Unit

  def encoder: CharsetEncoder
  def setEncoder(encoder: CharsetEncoder): Unit

  /**
   * Sets the fill byte. Only the least significant 8 bits of the argument are used.
   */
  def setFillByte(fillByte: Int): Unit

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putLong(signedLong: Long, bitLengthFrom1To64: Int): Boolean

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int): Boolean

  /**
   * If bitLengthFrom1 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1 least significant bits of the bigInt using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1 is not greater than or equal to 1.
   *
   */
  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean

  /**
   * Float and Double
   * <p>
   * These are unparsed per the currently set BinaryFloatRep, byteOrder, and bitOrder
   * <p>
   * Returns false if there are not 32 bits or 64 bits (respectively) available.
   */
  def putBinaryFloat(v: Float): Boolean
  def putBinaryDouble(v: Double): Boolean

  /**
   * Returns number of bytes transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  def putByteBuffer(bb: ByteBuffer): Long
  def putBytes(ba: Array[Byte]): Long

  /**
   * Returns number of bits transferred. The last byte of the byte buffer
   * can be a fragment byte.
   */
  def putBitBuffer(bb: java.nio.ByteBuffer, lengthInBits: Long): Long

  /**
   * Returns number of bits transferred. The last byte of the byte buffer
   * can be a fragment byte.
   */
  def putBits(ba: Array[Byte], byteStartOffset0b: Int, lengthInBits: Long): Long

  /**
   * Returns number of characters transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  def putString(str: String): Long
  def putCharBuffer(cb: CharBuffer): Long

  /**
   * Force buffered content to output if possible.
   *
   * Idempotent. That is dos.flush(); dos.flush(); means the same thing as just one call.
   * However, suppose this DOS is connected to a java.io.OutputStream which is connected to a pipe,
   * and some process is trying to read from that pipe.
   *
   * This operation writes and flushes only WHOLE BYTES to the java.io.OutputStream. If this
   * DOS has a fragment of a byte (less than 8 bits) at the end, then those bits are not written
   * to the java.io.OutputStream.
   *
   * To insure these final bits are in fact written, one must call dos.setFinished().
   */
  def flush(): Unit

  /**
   * close-out this output stream. No more writing to this after.
   */
  def setFinished(): Unit
  def isFinished: Boolean

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
  final def withBitLengthLimit(lengthLimitInBits: Long)(body: => Unit): Boolean = macro IOMacros.withBitLengthLimitMacroForOutput

}
