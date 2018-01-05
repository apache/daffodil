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

package org.apache.daffodil.io

import passera.unsigned.ULong
import java.nio.CharBuffer
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Logging

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
 * For DataOutputStream, we have slightly different concepts.
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
trait DataOutputStream extends DataStreamCommon
  with Logging {

  def id: Int

  def relBitPos0b: ULong

  final def relBitPos1b: ULong = ULong(relBitPos0b + 1L)

  def maybeAbsBitPos0b: MaybeULong

  /**
   * Besides setting the relBitPos, it also maintains the value of
   * the absolute bit pos, if it is known.
   */
  protected def setRelBitPos0b(newRelBitPos0b: ULong): Unit

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
  protected def setMaybeRelBitLimit0b(newMaybeRelBitLimit0b: MaybeULong, reset: Boolean = false): Boolean

  def resetMaybeRelBitLimit0b(savedBitLimit0b: MaybeULong): Unit

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putLong(signedLong: Long, bitLengthFrom1To64: Int, finfo: FormatInfo): Boolean

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int, finfo: FormatInfo): Boolean

  /**
   * If bitLengthFrom1 bits are available to be written before bitLimit0b (if
   * defined) is encountered, then this writes the bitLengthFrom1 least
   * significant bits of the bigInt using the current bit order and byte order,
   * and returns true. The signed flag determines whether or not the output
   * should be output as a signed or unsigned type.
   *
   * If not enough bits are available or the big integer cannot fit into
   * bitLengthFrom1 bits, this writes nothing and returns false.
   *
   * It is a usage error if signed is false and bigInt is a negative BigInteger.
   *
   * It is a usage error if bitLengthFrom1 is not greater than or equal to 1.
   *
   */
  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int, signed: Boolean, finfo: FormatInfo): Boolean

  /**
   * If bitLengthFrom1 bits are available to be written before bitLimit0b (if
   * defined) is encountered, then this writes the bitLengthFrom1 bits of the
   * ba using the current bit order and byte order, and returns true. The array
   * is assumed to be have bigEndian byte order and most significant bit first
   * bit order.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1 is not greater than or equal to 1.
   *
   */
  def putByteArray(ba: Array[Byte], bitLengthFrom1: Int, finfo: FormatInfo): Boolean

  /**
   * Float and Double
   * <p>
   * These are unparsed per the currently set BinaryFloatRep, byteOrder, and bitOrder
   * <p>
   * Returns false if there are not 32 bits or 64 bits (respectively) available.
   */
  def putBinaryFloat(v: Float, finfo: FormatInfo): Boolean
  def putBinaryDouble(v: Double, finfo: FormatInfo): Boolean

  /**
   * Returns number of characters transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  def putString(str: String, finfo: FormatInfo): Long
  def putCharBuffer(cb: CharBuffer, finfo: FormatInfo): Long

  /**
   * close-out this output stream. No more writing to this after.
   */
  def setFinished(finfo: FormatInfo): Unit
  def isFinished: Boolean

}
