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

import java.nio.ByteBuffer

import org.apache.daffodil.api.DataStreamLimits
import org.apache.daffodil.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeULong

/**
 * This is an interface trait, and it defines methods shared by
 * both DataInputStream and DataOutputStream.
 *
 * Implementation (partial) is in DataStreamCommonImplMixin.
 *
 */
trait DataStreamCommon {

  def limits: DataStreamLimits

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
  def setLimits(newLimits: DataStreamLimits): Unit

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
   * Note that even when character encodings are not byte-centric (e.g., 7, 6, 5, or 4 bits)
   * and a character can span a byte boundary, we STILL don't need byteOrder
   * to decode characters. Just bitOrder.
   *
   * This is because bitOrder alone specifies which end of byte N is adjacent to which
   * end of byte N+1.
   */
  def setByteOrder(byteOrder: ByteOrder): Unit
  def byteOrder: ByteOrder

  /**
   * Returns number of bits remaining (if a limit is defined). Nope if not defined.
   */

  def remainingBits: MaybeULong

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
