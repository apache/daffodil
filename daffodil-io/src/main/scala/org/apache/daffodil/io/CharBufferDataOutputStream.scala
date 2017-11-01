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

import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import passera.unsigned.ULong
import java.nio.ByteBuffer
import org.apache.daffodil.exceptions.Assert
import java.nio.CharBuffer
import org.apache.daffodil.util.Misc

/**
 * Used to implement unparsing when length is specified in lengthUnits 'characters' when
 * the encoding is variable width (e.g., utf-8)
 */
final class CharBufferDataOutputStream
  extends DataOutputStream with DataOutputStreamImplMixin {

  override def id = 0

  private[io] def isBuffering: Boolean = true

  private def doNotUse = Assert.usageError("Not to be called on " + Misc.getNameFromClass(this))

  protected def getJavaOutputStream(): java.io.OutputStream = doNotUse
  protected def setJavaOutputStream(newOutputStream: java.io.OutputStream): Unit = doNotUse

  private var target: CharBuffer = null

  def setCharBuffer(cb: CharBuffer) {
    target = cb
  }

  private def notToBeUsed = Assert.usageError("not to be used")

  override def putCharBuffer(cb: java.nio.CharBuffer): Long = {
    Assert.usage(target != null)
    val numCharsTransferred = cb.read(target)
    numCharsTransferred
  }

  override def putBigInt(bigInt: BigInt, bitLengthFrom1: Int, signed: Boolean): Boolean = notToBeUsed
  override def putByteArray(ba: Array[Byte], bitLengthFrom1: Int): Boolean = notToBeUsed
  override def putBytes(ba: Array[Byte]) = notToBeUsed
  override def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  override def putLong(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  override def putBinaryFloat(float: Float): Boolean = notToBeUsed
  override def putBinaryDouble(double: Double): Boolean = notToBeUsed
  final override protected def putLong_BE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  final override protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  final override protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  override def skip(nBits: Long): Boolean = notToBeUsed
  override def futureData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  override def pastData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  override def setByteOrder(byteOrder: ByteOrder): Unit = notToBeUsed
  override def byteOrder: ByteOrder = notToBeUsed
  override def validateFinalStreamState { /* do nothing */ }
}
