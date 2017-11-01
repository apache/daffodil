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
import org.apache.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.Assert
import java.nio.charset.StandardCharsets
import org.apache.daffodil.util.MaybeULong

/**
 * When unparsing, we reuse all the DFA logic to identify delimiters within
 * the data that need to be escaped, so we need to treat the
 * string data being unparsed as a DataInputStream.
 */
final class StringDataInputStreamForUnparse
  extends DataInputStreamImplMixin {
  import DataInputStream._

  final protected val cst = new AnyRef with DataStreamCommonState

  var str: String = null
  var dis: DataInputStream = null

  private val utf8Decoder = StandardCharsets.UTF_8.newDecoder()

  // TODO: Performance - we could keep a BBDIS around, and "reset" it by
  // putting it onto a byte buffer that we fill from the string. That way
  // we wouldn't have to allocate a BBDIS unless the string requires a bigger
  // byte buffer.
  //
  def reset(str: String) {
    this.str = str
    val ba = str.getBytes(StandardCharsets.UTF_8)
    dis = ByteBufferDataInputStream(ba)
    dis.setDecoder(utf8Decoder)
  }

  private def doNotUse = Assert.usageError("Not to be called on " + Misc.getNameFromClass(this))

  def asIteratorChar: DataInputStream.CharIterator = {
    Assert.usage(dis != null, "Must call reset(str) before any other method.")
    dis.asIteratorChar
  }

  override def bitLimit0b = dis.bitLimit0b
  override def bitPos0b: Long = dis.bitPos0b
  override def discard(mark: DataInputStream.Mark): Unit = dis.discard(mark)
  override def fillCharBuffer(cb: java.nio.CharBuffer) = dis.fillCharBuffer(cb)
  override def futureData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  override def getBinaryDouble(): Double = doNotUse
  override def getBinaryFloat(): Float = doNotUse
  override def getSignedBigInt(bitLengthFrom1: Int): BigInt = doNotUse
  override def getSignedLong(bitLengthFrom1To64: Int): Long = doNotUse
  override def getUnsignedBigInt(bitLengthFrom1: Int): BigInt = doNotUse
  override def getUnsignedLong(bitLengthFrom1To64: Int): passera.unsigned.ULong = doNotUse
  override def getByteArray(bitLengthFrom1: Int): Array[Byte] = doNotUse
  override def lookingAt(matcher: java.util.regex.Matcher, initialRegexMatchLimitInChars: Long): Boolean =
    dis.lookingAt(matcher, initialRegexMatchLimitInChars)
  override def mark(requestorID: String): DataInputStream.Mark = dis.mark(requestorID)
  override def markPos = dis.markPos
  override def pastData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  override def reset(mark: DataInputStream.Mark): Unit = dis.reset(mark)
  override def resetPos(m: MarkPos) = dis.resetPos(m)
  override def setBitLimit0b(bitLimit0b: MaybeULong): Boolean = doNotUse
  override def setByteOrder(byteOrder: ByteOrder): Unit = doNotUse
  override def byteOrder: ByteOrder = doNotUse
  // override def setCharWidthInBits(charWidthInBits: MaybeInt): Unit = doNotUse
  override def setDebugging(setting: Boolean): Unit = doNotUse
  override def getDecoder = doNotUse
  override def setDecoder(decoder: java.nio.charset.CharsetDecoder): Unit = doNotUse
  override def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = doNotUse
  // override def setEncodingMandatoryAlignment(bitAlignment: Int): Unit = doNotUse
  override def isDefinedForLength(length: Long): Boolean = doNotUse
  override def skip(nBits: Long): Boolean = doNotUse
  override def skipChars(nChars: Long): Boolean = getString(nChars).isDefined
  override def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit = doNotUse
  override def validateFinalStreamState {} // does nothing
}
