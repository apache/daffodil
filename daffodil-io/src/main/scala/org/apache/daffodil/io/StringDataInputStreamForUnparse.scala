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

import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeULong

/**
 * When unparsing, we reuse all the DFA logic to identify delimiters within
 * the data that need to be escaped, so we need to treat the
 * string data being unparsed as a DataInputStream.
 */
final class StringDataInputStreamForUnparse
  extends DataInputStreamImplMixin {
  import DataInputStream._

  override final protected val cst = new AnyRef with DataStreamCommonState

  var str: String = null
  var dis: DataInputStream = null

  def reset(str: String, finfo: FormatInfo) {
    this.str = str
    // TODO: This only works for Java charsets, we should really use our own
    // encoders to convert the string to bytes and support non-byte-size
    // encodings
    val ba = str.getBytes(finfo.encoder.bitsCharset.name)
    dis = InputSourceDataInputStream(ba)
  }

  private def doNotUse = Assert.usageError("Not to be called on " + Misc.getNameFromClass(this))

  override def asIteratorChar: DataInputStream.CharIterator = {
    Assert.usage(dis != null, "Must call reset(str) before any other method.")
    dis.asIteratorChar
  }

  override def bitLimit0b = dis.bitLimit0b
  override def bitPos0b: Long = dis.bitPos0b
  override def discard(mark: DataInputStream.Mark): Unit = dis.discard(mark)
  override def lookingAt(matcher: java.util.regex.Matcher, finfo: FormatInfo): Boolean = dis.lookingAt(matcher, finfo)
  override def markPos = dis.markPos
  override def mark(requestorID: String): DataInputStream.Mark = dis.mark(requestorID)
  override def reset(mark: DataInputStream.Mark): Unit = dis.reset(mark)
  override def resetPos(m: MarkPos) = dis.resetPos(m)
  override def skipChars(nChars: Long, finfo: FormatInfo): Boolean = dis.skipChars(nChars, finfo)
  override def getSomeString(nChars: Long,finfo: FormatInfo): Maybe[String] = dis.getSomeString(nChars, finfo)
  override def getString(nChars: Long,finfo: FormatInfo): Maybe[String] = dis.getString(nChars, finfo)

  override def futureData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  override def getBinaryDouble(finfo: FormatInfo): Double = doNotUse
  override def getBinaryFloat(finfo: FormatInfo): Float = doNotUse
  override def getSignedBigInt(bitLengthFrom1: Int, finfo: FormatInfo): BigInt = doNotUse
  override def getSignedLong(bitLengthFrom1To64: Int, finfo: FormatInfo): Long = doNotUse
  override def getUnsignedBigInt(bitLengthFrom1: Int, finfo: FormatInfo): BigInt = doNotUse
  override def getUnsignedLong(bitLengthFrom1To64: Int, finfo: FormatInfo): passera.unsigned.ULong = doNotUse
  override def getByteArray(bitLengthFrom1: Int, finfo: FormatInfo): Array[Byte] = doNotUse
  override def getByteArray(bitLengthFrom1: Int, finfo: FormatInfo, array: Array[Byte]): Unit = doNotUse
  override def pastData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  override def setBitLimit0b(bitLimit0b: MaybeULong): Boolean = doNotUse
  override def setDebugging(setting: Boolean): Unit = doNotUse
  override def isDefinedForLength(length: Long): Boolean = doNotUse
  override def skip(nBits: Long, finfo: FormatInfo): Boolean = doNotUse
  override def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit = doNotUse
  override def validateFinalStreamState {} // does nothing
}
