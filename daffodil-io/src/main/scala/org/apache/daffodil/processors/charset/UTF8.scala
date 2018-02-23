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

package org.apache.daffodil.processors.charset

import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream

object BitsCharsetUTF8 extends {
  override val name = "UTF-8"
} with BitsCharsetJava {

  override def newDecoder() = new BitsCharsetDecoderUTF8()
}

class BitsCharsetDecoderUTF8
  extends BitsCharsetDecoderCreatesSurrogates {

  protected override def decodeOneUnicodeChar(dis: InputSourceDataInputStream, finfo: FormatInfo): Char = {
    val byte1 = getByte(dis, 0)
    if ((byte1 & 0x80) == 0) {
      // 1 byte
      byte1.toChar
    } else if ((byte1 & 0xE0) == 0xC0) {
      // 2 bytes
      val byte2 = getByte(dis, 8)
      checkContinuationByte(dis, byte2, 16)
      checkOverlong(byte1, 0x1F, 16)
      val cp = ((byte1 & 0x1F) << 6) | (byte2 & 0x3F)
      cp.toChar
    } else if ((byte1 & 0xF0) == 0xE0) {
      // 3 bytes
      val byte2 = getByte(dis, 8)
      checkContinuationByte(dis, byte2, 16)
      val byte3 = getByte(dis, 16)
      checkContinuationByte(dis, byte3, 24)
      checkOverlong(byte1, 0x0F, 24)
      val cp = ((byte1 & 0x0F) << 12) | ((byte2 & 0x3F) << 6) | (byte3 & 0x3F)
      if (cp >= 0xD800 && cp <= 0xDFFF) {
        // out of valid range of Unicode (reserved for surrogate)
        throw new BitsCharsetDecoderMalformedException(24)
      }
      cp.toChar
    } else if ((byte1 & 0xF8) == 0xF0) {
      // 4 bytes
      val byte2 = getByte(dis, 8)
      checkContinuationByte(dis, byte2, 16)
      val byte3 = getByte(dis, 16)
      checkContinuationByte(dis, byte3, 24)
      val byte4 = getByte(dis, 24)
      checkContinuationByte(dis, byte4, 32)
      checkOverlong(byte1, 0x07, 32)
      val cp = ((byte1 & 0x0F) << 18) | ((byte2 & 0x3F) << 12) | ((byte3 & 0x3F) << 6) | (byte4 & 0x3F)
      if (cp > 0x10FFFF) {
         // out of valid range of Unicode
         throw new BitsCharsetDecoderMalformedException(32)
      }
      val high = Character.highSurrogate(cp)
      setLowSurrogate(Character.lowSurrogate(cp))
      high
    } else if ((byte1 & 0xFC) == 0xF8) {
      // 5 bytes, invalid, but we still need to read any continuation bits and
      // mark them as all malformed
      val byte2 = getByte(dis, 8)
      checkContinuationByte(dis, byte2, 16)
      val byte3 = getByte(dis, 16)
      checkContinuationByte(dis, byte3, 24)
      val byte4 = getByte(dis, 24)
      checkContinuationByte(dis, byte4, 32)
      val byte5 = getByte(dis, 32)
      checkContinuationByte(dis, byte5, 40)
      throw new BitsCharsetDecoderMalformedException(40)
    } else if ((byte1 & 0xFE) == 0xFC) {
      // 6 bytes, invalid, but we still need to read any continuation bits and
      // mark them as all malformed
      val byte2 = getByte(dis, 8)
      checkContinuationByte(dis, byte2, 16)
      val byte3 = getByte(dis, 16)
      checkContinuationByte(dis, byte3, 24)
      val byte4 = getByte(dis, 24)
      checkContinuationByte(dis, byte4, 32)
      val byte5 = getByte(dis, 32)
      checkContinuationByte(dis, byte5, 40)
      val byte6 = getByte(dis, 40)
      checkContinuationByte(dis, byte6, 48)
      throw new BitsCharsetDecoderMalformedException(48)
    } else {
      throw new BitsCharsetDecoderMalformedException(8)
    }
  }

  @inline final def checkContinuationByte(dis: InputSourceDataInputStream, byte: Int, bitsConsumedSoFar: Int): Unit = {
    if ((byte & 0xC0) != 0x80) {
      // if this is not a continuation byte, that means all bytes consumed so
      // far *except* this byte are malformed and this byte is expected to be
      // the first byte of new a UTF-8 sequence. So we want to back up the bit
      // position by one byte to the end of the malformed data, and thow an
      // exception for those malformed bytes
      dis.setBitPos0b(dis.bitPos0b - 8)
      throw new BitsCharsetDecoderMalformedException(bitsConsumedSoFar - 8)
    }
  }

  @inline final def checkOverlong(byte: Int, mask: Int, bitsConsumedSoFar: Int): Unit = {
    if ((byte & mask) == 0) throw new BitsCharsetDecoderMalformedException(bitsConsumedSoFar)
  }
}
