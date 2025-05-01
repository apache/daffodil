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

import java.nio.ByteBuffer

/**
 * Highly optimized converter for Ascii to Unicode
 */
object FastAsciiToUnicodeConverter {

  def convert(bb: ByteBuffer) = {
    val cb = ByteBuffer.allocate(2 * bb.limit())
    val cbChar = cb.asCharBuffer()
    //
    // Go after data in the largest chunks we can (Long)
    // so as to eliminate per-byte/char bounds checks
    //
    val bbBytesOfWholeLongWords = ((bb.limit() >> 3) << 3).toLong
    val numBytesTrailingFragment = bb.limit() - bbBytesOfWholeLongWords

    val bLong = bb.asLongBuffer()
    val cbLong = cb.asLongBuffer()
    (1 to bLong.limit()).foreach { i =>
      val bbl = bLong.get()
      val long1: Int = (bbl >> 32).toInt & 0xffffffff
      val long2: Int = bbl.toInt & 0xffffffff
      val cbl1 = convertLong(long1)
      val cbl2 = convertLong(long2)
      cbLong.put(cbl1)
      cbLong.put(cbl2)
    }

    (1 to numBytesTrailingFragment.toInt).foreach { j =>
      val pos = bb.limit() - j
      val byte = bb.get(pos)
      val char = convertByte(byte)
      cbChar.put(pos, char)
    }

    cb.asCharBuffer()
  }

  val UnicodeReplacementCharacter = 0xfffd.toChar

  /**
   * Convert a single byte of ascii to unicode.
   * If the MSBit is set (negative byte) then that's
   * not a legal character code so produce the unicode
   * replacement character.
   */
  @inline
  def convertByte(byte: Byte) = {
    if (byte < 0) UnicodeReplacementCharacter
    else byte.toChar
  }

  @inline
  def convertInt(int: Int) = {
    val i = int & 0xff
    if (i > 127) UnicodeReplacementCharacter
    else i.toChar
  }

  @inline
  def convertLong(bytes: Int): Long = {
    val int1 = bytes & 0xff
    val int2 = (bytes >> 8) & 0xff
    val int3 = (bytes >> 16) & 0xff
    val int4 = (bytes >> 24) & 0xff
    val char1 = convertInt(int1)
    val char2 = convertInt(int2)
    val char3 = convertInt(int3)
    val char4 = convertInt(int4)
    val res = (char4.toLong << 48) | (char3.toLong << 32) | (char2.toLong << 16) | char1
    res
  }

}
