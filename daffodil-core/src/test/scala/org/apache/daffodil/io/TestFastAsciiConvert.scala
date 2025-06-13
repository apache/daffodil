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

import org.junit.Assert._
import org.junit.Test

class TestFastAsciiConvert {

  val cvt = FastAsciiToUnicodeConverter

  @Test def testConvertByte1(): Unit = {
    assertEquals(cvt.UnicodeReplacementCharacter, cvt.convertByte(-1.toByte))
    assertEquals(0.toChar, cvt.convertByte(0.toByte))
    assertEquals(127.toChar, cvt.convertByte(127.toByte))
    assertEquals(cvt.UnicodeReplacementCharacter, cvt.convertByte(128.toByte))
  }

  @Test def testConvertInt1(): Unit = {
    assertEquals(cvt.UnicodeReplacementCharacter, cvt.convertInt(-1.toByte))
    assertEquals(0.toChar, cvt.convertInt(0.toByte))
    assertEquals(127.toChar, cvt.convertInt(127.toByte))
    assertEquals(cvt.UnicodeReplacementCharacter, cvt.convertInt(128.toByte))
  }

  @Test def testConvertLong1(): Unit = {
    assertEquals(0xfffdfffdfffdfffdL, cvt.convertLong(-1))
    assertEquals(0x0L, cvt.convertLong(0))
    assertEquals(0xfffdL, cvt.convertLong(128))
    assertEquals(0xfffd0000L, cvt.convertLong(0x00008000))
    assertEquals(0xfffd00000000L, cvt.convertLong(0x00800000))
    assertEquals(0xfffd000000000000L, cvt.convertLong(0x80000000))
    assertEquals(0x002c002c002c002cL, cvt.convertLong(0x2c2c2c2c))
  }

  @Test def testConvert1(): Unit = {
    val data = "abcdefg".toList.map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val cb = cvt.convert(bb)
    val str = cb.toString()
    assertEquals("abcdefg", str)
  }

  @Test def testConvert2(): Unit = {
    val data = "12345678abcdefg".toList.map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val cb = cvt.convert(bb)
    val str = cb.toString()
    assertEquals("12345678abcdefg", str)
  }

  @Test def testConvert2a(): Unit = {
    val data = "12345678a".toList.map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val cb = cvt.convert(bb)
    val str = cb.toString()
    assertEquals("12345678a", str)
  }

  @Test def testConvert3(): Unit = {
    val data = "\u00802345\u007F78abcdefg".toList.map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val cb = cvt.convert(bb)
    val str = cb.toString()
    assertEquals("\uFFFD2345\u007F78abcdefg", str)
  }

  @Test def testConvert4(): Unit = {
    val data = "\u00802345\u007F78\u00802345\u007F78abcdefg".toList.map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val cb = cvt.convert(bb)
    val str = cb.toString()
    assertEquals("\uFFFD2345\u007F78\uFFFD2345\u007F78abcdefg", str)
  }

}
