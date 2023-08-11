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

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.UsageException
import org.apache.daffodil.lib.util.MaybeULong

import org.junit.Assert._
import org.junit.Test

class TestInputSourceDataInputStream2 {
  val tenDigits = "1234567890"
  val ten = tenDigits.getBytes("utf-8")
  val twentyDigits = tenDigits * 2
  val twenty = twentyDigits.getBytes("utf-8")
  val finfo = FormatInfoForUnitTest()

  @Test def testMark1(): Unit = {
    val dis = InputSourceDataInputStream(ten)
    val m1 = dis.mark("testMark1")
    var arr = dis.getByteArray(80, finfo)
    assertEquals(10, arr.size)
    assertEquals(0x31.toByte, arr(0))
    assertEquals(80, dis.bitPos0b)
    assertEquals(false, dis.bitLimit0b.isDefined)
    assertEquals(81, dis.bitPos1b)
    assertEquals(false, dis.bitLimit1b.isDefined)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    arr = dis.getByteArray(80, finfo)
    assertEquals(10, arr.size)
    assertEquals(0x30.toByte, arr(9))
    assertEquals(80, dis.bitPos0b)
    assertEquals(false, dis.bitLimit0b.isDefined)
    assertEquals(81, dis.bitPos1b)
    assertEquals(false, dis.bitLimit1b.isDefined)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testMark2(): Unit = {
    val dis = InputSourceDataInputStream(twenty)
    var m1: DataInputStream.Mark = null
    dis.withBitLengthLimit(5 * 8) {
      val arr = dis.getByteArray(5 * 8, finfo)
      assertEquals(5, arr.size)
      m1 = dis.mark("testMark2")
      val iter = dis.asIteratorChar
      iter.setFormatInfo(finfo)
      assertFalse(iter.hasNext)
    }
    dis.setBitLimit0b(MaybeULong(10 * 8))
    var arr = dis.getByteArray(5 * 8, finfo)
    dis.mark("testMark2b")
    assertEquals(5, arr.size)
    assertEquals(0x30.toByte, arr(4))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    assertFalse(dis.hasData())
    dis.asInstanceOf[InputSourceDataInputStream].resetBitLimit0b(MaybeULong(10 * 8))
    arr = dis.getByteArray(5 * 8, finfo)
    assertEquals(5, arr.size)
    assertEquals(0x30.toByte, arr(4))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testMark3(): Unit = {
    val dis = InputSourceDataInputStream(twenty).asInstanceOf[InputSourceDataInputStream]
    dis.setBitLimit0b(MaybeULong(5 * 8))
    var arr = dis.getByteArray(5 * 8, finfo)
    assertEquals(5, arr.size)
    val m1 = dis.mark("testMark3")
    val iter = dis.asIteratorChar
    iter.setFormatInfo(finfo)
    assertFalse(iter.hasNext)
    dis.resetBitLimit0b(MaybeULong(10 * 8))
    arr = dis.getByteArray(5 * 8, finfo)
    val m2 = dis.mark("testMark3b")
    assertEquals(5, arr.size)
    assertEquals(0x30.toByte, arr(4))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    intercept[UsageException] {
      dis.reset(m2)
    }
  }

}
