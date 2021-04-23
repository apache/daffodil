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


import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Misc
import org.junit.Assert._
import org.junit.Test

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.io.OutputStream

/**
 * Tests about detecting end-of-data, and
 * insuring proper behavior for InputSourceDataInputStream
 * with respect to reading unnecessary data.
 *
 * These cover the fix for DAFFODIL-2502, where users observed
 * Daffodil would hang because of calls to isAtEnd.
 */
class TestInputSourceDataInputStream8 {

  /**
   * Test provides coverage of special detection in I/O layer
   * of a incorrectly behaving InputStream.
   */
  @Test def testReadZeroBytesDetected(): Unit = {
    //
    // an InputStream where the read(buf, off, len) call returns 0
    // meaning no data available temporarily.
    // This is a badly behaving input stream. The API
    // is supposed to be BLOCKING.
    //
    val badIS = new InputStream {
      override def read() = ???
      override def read(buf: Array[Byte], off: Int, len: Int): Int = {
        assertTrue(len > 0)
        0
      }
    }
    val isdis = InputSourceDataInputStream(badIS)
    val e = intercept[InputStreamReadZeroError] {
      isdis.hasData()
    }
    assertTrue(e.getMessage().toLowerCase.contains("illegal"))
  }

  @deprecated("Tests isAtEnd", "3.1.0")
  @Test def testIsAtEndEmpty1(): Unit = {
    val emptyIS = new InputStream {
      override def read() = ???
      override def read(buf: Array[Byte], off: Int, len: Int): Int = {
        assertTrue(len > 0)
        -1 // nothing here
      }
    }
    val isdis = InputSourceDataInputStream(emptyIS)
    //
    // before we attempt to read anything, we're not at end.
    assertFalse(isdis.hasReachedEndOfData)
    // this must attempt to read, and will get end-of-data
    assertTrue(isdis.isAtEnd())
    // so now we know we're at the end-of-data
    assertTrue(isdis.hasReachedEndOfData)
  }

  @deprecated("Tests isAtEnd", "3.1.0")
  @Test def testIsAtEndWholeByte1(): Unit = {
    val oneByteIS = new ByteArrayInputStream("A".getBytes)
    val isdis = InputSourceDataInputStream(oneByteIS)
    //
    // before we attempt to read anything, we're not at end.
    assertFalse(isdis.hasReachedEndOfData)
    // this must attempt to read, and will get end-of-data
    assertTrue(isdis.hasData())
    assertEquals(0, isdis.bitPos0b)
    assertEquals(0,isdis.getUnsignedLong(1,FakeFormatInfo_MSBF_BE).toLong)
    // we've only consumed 1 bit meaning
    // we should have only fetched 1 byte and this will NOT encounter end of data yet.
    assertFalse(isdis.hasReachedEndOfData)
    // now retrieve next 7 bits. We still won't be at end of data
    assertEquals('A',isdis.getUnsignedLong(7,FakeFormatInfo_MSBF_BE).toChar)
    assertFalse(isdis.hasReachedEndOfData)
    // try to read one more bit. That will not succeed, but now we'll be at end.
    assertTrue(isdis.isAtEnd())
    assertFalse(isdis.hasData())
    assertTrue(isdis.hasReachedEndOfData)
  }

  @Test def testIsAtEndPartialByte1(): Unit = {
    val oneByteIS = new ByteArrayInputStream(Misc.hex2Bytes("FF"))
    val isdis = InputSourceDataInputStream(oneByteIS)
    isdis.setBitLimit0b(MaybeULong(1))
    //
    // before we attempt to read anything, we're not at end.
    assertFalse(isdis.hasReachedEndOfData)
    assertTrue(isdis.isDefinedForLength(1)) // matches the bit limit set above
    assertEquals(0, isdis.bitPos0b)
    assertEquals(1,isdis.getUnsignedLong(1,FakeFormatInfo_MSBF_BE).toLong)
    // we've only consumed 1 bit meaning
    // we should have only fetched 1 byte and this will NOT encounter end of data yet.
    assertFalse(isdis.hasReachedEndOfData)
    assertEquals(1, isdis.bitPos0b)
    // try to read 7 more bits.
    // it will not succeed due to the bit limit, but
    // it is still not enough for us to have hit end of data
    assertFalse(isdis.isDefinedForLength(7))
    assertFalse(isdis.hasReachedEndOfData)
    // but, if we try to read 8 more bits, that puts us past the end of data
    // except, since there's a bit limit, we'll never touch the underlying
    // input source, so we still won't have gotten an end-of-data
    assertFalse(isdis.isDefinedForLength(8))
    assertFalse(isdis.hasReachedEndOfData)
    // if we remove the bit limit however, ...
    isdis.setBitLimit0b(MaybeULong.Nope)
    // 7 more bits of the first byte are available
    assertTrue(isdis.isDefinedForLength(7))
    // but if we even test for bits beyond that, we'll encounter the end-of-data
    assertFalse(isdis.isDefinedForLength(8))
    assertTrue(isdis.hasReachedEndOfData)
  }

  /**
   * This test shows that for a TCP stream, if 1 byte is sent,
   * then if you attempt to read more than that many bytes, it will
   * return just the 1 byte.
   *
   * This isn't testing Daffodil code, it's a test that assures us
   * that the underlying I/O layer of the JVM/Java/Scala has the
   * behavior we depend on.
   *
   * Our I/O layer depends on this behavior to avoid blocking
   * on reads that attempt to fill a bucket (for efficiency) when
   * there is temporarily less than that much data on a network
   * TCP stream.
   */
  @Test def networkReadPartial1(): Unit = {
    val sptr = new SocketPairTestRig {
      override def test(pos: OutputStream, cis: InputStream): Unit = {
        assertEquals(0, cis.available())
        pos.write(0x31)
        pos.flush()
        val buf = new Array[Byte](4)
        val nRead: Int = cis.read(buf, 0, 4)
        assertEquals(1, nRead)
        assertEquals(0x31, buf(0))
      }
    }
    sptr.run()
  }

}
