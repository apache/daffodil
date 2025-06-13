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

import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.lib.Implicits.intercept

import org.junit.Assert._
import org.junit.Test

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
