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

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * This is an input stream that infinitely returns incrementing bytes between 0
 * and 10. It returns at most 10 bytes in calls to .read(byte b[]). It will not
 * return EOF unless setEOF(...) is called, at which point only the specified
 * number of bytes can be read.
 */
class TestInputStream extends java.io.InputStream {
  private val maxByteVal: Int = 10
  private var _curByte: Int = 0

  private def readCurByte: Int = {
    if (_bytesUntilEOF.isDefined && _bytesUntilEOF.get == 0) {
      -1
    } else {
      val b = _curByte
      _curByte += 1
      _curByte = _curByte % maxByteVal
      _bytesUntilEOF = _bytesUntilEOF.map { _ - 1 }
      b
    }
  }

  private var _bytesUntilEOF: Option[Int] = None
  def setEOF(n: Int): Unit = {
    _bytesUntilEOF = Some(n)
  }

  override def read(): Int = readCurByte

  override def read(b: Array[Byte]): Int = read(b, 0, b.length)

  // Not that this differs from the default read in that it will only read
  // maxByteVal bytes at once. The idea is that this represents an input stream
  // that could read less data than was requested
  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    if (len == 0) {
      0
    } else if (_bytesUntilEOF.isDefined && _bytesUntilEOF.get == 0) {
      -1
    } else {
      val toRead1 = math.min(b.length - off, len)
      val toRead2 = math.min(_bytesUntilEOF.getOrElse(Int.MaxValue), maxByteVal)
      val toRead = math.min(toRead1, toRead2)
      var i = 0
      while (i < toRead) {
        b(off + i) = readCurByte.toByte
        i += 1
      }
      toRead
    }
  }
}

// Tests to make sure that the input stream above used for testing behaves as
// expected
class TestTestInputStream {

  @Test def testTestInputStream1(): Unit = {
    val is = new TestInputStream
    var i = 0
    while (i < 100) {
      assertEquals(i % 10, is.read)
      i += 1
    }
  }

  @Test def testTestInputStream2(): Unit = {
    val is = new TestInputStream
    val b = new Array[Byte](10)
    is.read(b)
    var i = 0
    while (i < 10) {
      assertEquals(i, b(i))
      i += 1
    }
  }

  @Test def testTestInputStream3(): Unit = {
    val is = new TestInputStream
    val b = new Array[Byte](10)
    is.read(b, 3, 3)
    var i = 3
    while (i < 6) {
      assertEquals(i - 3, b(i))
      i += 1
    }
  }

  @Test def testTestInputStream4(): Unit = {
    val is = new TestInputStream
    is.setEOF(4)
    assertEquals(0, is.read)
    assertEquals(1, is.read)
    assertEquals(2, is.read)
    assertEquals(3, is.read)
    assertEquals(-1, is.read)
  }

  @Test def testTestInputStream5(): Unit = {
    val is = new TestInputStream
    val b = new Array[Byte](10)
    is.setEOF(4)
    assertEquals(4, is.read(b))
    assertEquals(-1, is.read(b))
  }
}

class TestBucketingInputSource {

  @Test def testBucketingInputSource1(): Unit = {
    val tis = new TestInputStream
    val bis = new BucketingInputSource(tis, 4)
    var i = 0
    while (i < 100) {
      assertEquals(i, bis.position())
      assertEquals(i % 10, bis.get())
      assertEquals(i + 1, bis.position())
      i += 1
    }
  }

  @Test def testBucketingInputSource2(): Unit = {
    val tis = new TestInputStream
    val bis = new BucketingInputSource(tis, 3)
    val b = new Array[Byte](10)
    assertEquals(true, bis.get(b, 0, 10))
    var i = 0
    while (i < 10) {
      assertEquals(i, b(i))
      i += 1
    }
    assertEquals(true, bis.get(b, 0, 10))
    i = 0
    while (i < 10) {
      assertEquals(i, b(i))
      i += 1
    }
    assertEquals(20, bis.position())
  }

  @Test def testBucketingInputSource3(): Unit = {
    val tis = new TestInputStream
    val bis = new BucketingInputSource(tis, 3)
    val b = new Array[Byte](10)
    assertEquals(0, bis.get())
    assertEquals(1, bis.get())
    assertEquals(2, bis.get())
    assertEquals(3, bis.get())
    assertEquals(4, bis.get())
    assertEquals(true, bis.get(b, 3, 5))
    var i = 0
    while (i < 5) {
      assertEquals(i + 5, b(i + 3))
      i += 1
    }
  }

  @Test def testBucketingInputSource4(): Unit = {
    val tis = new TestInputStream
    val bis = new BucketingInputSource(tis, 3)
    tis.setEOF(4)
    assertEquals(0, bis.get())
    assertEquals(1, bis.get())
    assertEquals(2, bis.get())
    assertEquals(3, bis.get())
    assertEquals(-1, bis.get())
  }

  @Test def testBucketingInputSource5(): Unit = {
    val tis = new TestInputStream
    val bis = new BucketingInputSource(tis, 3)
    val b = new Array[Byte](10)
    tis.setEOF(4)
    assertEquals(false, bis.get(b, 0, 10))
    assertEquals(true, bis.get(b, 0, 4))
    var i = 0
    while (i < 4) {
      assertEquals(i, b(i))
      i += 1
    }
    assertEquals(-1, bis.get())
  }

  @Test def testBucketingInputSource6(): Unit = {
    val tis = new TestInputStream
    val bis = new BucketingInputSource(tis, 3)
    tis.setEOF(17)
    var i = 0
    while (i < 8) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.compact()
    while (i < 16) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.compact()
    assertEquals(i % 10, bis.get())
    assertEquals(-1, bis.get())
  }

  @Test def testBucketingInputSource7(): Unit = {
    val tis = new TestInputStream
    val bis = new BucketingInputSource(tis, 3)
    tis.setEOF(17)
    var i = 0
    while (i < 2) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.lockPosition(2)
    while (i < 17) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    assertEquals(-1, bis.get())
    assertEquals(17, bis.position())

    bis.compact() // shouldn't do anything since first bucket is locked
    bis.position(2)
    assertEquals(2, bis.position())
    bis.releasePosition(2)
    bis.compact() // should'nt do anything since cur pos is at the first bucket
    i = 2
    while (i < 15) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.compact() // should releaes two buckets
    assertEquals(5, bis.get())
    assertEquals(6, bis.get())
    assertEquals(-1, bis.get())
  }
}

class TestByteBufferInputSource {

  val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3,
    4, 5, 6, 7, 8, 9)

  @Test def testByteBufferInputSource1(): Unit = {
    val bb = ByteBuffer.wrap(data)
    val bis = new ByteBufferInputSource(bb)
    var i = 0
    while (i < 30) {
      assertEquals(i, bis.position())
      assertEquals(i % 10, bis.get())
      assertEquals(i + 1, bis.position())
      i += 1
    }
    assertEquals(-1, bis.get())
  }

  @Test def testByteBufferInputSource2(): Unit = {
    val bb = ByteBuffer.wrap(data)
    val bis = new ByteBufferInputSource(bb)
    val b = new Array[Byte](10)
    assertEquals(true, bis.get(b, 0, 10))
    var i = 0
    while (i < 10) {
      assertEquals(i, b(i))
      i += 1
    }
    assertEquals(true, bis.get(b, 0, 10))
    i = 0
    while (i < 10) {
      assertEquals(i, b(i))
      i += 1
    }
    assertEquals(20, bis.position())
  }

  @Test def testByteBufferInputSource3(): Unit = {
    val bb = ByteBuffer.wrap(data)
    val bis = new ByteBufferInputSource(bb)
    val b = new Array[Byte](10)
    assertEquals(0, bis.get())
    assertEquals(1, bis.get())
    assertEquals(2, bis.get())
    assertEquals(3, bis.get())
    assertEquals(4, bis.get())
    assertEquals(true, bis.get(b, 3, 5))
    var i = 0
    while (i < 5) {
      assertEquals(i + 5, b(i + 3))
      i += 1
    }
  }

  @Test def testByteBufferInputSource4(): Unit = {
    val bb = ByteBuffer.wrap(data, 0, 4)
    val bis = new ByteBufferInputSource(bb)
    assertEquals(0, bis.get())
    assertEquals(1, bis.get())
    assertEquals(2, bis.get())
    assertEquals(3, bis.get())
    assertEquals(-1, bis.get())
  }

  @Test def testByteBufferInputSource5(): Unit = {
    val bb = ByteBuffer.wrap(data, 0, 4)
    val bis = new ByteBufferInputSource(bb)
    val b = new Array[Byte](10)
    assertEquals(false, bis.get(b, 0, 10))
    assertEquals(true, bis.get(b, 0, 4))
    var i = 0
    while (i < 4) {
      assertEquals(i, b(i))
      i += 1
    }
    assertEquals(-1, bis.get())
  }

  @Test def testByteBufferInputSource6(): Unit = {
    val bb = ByteBuffer.wrap(data, 0, 17)
    val bis = new ByteBufferInputSource(bb)
    var i = 0
    while (i < 8) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.compact()
    while (i < 16) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.compact()
    assertEquals(i % 10, bis.get())
    assertEquals(-1, bis.get())
  }

  @Test def testByteBufferInputSource7(): Unit = {
    val bb = ByteBuffer.wrap(data, 0, 17)
    val bis = new ByteBufferInputSource(bb)
    var i = 0
    while (i < 2) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.lockPosition(2)
    while (i < 17) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    assertEquals(-1, bis.get())
    assertEquals(17, bis.position())

    bis.compact() // shouldn't do anything since first bucket is locked
    bis.position(2)
    assertEquals(2, bis.position())
    bis.releasePosition(2)
    bis.compact() // should'nt do anything since cur pos is at the first bucket
    i = 2
    while (i < 15) {
      assertEquals(i % 10, bis.get())
      i += 1
    }
    bis.compact() // should releaes two buckets
    assertEquals(5, bis.get())
    assertEquals(6, bis.get())
    assertEquals(-1, bis.get())
  }
}
