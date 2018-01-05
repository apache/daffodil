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
package edu.illinois.ncsa.daffodil.processors.charset

import java.nio.charset.CodingErrorAction
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CoderResult
import org.junit.Test
import org.junit.Assert._

class TestBitsCharset {

  @Test def testDecoder0 {
    val jbcs = new BitsCharsetWrappingJavaCharset("utf-8")
    val decoder = jbcs.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val bytes = Seq(0x31, 0x32, 0x33, 0x34, 0x35).map { _.toByte }
    val bb = ByteBuffer.wrap(bytes.toArray)
    val cb = CharBuffer.allocate(5)
    cb.limit(4)

    val decodeCR = decoder.decode(bb, cb, true)
    //
    // We get an OVERFLOW
    //
    assertEquals(CoderResult.OVERFLOW, decodeCR)
    assertEquals(4, cb.position())
    assertEquals(4, bb.position())

    assertEquals("1234", cb.flip().toString())
  }

  @Test def testDecoder1 {
    val jbcs = new BitsCharsetWrappingJavaCharset("utf-8")
    val decoder = jbcs.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    //
    // This byte sequence "needs" the fourth byte. The first 3 bytes are consistent
    // with the 4-byte utf-8 bytes. However, the last byte here is not a valid
    // last byte of a 4-byte representation.
    //
    val bytes = Seq(0xf0, 0x90, 0x87, 0x67).map { _.toByte }
    val bb = ByteBuffer.wrap(bytes.toArray)
    val cb = CharBuffer.allocate(2)
    cb.limit(1) // room for only 1 character

    var decodeCR = decoder.decode(bb, cb, true)
    //
    // We get an OVERFLOW with nothing consumed or produced
    //
    assertEquals(CoderResult.OVERFLOW, decodeCR)
    assertEquals(0, cb.position())
    assertEquals(0, bb.position())
    //
    // Note that even though we passed 'true' meaning no more data possible,
    // running out of data wasn't the issue.
    //
    // It appears Java's decoder for utf-8 is deciding that it WILL need
    // to create two characters (a surrogate pair) becuase that first utf-8 byte of 0xF0
    // indicates 4 bytes.
    //
    // At that point it checks if there is room for the two surrogate code units,
    // finds that there is not, and returns OVERFLOW.
    //
    // Note: It isn't legal for a utf-8 decoder to return only 1 half of a
    // surrogate pair, because what bytes would it indicate were consumed in that case?
    // It can't say zero, because then there is no forward progress. It can't say
    // any of them were consumed, because then they won't be there to decode
    // in order to figure out the other half of the surrogate pair.
    //
    // It *could* have decided to check if the 4-bytes actually decoded correctly or not
    // and then even though there was only room for 1 half of the surrogate pair, then
    // issued the Malformed[3]. It doesn't do that though.
    //

    var flushCR = decoder.flush(cb) // causes IllegalStateException

    //
    // We don't even get the error here
    //
    // assertEquals(CoderResult.UNDERFLOW, flushCR)

    //
    // Start over
    //
    decoder.reset()
    cb.limit(2) // room for 2 characters
    decodeCR = decoder.decode(bb, cb, true)
    //
    // now it detects an error.
    //
    assertTrue(decodeCR.isMalformed())
    assertEquals(3, decodeCR.length())
    assertEquals(0, cb.position)
    assertEquals(0, bb.position)
    //
    flushCR = decoder.flush(cb)
    assertEquals(CoderResult.UNDERFLOW, flushCR)
    assertEquals(0, cb.position)
    assertEquals(0, bb.position)
  }

  @Test def testDecoderWorkaround1 {
    val jbcs = new BitsCharsetWrappingJavaCharset("utf-8")
    val decoder = jbcs.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    //
    // That initial 0xF0 tells the UTF-8 decoder that
    // it should expect a 4-byte character. That means it
    // is going to try to create a surrogate pair (if it
    // decodes correctly.)
    //
    val bytes = Seq(0xf0, 0x90, 0x87, 0x67).map { _.toByte }
    val bb = ByteBuffer.wrap(bytes.toArray)
    val cb = CharBuffer.allocate(2)
    cb.limit(1) // room for only 1 character
    var decodeCR = decoder.decode(bb, cb, true)
    //
    // result is strange, OVERFLOW with nothing consumed or produced
    //
    assertEquals(CoderResult.OVERFLOW, decodeCR)
    assertEquals(0, cb.position())
    assertEquals(0, bb.position())
    //
    //
    val tempCB = CharBuffer.allocate(2)
    decodeCR = decoder.decode(bb, tempCB, true)

    assertTrue(decodeCR.isError)
    assertEquals(0, tempCB.position)
    assertEquals(0, bb.position)

    val flushCR = decoder.flush(cb)
    assertEquals(0, tempCB.position)
    assertEquals(0, bb.position)

    assertEquals(CoderResult.UNDERFLOW, flushCR)
    //
    // now it detects an error.
    //
    assertTrue(decodeCR.isMalformed())
    assertEquals(3, decodeCR.length())
    assertEquals(0, cb.position)
    assertEquals(0, bb.position)
    //
  }

}
