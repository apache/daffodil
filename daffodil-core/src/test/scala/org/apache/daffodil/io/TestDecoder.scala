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

//
// KEEP THIS FILE IN CASE WE HAVE TO GO BACK TO SUPPORTING JAVA 7
//
package org.apache.daffodil.io

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CoderResult
import java.nio.charset.CodingErrorAction
//
import java.nio.charset.StandardCharsets

import org.junit.Assert._
import org.junit.Test

/**
 * These unit tests characterize the behavior of charset decoders
 * that we depend upon.
 *
 * There appear to be some bugs in Java 7 that are fixed in Java 8.
 * But to work around them we have a DecoderWrapper class.
 *
 * The bug is specifically associated with
 * a use case we need, which is the case where there is room in the
 * output CharBuffer for exactly 1 character.
 *
 */
class TestDecoder {

  /**
   * We no longer use the CharsetICU decoders directly, nor have those libraries
   * on the class path.
   *
   * The standard java decoders have slightly different behavior
   * even in Java 8.
   *
   * This test shows that even if there is room in the char buffer for a character,
   * that sometimes it returns OVERFLOW, but has not advanced the position of
   * the byte buffer or the char buffer. If there is room for 2 characters, the
   * same input decodes to a Malformed[3].
   *
   * This happens because UTF-8 has these 4-byte characters that
   * turn into two 16bit characters called the high surrogate (or leading)
   * and the low surrogate (or trailing).
   *
   * If there isn't room for 2 in the char buffer, then a bad encoding
   * with 3 of the 4 bytes of a 4-byte code, followed by a broken 4th byte
   * dosen't produce an error.
   *
   * It seems they don't check for the decode error until after they've
   * checked for enough room for a surrogate pair.
   */
  @Test def testDecoder1(): Unit = {
    val originalDecoder = StandardCharsets.UTF_8.newDecoder()
    originalDecoder.onMalformedInput(CodingErrorAction.REPORT)
    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val decoder = originalDecoder
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
    var flushCR = decoder.flush(cb)
    //
    // We don't even get the error here
    //
    assertEquals(CoderResult.UNDERFLOW, flushCR)
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
    assertEquals(0, cb.position())
    assertEquals(0, bb.position())
    //
    flushCR = decoder.flush(cb)
    assertEquals(CoderResult.UNDERFLOW, flushCR)
    assertEquals(0, cb.position())
    assertEquals(0, bb.position())
  }

  @Test def testDecoderWorkaround1(): Unit = {
    val originalDecoder = StandardCharsets.UTF_8.newDecoder()
    originalDecoder.onMalformedInput(CodingErrorAction.REPORT)
    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val decoder = originalDecoder
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
    assertEquals(0, tempCB.position())
    assertEquals(0, bb.position())

    val flushCR = decoder.flush(cb)
    assertEquals(0, tempCB.position())
    assertEquals(0, bb.position())

    assertEquals(CoderResult.UNDERFLOW, flushCR)
    //
    // now it detects an error.
    //
    assertTrue(decodeCR.isMalformed())
    assertEquals(3, decodeCR.length())
    assertEquals(0, cb.position())
    assertEquals(0, bb.position())
    //

  }

  // KEEP THESE TESTS IN CASE WE EVER HAVE TO SUPPORT JAVA 7
  // Delete once it's clear we're never going to have to do that.
  //
  //  @Test def testDecoder2 {
  //    val originalDecoder = StandardCharsets.UTF_8.newDecoder()
  //    originalDecoder.onMalformedInput(CodingErrorAction.REPORT)
  //    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPORT)
  //    val decoder = DecoderWrapper(originalDecoder)
  //    val bb = ByteBuffer.allocate(6)
  //    bb.put(-16.toByte) // invalid first utf-8 byte
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.flip
  //    val cb = CharBuffer.allocate(1) // allow room for exactly one character.
  //    val decodeCR = decoder.decode(bb, cb, true)
  //    assertEquals(CoderResult.malformedForLength(1), decodeCR)
  //    val flushCR = decoder.flush(cb)
  //    assertEquals(CoderResult.UNDERFLOW, flushCR)
  //    assertEquals(0, cb.position())
  //    assertEquals(0, bb.position())
  //  }
  //
  //  @Test def testDecoder3 {
  //    val originalDecoder = StandardCharsets.UTF_8.newDecoder()
  //    originalDecoder.onMalformedInput(CodingErrorAction.REPLACE)
  //    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
  //    val decoder = DecoderWrapper(originalDecoder)
  //    val bb = ByteBuffer.allocate(6)
  //    bb.put(-16.toByte) // invalid first utf-8 byte
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.put(0.toByte)
  //    bb.flip
  //    val cb = CharBuffer.allocate(1) // allow room for exactly one character.
  //    val decodeCR = decoder.decode(bb, cb, true)
  //    assertEquals(CoderResult.OVERFLOW, decodeCR)
  //    val flushCR = decoder.flush(cb)
  //    assertEquals(CoderResult.UNDERFLOW, flushCR)
  //    assertEquals(1, cb.position())
  //    assertEquals(1, bb.position())
  //    cb.flip
  //    assertEquals(decoder.unicodeReplacementChar, cb.get())
  //  }
  //
  //  @Test def testDecoder4 {
  //    val originalDecoder = StandardCharsets.UTF_8.newDecoder()
  //    originalDecoder.onMalformedInput(CodingErrorAction.REPLACE)
  //    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
  //    val decoder = DecoderWrapper(originalDecoder)
  //    val bb = ByteBuffer.wrap("日".getBytes("utf-8"))
  //    val cb = CharBuffer.allocate(1) // allow room for exactly one character.
  //    val decodeCR = decoder.decode(bb, cb, true)
  //    //
  //    // If all available bytes are used up, then we get an UNDERFLOW even if
  //    // every location in the output is filled in. Because until we get more bytes,
  //    // we can't be overflowing the input buffer.
  //    //
  //    assertEquals(CoderResult.UNDERFLOW, decodeCR)
  //    val flushCR = decoder.flush(cb)
  //    assertEquals(CoderResult.UNDERFLOW, flushCR)
  //    assertEquals(1, cb.position())
  //    assertEquals(3, bb.position())
  //    cb.flip
  //    assertEquals('日', cb.get())
  //  }
}
