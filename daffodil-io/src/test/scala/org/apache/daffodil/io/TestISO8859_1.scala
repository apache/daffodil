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

import java.nio._
import java.nio.charset._

import org.junit.Assert._
import org.junit.Test

class TestISO8859_1 {

  /**
   * Test that shows that scala/java implementations of iso-8859-1 implement all 256 byte values as one-to-one mapping
   * to Unicode code points.
   *
   * This allows one to use iso-8859-1 as a text string for an arbitrary binary blob of data no matter what bytes appear.
   *
   * This test is here, because if you look at the official (or wikipedia) definitions of iso-8859-1, there are a
   * number of unassigned code points.
   *
   * For example, according to wikipedia, 0x7F is unassigned. None of the C0 Control characters are assigned. This is
   * ludicrous as 0x7F is DEL,, 0x00 is NUL, 0x0A is LF, etc. and those are standard characters.
   *
   * More controversial, iso-8859-1 does not assign characters to 0x80-0x9F.
   *
   * Java/Scala implementations of iso-8859-1 however, reliably map byte 0x80 to Unicode U+0080, and 0x9F to U+009F and
   * similarly all the characters in between.
   *
   * The above has nothing to do with fonts and what glphs display for these characters.
   * Many of these codepoints are whitespace of various flavors.
   * There are utility functions in the utils.Misc class for mapping all 256 byte/codepoints to some printing
   * non-whitespace glyph. That transformation does NOT preserve the 256 codepoint values. E.g., all the
   * CO Control characters are mapped to the Unicode CO Control "picture" characters.
   */
  @Test def test_ISO_8859_1_has256CodepointsIsomorphicToUnicodeCodepointsU0000toU00FF()
    : Unit = {
    val byteArray = (0 to 255).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(byteArray)
    val cs = StandardCharsets.ISO_8859_1
    val decoder = cs.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    // This decoder now should stop if any character codepoint is considered malformed/not-allowed
    // or is considered to be unmapped to a unicode character.
    val cb = CharBuffer.allocate(256)
    assertTrue(cb.hasArray)
    val cr = decoder.decode(bb, cb, true)
    cr match {
      case CoderResult.UNDERFLOW => // ok. This is the expected exit
      case CoderResult.OVERFLOW =>
        fail() // somewhere it consumed more than one byte per character
      case m if m.isMalformed() => fail()
      case u if u.isUnmappable() => fail()
    }
    cb.flip
    assertEquals(256, bb.position())
    assertEquals(0, cb.position())
    assertEquals(256, cb.limit())
    val charArray = cb.array()
    val actualByteArray = charArray.map { _.toInt.toByte }
    //
    // If this test succeeds, then each unicode codepoint created
    // was in one-to-one correspondence with the byte we started from.
    assertArrayEquals(byteArray, actualByteArray)
  }

}
