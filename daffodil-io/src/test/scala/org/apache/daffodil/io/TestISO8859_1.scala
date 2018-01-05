/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.io

import org.junit.Test
import org.junit.Assert._
import java.nio.charset._
import java.nio._

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
  @Test def test_ISO_8859_1_has256CodepointsIsomorphicToUnicodeCodepointsU0000toU00FF() {
    val byteArray = (0 to 255).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(byteArray)
    val cs = Charset.forName("iso-8859-1")
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
      case CoderResult.OVERFLOW => fail() // somewhere it consumed more than one byte per character
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