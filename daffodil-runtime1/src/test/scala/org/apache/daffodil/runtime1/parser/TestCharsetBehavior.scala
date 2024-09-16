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

package org.apache.daffodil.runtime1.parser

import java.io._
import java.nio._
import java.nio.charset._

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils

import Converter._
import org.junit.Assert._
import org.junit.Test

/**
 * These tests characterize behavior of the Java libraries for character sets. We're going to have to rely on
 * some detailed behaviors here, so the point of these tests is to isolate behaviors we
 * depend on. If these tests break, then there's a high likelihood that something
 * changed in a way that will break daffodil.
 */

/**
 * We're going to make use of the Unicode character U+1d4d0, which is a Mathematical Script A.
 * It requires a surrogate pair in UTF-16 which is: D835 DCD0
 * It encodes to 4 bytes in UTF-8 of: F0 9D 93 90
 *
 * We also make use of U+11000 (that's hex). This is 1 past the end of the legal Unicode character range
 * which the max code point is U+10FFFF.
 *
 * If you use the UTF-16 surrogate pair "scheme", the bits will fit, and you'll get
 * this surrogate pair: D804 DC00 (I think)
 *
 * If you encode into UTF-8, it fits in 4 bytes as these bytes: F0 91 80 80
 *
 * Also, there is 0x1FFFFF - this is the highest hex value that the UTF-8 scheme can encode in 4 bytes.
 */

object Converter {
  def convert(
    in: InputStream,
    out: OutputStream,
    inDecoder: CharsetDecoder,
    outEncoder: CharsetEncoder
  ) = {
    val i = new BufferedReader(new InputStreamReader(in, inDecoder));
    val o = new BufferedWriter(new OutputStreamWriter(out, outEncoder));
    val cb = CharBuffer.allocate(65536);

    var nCharsRead = 0;
    nCharsRead = i.read(cb);
    while (nCharsRead > 0) {
      cb.flip();
      o.append(cb);
      cb.clear();
      nCharsRead = i.read(cb);
    }
    o.flush();
  }

  def parse(in: InputStream, inDecoder: CharsetDecoder): String = {
    val r = new BufferedReader((new InputStreamReader(in, inDecoder)))
    val s = r.readLine()
    s
  }

  def unparse(out: OutputStream, outEncoder: CharsetEncoder)(s: String) = {
    val o = new BufferedWriter(new OutputStreamWriter(out, outEncoder));
    o.append(s);
    o.flush();
  }

  import scala.language.implicitConversions

  implicit def intArrayToByteArray(intArray: Array[Int]): Array[Byte] = {
    intArray.map(int => int.asInstanceOf[Byte]).toArray
  }
}

class TestUnicodeErrorTolerance {

  @Test def testIntArrayToByteArray(): Unit = {
    val ia = Array[Int](1, 127, 128, 255, 0)
    val actualByteArray = Converter.intArrayToByteArray(ia)
    val expectedByteArray = Array[Byte](1, 127, -128, -1, 0)
    val pairs = expectedByteArray.zip(actualByteArray)
    for ((exp, act) <- pairs) {
      assertEquals(exp, act)
    }
  }

  /**
   * Scala, like Java, tolerates isolated broken surrogate halves.
   */
  @Test def testScalaAllowsBadUnicode(): Unit = {
    val exp =
      "@@@\udcd0@@@" // that's the 2nd half of a surrogate pair for U+1d4d0 sandwiched between @@@
    val codepoint = exp.charAt(3)
    assertEquals(0xdcd0, codepoint)
  }

  /**
   * This test shows that we're not tolerating 3-byte encodings of surrogates if they are
   * isolated.
   */
  @Test def testUTF8Decode3ByteSurrogateIsMalformed(): Unit = {
    //    val exp = "\udcd0" // that's the trailing surrogate in the surrogate pair for U+1d4d0
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder()
    val inBuf = Array[Int]( // 3 byte encoding of 2nd half of surrogate pair for U+1d4d0
      0xed,
      0xb3,
      0x90
    )
    val input = new ByteArrayInputStream(inBuf);

    val exc = intercept[MalformedInputException] {
      Converter.parse(input, decoder)
    }
    val badLength = exc.getInputLength()
    assertEquals(3, badLength)
  }

  /**
   * This test shows that Java isn't tolerating 3-byte encodings of surrogates if they are
   * isolated. It is substituting for them.
   */
  @Test def testUTF8Decode3ByteSurrogateReplacement(): Unit = {
    val inBuf = Array[Int](0xed, 0xb3, 0x90)
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD", act)
  }

  /**
   * This test shows that Java substitutes on encoding/output also when an isolated surrogate
   * is presented to the encoder.
   */
  @Test def testUTF8Encode3ByteSurrogateReplacement(): Unit = {
    // scalafmt detects invalid surrogate pairs in unicode literals, so we can't use them like
    // we do for other tests--we must manually create the surrogate chars and append to a string
    val s = 0xd800.toChar
    val act = replaceBadCharactersEncoding("" + s)
    val exp = Array[Int](0xef, 0xbf, 0xbd) // the 3-byte UTF-8 replacement sequence
    // which is just the UTF-8 encoding of the Unicode replacement character U+FFFD.
    for ((e, a) <- exp.zip(act)) {
      assertEquals(e, a)
    }
  }

  /**
   * This test shows that Java isn't tolerating 3-byte encodings of surrogates if they are
   * isolated when encoding into utf-8 from internal Java/Scala strings.
   *
   * also shows that when there are back-to back problems in the String, that the exception
   * is thrown for the very first one.
   *
   * Also, the "length" of the malformed input is 1, as in one character that we're encoding.
   * I.e., not measured in bytes here.
   */
  @Test def testUTF8Encode3ByteSurrogateIsMalformed(): Unit = {
    val s = "\udcd0\udcd0\udcd0\udcd0" // that's the 2nd half of a surrogate pair for U+1d4d0
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val encoder = cs.newEncoder()
    //    val exp: Array[Byte] = Array[Int]( // 3 byte encoding of 2nd half of surrogate pair for U+1d4d0
    //      0xED, 0xB3, 0x90)
    val output = new ByteArrayOutputStream();

    val exc = intercept[MalformedInputException] {
      Converter.unparse(output, encoder)(s)
    }
    assertEquals(1, exc.getInputLength())
  }

  /**
   * shows that the codepoints that require surrogate pairs do in fact create two Java/Scala string codepoints.
   */
  @Test def testUTF8ToSurrogatePair(): Unit = {
    val exp = "\ud800\udc00" // surrogate pair for U+010000
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder()
    val inBuf: Array[Byte] = Array[Int](
      // 4 byte encoding of U+010000 (that's hex) which is the first character that requires a surrogate pair.
      0xf0,
      0x90,
      0x80,
      0x80
    )
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    assertEquals(exp, act)
  }

  /**
   * shows that really wild UTF-8 variants that support up to 6 bytes per character
   * aren't fully supported.
   *
   * Of course this is really extreme as there is clearly no surrogate pair represntation
   * of this code point possible.
   */
  @Test def testUTF8Extreme6ByteToSurrogatePair(): Unit = {
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder()
    val inBuf: Array[Byte] = Array[Int](
      // 6 byte encoding of \x7FFFFFFF
      0xfd, 0xbf, 0xbf, 0xbf, 0xbf, 0xbf, 0xbf
    )
    val input = new ByteArrayInputStream(inBuf);
    val e = intercept[MalformedInputException] {
      Converter.parse(input, decoder)
    }
    assertEquals(1, e.getInputLength()) // However, it doesn't say there are 6 bad bytes here.
  }

  @Test def testUTF8Extreme4ByteToSurrogatePair(): Unit = {
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder()
    val inBuf: Array[Byte] = Array[Int](
      // 4 byte encoding of \x110000
      0xf4,
      0x90,
      0x80,
      0x80
    )
    val input = new ByteArrayInputStream(inBuf);
    val e =
      intercept[MalformedInputException] { // fails to convert because there is no possible surrogate-pair rep for this.
        Converter.parse(input, decoder)
      }
    assertEquals(1, e.getInputLength())
  }

  /**
   * This test shows that Java isn't tolerating 3-byte encodings (CESU-8 encoding) of surrogates at all, it's not
   * accepting them if they are properly matched even.
   */
  @Test def testUTF8Decode6ByteSurrogatePairIsMalformed(): Unit = {
    // val exp = "\ud4d0" // that's the 2nd half of a surrogate pair for U+1d4d0
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder()
    val inBuf = Array[Int](
      // Compatibility with pre-surrogate world.
      // Character U+1d4d0, but represented as a surrogate pair, each surrogate then
      // represented as a 3-byte UTF-8 sequence. (This is an older technique).
      0xed, 0xa0, 0xb5, 0xed, 0xb3, 0x90
    )
    val input = new ByteArrayInputStream(inBuf);

    val exc = intercept[MalformedInputException] {
      Converter.parse(input, decoder)
    }
    val badLength = exc.getInputLength()
    assertEquals(3, badLength)
  }

  /**
   * This test just confirms this statement (taken from ICU web site)
   *
   * 16-bit Unicode strings in internal processing contain sequences of 16-bit code units that may
   * not always be well-formed UTF-16. ICU treats single, unpaired surrogates as surrogate code points,
   * i.e., they are returned in per-code point iteration, they are included in the number of code points
   * of a string, and they are generally treated much like normal, unassigned code points in most APIs.
   * Surrogate code points have Unicode properties although they cannot be assigned an actual character.
   *
   * ICU string handling functions (including append, substring, etc.) do not automatically protect
   * against producing malformed UTF-16 strings.
   */
  @Test def testUTF16DecodeBadSurrogate(): Unit = {
    val exp = "\ud4d0" // that's the 2nd half of a surrogate pair for U+1d4d0
    val cs = StandardCharsets.UTF_16BE
    val dn = cs.displayName()
    assertEquals("UTF-16BE", dn)
    val decoder = cs.newDecoder()
    val inBuf = Array[Int](0xd4, 0xd0) // 2nd surrogate in pair for U+1d4d0
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    assertEquals(exp, act)
  }

  /**
   * BOM's in middle of UTF-16 strings cause no problems.
   */
  @Test def testUTF16DecodeBOMsInMidString(): Unit = {
    val exp = "\uFEFF@\uFEFF@" // BOM, then @ then ZWNBS (aka BOM), then @
    val cs = StandardCharsets.UTF_16BE
    val dn = cs.displayName()
    assertEquals("UTF-16BE", dn)
    val decoder = cs.newDecoder()
    val inBuf = Array[Int](0xfe, 0xff, 0x00, 0x40, 0xfe, 0xff, 0x00, 0x40)
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    assertEquals(exp, act)
  }

  def howManyBadBytes(inBuf: Array[Byte]): Int = {

    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    var counter: Int = 0

    assertEquals("UTF-8", dn)

    val decoder = cs.newDecoder()

    val input = new ByteArrayInputStream(inBuf);
    try {
      Converter.parse(input, decoder)
    } catch {
      case e: MalformedInputException => {
        counter += e.getInputLength()
      }
    }
    counter
  }

  @Test def testHowManyBadBytes1(): Unit = {
    val inBuf = Array[Int](0xff, 0xff, 0xff) // 0xFF is always illegal utf-8
    val count = howManyBadBytes(inBuf)
    assertEquals(1, count)
  }

  @Test def testHowManyBadBytes2(): Unit = {
    val inBuf = Array[Int](0xc2, 0x00) // a bad 2-byte sequence 2nd byte bad = 1 error
    val count = howManyBadBytes(inBuf)
    assertEquals(1, count)
  }

  @Test def testHowManyBadBytes3(): Unit = {
    val inBuf = Array[Int](0xe2, 0xa2, 0xcc) // a bad 3-byte sequence 3rd byte bad = 1 error
    val count = howManyBadBytes(inBuf)
    assertEquals(2, count)
  }

  @Test def testHowManyBadBytes4(): Unit = {
    val inBuf =
      Array[Int](0xf0, 0xa4, 0xad, 0xc2) // a bad 4-byte sequence - 4th byte bad = 1 error
    val count = howManyBadBytes(inBuf)
    assertEquals(3, count)
  }

  /**
   * Here is why the below is 2 error calls.
   * It's a 4-byte sequence. It's broken at byte 2, which isn't a suitable follow on for byte 1.
   * So byte 1 is deemed an error.
   * Then it picks up with byte 2. Turns out CF BF is a valid 2-byte sequence for character U+03FF.
   * Then it should pick up at the last byte. BF by itself is not valid alone or as first byte of
   * a multi-byte character, so that's an error also. Two errors total.
   */
  @Test def testHowManyBadBytes5(): Unit = { // That's character U+10FFFF, but with an error
    val inBuf = Array[Int](0xf4, 0xcf, 0xbf, 0xbf)
    val count = howManyBadBytes(inBuf)
    assertEquals(1, count)
  }

  def replaceBadCharacters(inBuf: Array[Byte]): String = {
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPLACE)
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    act
  }

  def replaceBadCharactersEncoding(s: String): Array[Byte] = {
    val cs = StandardCharsets.UTF_8
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val encoder = cs.newEncoder()
    encoder.onMalformedInput(CodingErrorAction.REPLACE)
    val output = new ByteArrayOutputStream();
    Converter.unparse(output, encoder)(s)
    val act = output.toByteArray()
    act
  }

  @Test def testHowManyReplacements1(): Unit = {
    val inBuf = Array[Int](0xff, 0xff, 0xff) // 0xFF is always illegal utf-8
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\uFFFD\uFFFD", act)
  }

  @Test def testHowManyReplacements2(): Unit = {
    val inBuf = Array[Int](0xc2, 0x40) // a bad 2-byte sequence 2nd byte bad = 1 error
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD@", act)
  }

  @Test def testHowManyReplacements3(): Unit = {
    val inBuf = Array[Int](0xe2, 0xa2, 0xcc) // a bad 3-byte sequence. 3rd byte bad
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\uFFFD", act)
  }

  @Test def testHowManyReplacements4(): Unit = {
    val inBuf = Array[Int](0xf0, 0xa4, 0xad, 0xc2) // a bad 4-byte sequence - 4th byte bad
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\uFFFD", act)
  }

  @Test def testHowManyReplacements5(): Unit = {
    // That's character U+10FFFF, but with an error
    // a bad 4-byte sequence, 2nd byte doesn't go with first.
    // 2nd and 3rd bytes go together, but fourth is illegal on its own.
    // so 2 errors
    val inBuf = Array[Int](0xf4, 0xcf, 0xbf, 0xbf)
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\u03ff\uFFFD", act)
  }

  @Test def testHowManyReplacements6(): Unit = {
    // Gibberish. 1st byte of a 3-byte sequence, 2nd byte doesn't go with it,
    // but the sequence of 2nd and 3rd byte is valid (U+03FF). 4th byte invalid alone.
    val inBuf = Array[Int](
      0xe2,
      0xcf,
      0xbf,
      0xbf
    ) // a bad 4-byte sequence, but bad in 2nd byte. = 3 errors
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\u03ff\uFFFD", act)
  }

  /**
   * This test shows that Java ISO-8859-1 can decode any byte at all.
   */
  @Test def testISO8859HandlesAllBytes(): Unit = {
    val cs = StandardCharsets.ISO_8859_1
    val decoder = cs.newDecoder()

    val inBuf = Array[Int](0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a,
      0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
      0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
      0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46,
      0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55,
      0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x60, 0x61, 0x62, 0x63, 0x64,
      0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73,
      0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x80, 0x81, 0x82,
      0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91,
      0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xa0,
      0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
      0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe,
      0xbf, 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd,
      0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc,
      0xdd, 0xde, 0xdf, 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb,
      0xec, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa,
      0xfb, 0xfc, 0xfd, 0xfe, 0xff)
    val input = new ByteArrayInputStream(inBuf);
    val inreader = new InputStreamReader(input, decoder)
    val cb = new StringBuffer;
    for (i <- 0 to 255) cb.appendCodePoint(inreader.read())
    val act = Misc.remapStringToVisibleGlyphs(
      XMLUtils.remapXMLIllegalCharactersToPUA(cb.toString())
    )
    //
    //    val act = Converter.parse(input, decoder) // nope. Uses readLine, not just read. Trips over line endings.
    //
    val len = act.length
    assertEquals(256, len)
  }

}
