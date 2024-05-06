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

package org.apache.daffodil.lib.xml.test.unit

import org.junit.Assert._
import org.junit.Test

/**
 * These tests characterize utf-8 and utf-16 character set conversion behavior
 * so that the assumptions are clear that we depend on in our code. E.g., that Byte-order-marks
 * don't cause anything to blow up, and that surrogate pairs really do work, etc.
 *
 * These things are some of the corner cases in the world of character set conversions, and
 * knowing they work appropriately lets our code be less conservative. E.g., we don't put checks
 * for various corner cases all over our code if we can show it will work right here.
 *
 * Really great converter site: http://rishida.net/tools/conversion/
 */

class TestUTF8AndUTF16Conversions {

  @Test def testDecoding(): Unit = {
    // This is dollar sign, cents sign, euro symbol,
    // and a 3-byte unicode whitespace character (which has been problematic
    // some places we think)
    val data = <data>&#x24;&#xA2;&#x20AC;&#x2028;</data>
    val str = data.child.text
    assertEquals(4, str.length)
    val bytes = str.getBytes("utf-8")
    assertEquals(9, bytes.length)
  }

  @Test def testEquality(): Unit = {
    val data = <data><d>&#x24;</d></data>
    val data2 = <data><d>$</d></data>
    assertEquals(data2, data)
  }

  @Test def testSupplementalCharSurrogates(): Unit = {
    // U+1d420 is a script capital A which is part of the unicode supplemental characters
    // which requires a surrogate pair in the JVM/Java/Scala, and 4 bytes of UTF-8 representation.
    val data = <data>&#x1d420;</data>
    val str = data.child.text
    //    println("%x".format(str(0).toInt))
    //    println("%x".format(str(1).toInt))
    assertEquals(
      2,
      str.length
    ) // 2 because that U+1d420 requires two utf-16 codepoints to represent it.
    val bytes = str.getBytes("utf-8")
    assertEquals(4, bytes.length) // encodes to 4 bytes.

  }

  /**
   * Tests that our utf-8 converting can tolerate the old style
   * where surrogate-pair characters were represented by
   * representing each surrogate code point separately in a 3-byte encoding
   * (total of 6 bytes for the character) instead of the more modern
   * (and required!) 4-byte utf-8 encoding.
   */
  @Test def testReadSurrogateCodePointsLikeCharacters(): Unit = {

    val data = <data>&#xd835;&#xdcd0;</data> // technically, this is illegal.
    val str = data.child.text
    assertEquals(2, str.length)
    val d835 = str(0).toInt
    val dcd0 = str(1).toInt
    val bytes = str.getBytes("utf-8")
    assertEquals(4, bytes.length) // If you convert back to utf-8, you get a 4-byte encoding

    val alternateOldRep: Array[Byte] = Array(
      // Compatibility with pre-surrogate world.
      // Same U+1d4d0, but represented as a surrogate pair, each surrogate then
      // represented as a 3-byte UTF-8 sequence. (This is an older technique).
      0xed.toByte,
      0xa0.toByte,
      0xb5.toByte,
      0xed.toByte,
      0xb3.toByte,
      0x90.toByte
    )
    val decodedAlt = new String(alternateOldRep, "utf-8")
    assertEquals(2, decodedAlt.length)

    assertEquals(d835, 0xd835)
    assertEquals(dcd0, 0xdcd0)

    if (scala.util.Properties.isJavaAtLeast("1.8")) {
      // Java 1.8 removed support for modified UTF-8, and so deocdedAlt is just
      // the two unicode replacement characters
      assertEquals(0xfffd, decodedAlt(0).toInt)
      assertEquals(0xfffd, decodedAlt(1).toInt)
    } else {
      assertEquals(d835, decodedAlt(0).toInt)
      assertEquals(dcd0, decodedAlt(1).toInt)
    }
  }

}
