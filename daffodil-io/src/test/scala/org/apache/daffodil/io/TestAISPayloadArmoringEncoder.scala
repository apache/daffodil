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
import java.nio.CharBuffer
import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.util.Misc
import org.apache.daffodil.processors.charset.BitsCharsetAISPayloadArmoring

class TestAISPayloadArmoringEncoder {

  val finfo = FormatInfoForUnitTest()

  @Test def testAISArmoringDecoder_01(): Unit = {
    val cs = BitsCharsetAISPayloadArmoring
    val decoder = cs.newDecoder()
    assertNotNull(decoder)

    // val expected = """14eG;o@034o8sd<L9i:a;WF>062D"""
    //
    // Above example string from http://www.bosunsmate.org/ais/#bitvector
    //
    // Unfortunately, that string doesn't match the binary data. I.e., pretty
    // sure the web site is wrong.
    //
    // Look at the fifth set of 6 bits which is 011100. In the above string
    // they have ";" for this, but on the web site, the bit pattern for ";"
    // is 001011. The character for the bits given is "L".
    //
    // The corrected expected string is below.
    val expected = "14eGL:@000o8oQ'LMjOchmG@08HK"

    val cb = CharBuffer.allocate(expected.length())
    val bb = ByteBuffer.wrap(Misc.bits2Bytes(
      """000001 000100 101101 010111
          011100 001010 010000 000000
          000000 000000 110111 001000
          110111 100001 101000 011100
          011101 110010 011111 101011
          110000 110101 010111 010000
          000000 001000 011000 011011"""))
    val dis = InputSourceDataInputStream(bb)
    val res = decoder.decode(dis, finfo, cb)
    assertEquals(expected.length, res)
    cb.flip()
    val actual = cb.toString()
    assertEquals(expected, actual)
  }

  @Test def testAISArmoringEncoder_01(): Unit = {
    val cs = BitsCharsetAISPayloadArmoring
    val encoder = cs.newEncoder()
    assertNotNull(encoder)

    // val actual = """14eG;o@034o8sd<L9i:a;WF>062D"""
    //
    // Above example string from http://www.bosunsmate.org/ais/#bitvector
    //
    // Unfortunately, that string doesn't match the binary data. I.e., pretty
    // sure the web site is wrong.
    //
    // Look at the fifth set of 6 bits which is 011100. In the above string
    // they have ";" for this, but on the web site, the bit pattern for ";"
    // is 001011. The character for the bits given is "L".
    //
    // The corrected string is below.
    val actual = "14eGL:@000o8oQ'LMjOchmG@08HK"

    val cb = CharBuffer.wrap(actual)
    val expected = ByteBuffer.wrap(Misc.bits2Bytes(
      """000001 000100 101101 010111
          011100 001010 010000 000000
          000000 000000 110111 001000
          110111 100001 101000 011100
          011101 110010 011111 101011
          110000 110101 010111 010000
          000000 001000 011000 011011"""))
    val bb = ByteBuffer.allocate(expected.limit())
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isUnderflow())
    bb.flip()
    assertEquals(expected.compareTo(bb), 0)
  }
}
