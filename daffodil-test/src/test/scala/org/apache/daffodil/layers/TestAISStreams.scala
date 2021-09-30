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

package org.apache.daffodil.layers

import org.junit.Assert._

import java.io._
import org.junit.Test
import org.apache.daffodil.io.RegexLimitingStream

import java.nio.charset.StandardCharsets
import org.apache.daffodil.util.Misc
import org.apache.commons.io.IOUtils
import org.apache.daffodil.io.LayerBoundaryMarkInsertingJavaOutputStream

/**
 * AISPayloadArmoring Stream unit tests
 *
 * Results verified against this site: http://www.bosunsmate.org/ais/#bitvector
 * However, that site has the data string wrong. The proper dataString
 * is below.
 */
class TestAISPayloadArmoringStreams {

  val iso8859 = StandardCharsets.ISO_8859_1

  /**
   * Shows that the regex will limit length and then AISPayloadArmoring decode does what
   * it is allowed to decode.
   */
  @Test def testAISPayloadArmoringDecode() = {
    val dataString = "14eGL:@000o8oQ'LMjOchmG@08HK,"
    val bba = new ByteArrayInputStream(dataString.getBytes(iso8859))
    //
    // regex is ",0*"
    //
    val rls = new RegexLimitingStream(bba, ",", ",", iso8859)
    val aas = new AISPayloadArmoringInputStream(rls)

    val baos = new ByteArrayOutputStream()
    var c: Int = -1
    while ({
      c = aas.read()
      c != -1
    }) {
      baos.write(c)
    }
    baos.close()
    val result = baos.toByteArray()
    val expected = Misc.bits2Bytes("000001 000100 101101 010111 011100 001010 010000 000000 000000 000000 110111 001000 110111 100001 101000 011100 011101 110010 011111 101011 110000 110101 010111 010000 000000 001000 011000 011011 ")
    assertEquals(expected.length, result.length)
    val pairs = expected zip result
    pairs.foreach {
      case (exp, act) =>
        assertEquals(exp, act)
    }
  }

  @Test def testAISPayloadArmoringEncode() = {
    val dataBytes = Misc.bits2Bytes("000001 000100 101101 010111 011100 001010 010000 000000 000000 000000 110111 001000 110111 100001 101000 011100 011101 110010 011111 101011 110000 110101 010111 010000 000000 001000 011000 011011 ")
    val bais = new ByteArrayInputStream(dataBytes)
    val baos = new ByteArrayOutputStream()
    val lbmijos = new LayerBoundaryMarkInsertingJavaOutputStream(baos, ",", iso8859)
    val aas = new AISPayloadArmoringOutputStream(lbmijos)
    IOUtils.copy(bais, aas)
    aas.close()
    val result = baos.toByteArray()
    val expected = "14eGL:@000o8oQ'LMjOchmG@08HK,".getBytes(iso8859)
    assertEquals(expected.length, result.length)
    (expected zip result).foreach {
      case (exp, act) =>
        assertEquals(exp, act)
    }
  }

}
