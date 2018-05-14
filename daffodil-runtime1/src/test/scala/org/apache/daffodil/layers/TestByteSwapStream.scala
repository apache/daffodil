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

import junit.framework.Assert._
import java.io._
import org.junit.Test
import java.nio.charset.StandardCharsets

class TestByteSwapStreams {

  val iso8859 = StandardCharsets.ISO_8859_1

  val unswapped32BitData = Array[Byte](0x76, 0x54, 0x32,        0x10,
                                                   0xBA.toByte, 0x98.toByte)

  val swapped32BitData = Array[Byte](0x10, 0x32, 0x54, 0x76, 0x98.toByte, 0xBA.toByte)

  @Test def testFourByteSwapInputStream() = {
    val data = unswapped32BitData
    val bba = new ByteArrayInputStream(data)
    val bss = new ByteSwapInputStream(4, bba)

    val baos = new ByteArrayOutputStream()
    var c: Int = -1
    while ({
      c = bss.read()
      c != -1
    }) {
      baos.write(c)
    }
    baos.close()
    val result = new String(baos.toByteArray(), iso8859)
    val expected = new String(swapped32BitData, iso8859)
    assertEquals(expected, result)
  }

  @Test def testFourByteSwapOutputStream() = {
    val data = swapped32BitData
    val bba = new ByteArrayInputStream(data)

    val baos = new ByteArrayOutputStream()
    val bsos = new ByteSwapOutputStream(4, baos)
    var c: Int = -1
    while ({
      c = bba.read()
      c != -1
    }) {
      bsos.write(c)
    }
    bsos.close()
    baos.close()

    val result = new String(baos.toByteArray(), iso8859)
    val expected = new String(unswapped32BitData, iso8859)
    assertEquals(expected, result)
  }

}
