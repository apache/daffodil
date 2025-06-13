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

package org.apache.daffodil.lib.util

import org.apache.daffodil.lib.Implicits.intercept

import org.junit.Assert._
import org.junit.Test

class TestMisc {

  @Test def testHex2BytesInvalidLower(): Unit = {
    val e = intercept[NumberFormatException] {
      Misc.hex2Bytes("0g")
    }
    assertTrue(e.getMessage().contains("Hex character must be 0-9, a-f, or A-F"))
  }

  @Test def testHex2BytesInvalidUpper(): Unit = {
    val e = intercept[NumberFormatException] {
      Misc.hex2Bytes("0G")
    }
    assertTrue(e.getMessage().contains("Hex character must be 0-9, a-f, or A-F"))
  }

  @Test def testHex2BytesOdd(): Unit = {
    val e = intercept[NumberFormatException] {
      Misc.hex2Bytes("0")
    }
    assertTrue(e.getMessage().contains("Hex string must have an even number of characters"))
  }

  @Test def testHex2BytesValid(): Unit = {
    assertArrayEquals(Array(0).map(_.toByte), Misc.hex2Bytes("00"))
    assertArrayEquals(Array(9).map(_.toByte), Misc.hex2Bytes("09"))
    assertArrayEquals(Array(10).map(_.toByte), Misc.hex2Bytes("0A"))
    assertArrayEquals(Array(15).map(_.toByte), Misc.hex2Bytes("0F"))
    assertArrayEquals(Array(10).map(_.toByte), Misc.hex2Bytes("0a"))
    assertArrayEquals(Array(15).map(_.toByte), Misc.hex2Bytes("0f"))
    assertArrayEquals(Array(171).map(_.toByte), Misc.hex2Bytes("Ab"))
  }

  @Test def testIsAllUpper(): Unit = {
    assertTrue(Misc.isAllUpper("A", 0))
    assertFalse(Misc.isAllUpper("a", 0))
    assertTrue(Misc.isAllUpper("AB", 0))
    assertFalse(Misc.isAllUpper("Ab", 0))
    assertTrue(Misc.isAllUpper("ABC", 0))
    assertFalse(Misc.isAllUpper("ABc", 0))

    assertTrue(Misc.isAllUpper("AB", 1))
    assertFalse(Misc.isAllUpper("Ab", 1))
    assertTrue(Misc.isAllUpper("ABC", 1))
    assertFalse(Misc.isAllUpper("ABc", 1))
  }

  @Test def testToInitialLowerUnlessAllUpper(): Unit = {
    assertEquals("fooBar", Misc.toInitialLowerCaseUnlessAllUpperCase("FooBar"))
    assertEquals("FOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("FOOBAR"))
    assertEquals("fOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("fOOBAR"))
    assertEquals("foobar", Misc.toInitialLowerCaseUnlessAllUpperCase("Foobar"))
  }
}
