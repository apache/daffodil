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
import org.apache.daffodil.lib.xml.RemapPUACharDetected
import org.apache.daffodil.lib.xml.RemapPUAToXMLIllegalChar
import org.apache.daffodil.lib.xml.RemapXMLIllegalCharToPUA

import org.junit.Assert._
import org.junit.Test

object TestPUARemapper {

  /**
   * For testing surrogate pairs (good ones, and isolated halves)
   *
   * I pasted the single elephant character into this string.
   * On my ubuntu linux using IntelliJ IDEA it looks like an elephant to me.
   *
   * Here is the same elephant between two vertical bars |𓃰| visible if
   * you have fantastic Unicode support
   *
   * That character is EGYPTIAN HIEROGLYPH E026
   * Hex code point	130F0
   * Hex UTF-8 bytes	F0 93 83 B0
   * Hex UTF-16 Surrogates	D80C DCF0
   */
  val elephant = "𓃰"
}

class TestPUARemapper {
  import TestPUARemapper._

  val xmlToPUANoCR2LF = new RemapXMLIllegalCharToPUA(true, false)
  val xmlToPUAUnsafe = new RemapXMLIllegalCharToPUA(false, false)
  val xmlToPUAAndCRToLF = new RemapXMLIllegalCharToPUA(true, true)

  @Test def testRemapXMLToPUAUnnecessary(): Unit = {
    val input = "no remapping needed."
    val actual = xmlToPUANoCR2LF.remap(input)
    assertTrue(actual eq input) // same exact string object.
  }

  @Test def testRemapXMLToPUAPrexistingPUAOk(): Unit = {
    val input = "abc\uE001def\u0002ghi" // one pre-existing, one to remap
    val actual = xmlToPUAUnsafe.remap(input)
    assertEquals("abc\uE001def\uE002ghi", actual) // both are in in PUA
  }

  @Test def testRemapXMLToPUAPrexistingPUAError(): Unit = {
    val input = "abc\uE001def\u0002ghi" // one pre-existing, one to remap
    val e = intercept[RemapPUACharDetected] {
      xmlToPUANoCR2LF.remap(input)
    }
    val msg = e.getMessage
    assertEquals(0xe001, e.char)
    assertTrue(msg.contains("Pre-existing Private Use Area (PUA) character"))
    assertTrue(msg.contains("U+E001"))
  }

  @Test def testRemapXMLIllegalCharToPUA(): Unit = {
    val ec = xmlToPUANoCR2LF.remap("\u0000")
    assertEquals("\uE000", ec)
    // scalafmt detects invalid surrogate pairs in unicode literals, so we can't use them like
    // we do for other tests--we must manually create the surrogate chars and append to a string
    val loneSurrogate = 0xd880.toChar
    val ed = xmlToPUANoCR2LF.remap("" + loneSurrogate)
    assertEquals("\uE880", ed)
    val crInPUA = xmlToPUANoCR2LF.remap("\u000d") // CR
    assertEquals("\uE00D", crInPUA)
    val lf = xmlToPUAAndCRToLF.remap("\u000D") // CR
    assertEquals("\u000A", lf)
  }

  @Test def testRemapXMLIllegalCharactersToPUA(): Unit = {
    val input = "nul\u0000ctrlA\u0001cr\u000d_fffe\ufffe_ffff\uffffdone"
    val actual = xmlToPUANoCR2LF.remap(input)
    assertEquals("nul\uE000ctrlA\uE001cr\uE00d_fffe\uf0fe_ffff\uf0ffdone", actual)
  }

  @Test def testRemapXMLSurrogateCharactersToPUA(): Unit = {
    assertEquals(2, elephant.length)
    assertEquals("\uD80C\uDCF0", elephant)
    // scalafmt detects invalid surrogate pairs in unicode literals, so we can't use them like
    // we do for other tests--we must manually create the surrogate chars and append to a string
    val highSurrogate = 0xd80c.toChar
    val lowSurrogate = 0xdcf0.toChar
    val input =
      "elephant" + elephant + "isolatedHigh" + highSurrogate + "isolatedLow" + lowSurrogate
    val actual = xmlToPUANoCR2LF.remap(input)
    assertEquals("elephant\uD80C\uDCF0isolatedHigh\uE80CisolatedLow\uECF0", actual)
  }

  @Test def testRemapXMLSurrogateCharactersToPUA2(): Unit = {
    // scalafmt detects invalid surrogate pairs in unicode literals, so we can't use them like
    // we do for other tests--we must manually create the surrogate chars and append to a string
    val highSurrogate = 0xd80c.toChar
    val lowSurrogate = 0xdcf0.toChar
    val input = "badSurrogateOrder lowFirst" + lowSurrogate + highSurrogate + "thenHigh"
    val actual = xmlToPUANoCR2LF.remap(input)
    assertEquals("badSurrogateOrder lowFirst\uECF0\uE80CthenHigh", actual)
  }

  @Test def testRemapXMLSurrogateCharactersToPUA3(): Unit = {
    val input = "badSurrogateOrder isolatedLowFirst\uDCF0\uD80C\uDCF0thenACorrectPair"
    val actual = xmlToPUANoCR2LF.remap(input)
    assertEquals("badSurrogateOrder isolatedLowFirst\uECF0\uD80C\uDCF0thenACorrectPair", actual)
  }

  @Test def testRemapReplacesCRLFWithLF(): Unit = {
    val input = "abc\r\ndef\r\nghi\r\njkl"
    val actual = xmlToPUAAndCRToLF.remap(input)
    assertEquals("abc\ndef\nghi\njkl", actual)
  }

  @Test def testRemapDoesNotReplaceCRLFWithLF(): Unit = {
    val input = "abc\r\ndef\r\nghi\r\njkl"
    val actual = xmlToPUANoCR2LF.remap(input)
    assertEquals("abc\uE00D\ndef\uE00D\nghi\uE00D\njkl", actual)
  }
}

class TestRemapPUAToXML() {
  import TestPUARemapper._

  val puaToXML = new RemapPUAToXMLIllegalChar()

  @Test def testRemapPUAToXMLIllegalChar(): Unit = {
    val ec = puaToXML.remap("\uE000")
    assertEquals("\u0000", ec)
    val ed = puaToXML.remap("\uE880")
    // scalafmt detects invalid surrogate pairs in unicode literals, so we can't use them like
    // we do for other tests--we must manually create the surrogate chars and append to a string
    val loneSurrogate = 0xd880.toChar
    assertEquals("" + loneSurrogate, ed)
    val cr = puaToXML.remap("\uE00D")
    assertEquals("\u000D", cr)
  }

  @Test def testRemapPUAToIllegalXMLChars(): Unit = {
    val input = "nul\uE000ctrlA\uE001cr\uE00d_fffe\uf0fe_ffff\uf0ffdone"
    val actual = puaToXML.remap(input)
    assertEquals("nul\u0000ctrlA\u0001cr\u000d_fffe\ufffe_ffff\uffffdone", actual)
  }

  @Test def testRemapPUAToSurrogateChars(): Unit = {
    // scalafmt detects invalid surrogate pairs in unicode literals, so we can't use them like
    // we do for other tests--we must manually create the surrogate chars and append to a string
    val highSurrogate = 0xd80c.toChar
    val lowSurrogate = 0xdcf0.toChar
    val input = "elephant\uD80C\uDCF0isolatedHigh\uE80CisolatedLow\uECF0"
    val actual = puaToXML.remap(input)
    assertEquals(
      "elephant" + elephant + "isolatedHigh" + highSurrogate + "isolatedLow" + lowSurrogate,
      actual
    )
  }

}
