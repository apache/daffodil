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

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert

import org.apache.commons.io.IOUtils
import org.junit.Assert._
import org.junit.Test

/**
 * There are 3 variations of base64 we care about.
 *
 * Base64_RFC2045 (MIME)
 * Base64_RFC4648
 * Base64_RFC4648_URLSAFE
 *
 * When decoding from Base64, the CRLF vs. LF doesn't matter. However when
 * encoding, the RFC specify that CRLF are inserted.
 */
class TestBase64 {
  Assert.usage(scala.util.Properties.isJavaAtLeast("1.8"))

  val crlf = "\r\n"

  val text = """This is just some made up text that is intended to be
a few lines long. If this had been real text, it would not have been quite
so boring to read. Use of famous quotes or song lyrics or anything like that
introduces copyright notice issues, so it is easier to simply make up
a few lines of pointless text like this.""".replace(crlf, "\n").replace("\n", " ")

  val b64Text = """VGhpcyBpcyBqdXN0IHNvbWUgbWFkZSB1cCB0ZXh0IHRoYXQgaXMgaW50Z
W5kZWQgdG8gYmUgYSBmZXcgbGluZXMgbG9uZy4gSWYgdGhpcyBoYWQgYmVlbiByZWFsIHRleHQsI
Gl0IHdvdWxkIG5vdCBoYXZlIGJlZW4gcXVpdGUgc28gYm9yaW5nIHRvIHJlYWQuIFVzZSBvZiBmY
W1vdXMgcXVvdGVzIG9yIHNvbmcgbHlyaWNzIG9yIGFueXRoaW5nIGxpa2UgdGhhdCBpbnRyb2R1Y
2VzIGNvcHlyaWdodCBub3RpY2UgaXNzdWVzLCBzbyBpdCBpcyBlYXNpZXIgdG8gc2ltcGx5IG1ha
2UgdXAgYSBmZXcgbGluZXMgb2YgcG9pbnRsZXNzIHRleHQgbGlrZSB0aGlzLg==
""".replace(crlf, "\n").replace("\n", "").sliding(76, 76).mkString(crlf)

  /**
   * Same encoded data, but shot through with extra CRLFs
   */
  val b64TextExtraLFs = b64Text.split("Z").mkString("Z\r\n")

  @Test def testBase64_01(): Unit = {
    val input = text
    val expected = b64Text
    val encoded = java.util.Base64.getMimeEncoder.encodeToString(input.getBytes("ascii"))
    assertEquals(expected.length, encoded.length)
    val pairs = expected.zip(encoded)
    var i = 0
    var failed = false
    pairs.foreach {
      case (exp, act) => {
        if (exp != act) {
          failed = true
        }
        i += 1
      }
    }
    val textDecoded = new String(java.util.Base64.getMimeDecoder.decode(encoded))
    assertEquals(input, textDecoded)
    if (failed) fail()
  }

  @Test def testBase64_broken_data_01(): Unit = {

    val data = b64Text.tail

    intercept[IllegalArgumentException] {
      java.util.Base64.getMimeDecoder.decode(data)
    }

  }

  @Test def testBase64_broken_data_02(): Unit = {

    val data = b64Text.dropRight(3)

    intercept[IllegalArgumentException] {
      java.util.Base64.getMimeDecoder.decode(data)
    }

  }

  @Test def testBase64_tolerates_extra_CRLFs(): Unit = {
    val data = b64TextExtraLFs
    val textDecoded = new String(java.util.Base64.getMimeDecoder.decode(data))
    assertEquals(text, textDecoded)
  }

  @Test def testBase64_decode_consumes_final_CRLF(): Unit = {

    val data = b64Text ++ crlf // add extra CRLF

    val actual = new String(java.util.Base64.getMimeDecoder.decode(data))

    assertEquals(text, actual)
  }

  @Test def testBase64_decode_consumes_final_LF(): Unit = {

    val data = b64Text ++ "\n" // add extra LF

    val actual = new String(java.util.Base64.getMimeDecoder.decode(data))

    assertEquals(text, actual)
  }

  /**
   * Base64 decoder that decodes strings consumes any trailing CRLFs or LFs
   * and ignores them.
   */
  @Test def testBase64_decode_consumes_final_LFLFLFLF(): Unit = {

    val data = b64Text ++ "\n\n\n\n" // add extra LF

    val actual = new String(java.util.Base64.getMimeDecoder.decode(data))

    assertEquals(text, actual)
  }

  /**
   * This test shows us that if we wrap the base64 decoder around a
   * java input stream, and read as much as we can from it, that it will stop
   * at an equals sign, and tolerates additional equals signs even.
   */
  @Test def testBase64_decode_from_stream_consumes_nothing_extra(): Unit = {

    val additional = "====ABCD"
    val data = b64Text ++ additional // add extra characters

    val is = IOUtils.toInputStream(data, "ascii")
    val b64 = java.util.Base64.getMimeDecoder().wrap(is)
    val actual = IOUtils.toString(b64, "ascii")
    assertEquals(text, actual)
    b64.close()
    val leftOver = IOUtils.toString(is, "ascii")

    assertEquals(additional, leftOver)
  }

  /**
   * This test shows us that if we wrap the base64 decoder around a
   * java input stream, and read from an encoding that has no padding so
   * no equals-signs on the end. Then it will NOT stop and will fail
   * trying to read past that end.
   *
   * This tells us that base64 is, generally speaking, not self-delimiting.
   */
  @Test def testBase64_decode_from_stream_does_not_stop_by_itself(): Unit = {

    val data = "cGxlYXN1cmUu" ++ "=" // encoding of "pleasure."

    val is = IOUtils.toInputStream(data, "ascii")
    val b64 = java.util.Base64.getMimeDecoder().wrap(is)
    val ex = intercept[java.io.IOException] {
      IOUtils.toString(b64, "ascii")
    }
    val msg = ex.getMessage()
    assertTrue(msg.toLowerCase().contains("illegal base64 ending sequence"))
  }

  /**
   * Even if there is a LF there. And the line is shorter than the spec
   * maximum.
   *
   * It behaves as if it removes all the line endings first, and then decodes.
   */
  @Test def testBase64_decode_from_stream_does_not_stop_by_itself2(): Unit = {

    val data = "cGxlYXN1cmUu" ++ "\n=" // encoding of "pleasure."

    val is = IOUtils.toInputStream(data, "ascii")
    val b64 = java.util.Base64.getMimeDecoder().wrap(is)
    val ex = intercept[java.io.IOException] {
      IOUtils.toString(b64, "ascii")
    }
    val msg = ex.getMessage()
    assertTrue(msg.toLowerCase().contains("illegal base64 ending sequence"))
  }

  def compare(input: String, expected: String) = {
    val encoded = java.util.Base64.getMimeEncoder.encodeToString(input.getBytes("ascii"))
    assertEquals(expected.length, encoded.length)
    val pairs = expected.zip(encoded)
    var i = 0
    var failed = false
    pairs.foreach {
      case (exp, act) => {
        if (exp != act) {
          failed = true
        }
        i += 1
      }
    }
    val textDecoded = new String(java.util.Base64.getMimeDecoder.decode(encoded))
    assertEquals(input, textDecoded)
    if (failed) fail()
  }

  /**
   * Zero-length string encodes to zero length string.
   */
  @Test def testBase64_0Byte(): Unit = {
    compare("", "")
  }

  /**
   * If length is 1 mod 3, then there will be "==" after.
   */
  @Test def testBase64_1Byte(): Unit = {
    compare("Q", "UQ==")
  }

  /**
   * If length is 2 mod 3, then there will be "=" after.
   */
  @Test def testBase64_2Byte(): Unit = {
    compare("QQ", "UVE=")
  }

  /**
   * If length is 0 mod 3, then there will be no trailing characters.
   */
  @Test def testBase64_3Byte(): Unit = {
    compare("QQQ", "UVFR")
  }

}
