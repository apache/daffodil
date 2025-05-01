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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import org.apache.daffodil.io.BoundaryMarkLimitingInputStream
import org.apache.daffodil.io.RegexLimitingInputStream
import org.apache.daffodil.lib.exceptions.Assert

import org.apache.commons.io.IOUtils
import org.junit.Assert._
import org.junit.Test

/**
 * Tests our layering java io streams. These are supposed to implement
 * a principle called "precise consumption of bytes",
 * that is, say I am reading input from a gzip stream, which is itself consuming
 * from another input stream. We depend on the fact that the gzip stream, once it
 * finishes producing data, it has not consumed any more data from the underlying
 * input stream than is necessary to produce the gzip output.
 *
 * The layering streams have to constrain the data available to these overlying
 * layer streams in order to insure the precise consumptino of bytes principle is
 * upheld, because the java io streams for Base64, gzip, etc. do internal buffering
 * and such, so are not compatible with this principle by themselves.
 *
 */
class TestLimitingJavaIOStreams {
  Assert.usage(scala.util.Properties.isJavaAtLeast("1.8"))

  val iso8859 = StandardCharsets.ISO_8859_1
  val utf8 = StandardCharsets.ISO_8859_1

  val crlfRegex = "\\r\\n(?!(?:\\t|\\ ))"
  val crlf = "\r\n"

  val text = """Daffodil is an open source implementation of the DFDL
specification that uses these DFDL schemas to parse fixed format data into an
infoset, which is most commonly represented as either XML or JSON. This
allows the use of well-established XML or JSON technologies and libraries to
consume, inspect, and manipulate fixed format data in existing solutions."""
    .replace(crlf, "\n")
    .replace("\n", " ")

  val b64Text = """RGFmZm9kaWwgaXMgYW4gb3BlbiBzb3VyY2UgaW1wbGVtZW50YXRpb24gb2YgdGhlIERGREwgc3Bl
Y2lmaWNhdGlvbiB0aGF0IHVzZXMgdGhlc2UgREZETCBzY2hlbWFzIHRvIHBhcnNlIGZpeGVkIGZv
cm1hdCBkYXRhIGludG8gYW4gaW5mb3NldCwgd2hpY2ggaXMgbW9zdCBjb21tb25seSByZXByZXNl
bnRlZCBhcyBlaXRoZXIgWE1MIG9yIEpTT04uIFRoaXMgYWxsb3dzIHRoZSB1c2Ugb2Ygd2VsbC1l
c3RhYmxpc2hlZCBYTUwgb3IgSlNPTiB0ZWNobm9sb2dpZXMgYW5kIGxpYnJhcmllcyB0byBjb25z
dW1lLCBpbnNwZWN0LCBhbmQgbWFuaXB1bGF0ZSBmaXhlZCBmb3JtYXQgZGF0YSBpbiBleGlzdGlu
ZyBzb2x1dGlvbnMuCg=="""

  val zipped = {
    val baos = new ByteArrayOutputStream()
    val gzs = new java.util.zip.GZIPOutputStream(baos)
    IOUtils.write(text, gzs, iso8859)
    gzs.close()
    val bytes = baos.toByteArray()
    bytes
  }

  val additionalText = "This is text that shouldn't be read."

  @Test def testBase64DecodeFromDelimitedStream1(): Unit = {
    val data = "c29sdXRpb25zLg" // encoding of "solutions."
    val terminator = "terminator"
    val afterTerminator = "afterTerminator"
    val is = IOUtils
      .toInputStream(data + terminator + afterTerminator, StandardCharsets.UTF_8)
      .asInstanceOf[ByteArrayInputStream]
    val delimitedStream =
      new BoundaryMarkLimitingInputStream(is, terminator, iso8859, targetChunkSize = 4)
    val b64 = java.util.Base64.getMimeDecoder().wrap(delimitedStream)
    val actualData = IOUtils.toString(b64, iso8859)
    assertEquals("solutions.", actualData)
    // The input stream should have been advanced past the terminator
    val actualAfterData = IOUtils.toString(is, iso8859)
    assertEquals(afterTerminator, actualAfterData)
  }

  @Test def testBase64DecodeFromDelimitedStream2(): Unit = {
    val data = "c29sdXRpb25zLg" // encoding of "solutions."
    val terminator = ";;;"
    val afterTerminator = "afterTerminator"
    val is = IOUtils
      .toInputStream(data + terminator + afterTerminator, "ascii")
      .asInstanceOf[ByteArrayInputStream]
    val delimitedStream =
      new BoundaryMarkLimitingInputStream(is, terminator, iso8859, targetChunkSize = 1)
    val b64 = java.util.Base64.getMimeDecoder().wrap(delimitedStream)
    val actualData = IOUtils.toString(b64, iso8859)
    assertEquals("solutions.", actualData)
    val actualAfterData = IOUtils.toString(is, iso8859)
    assertEquals(afterTerminator, actualAfterData)
  }

  @Test def testBase64DecodeFromDelimitedStream3(): Unit = {
    val data = "cGxl" // encoding of "ple"
    val terminator = ";"
    val afterTerminator = "afterTerminator"
    val is = IOUtils
      .toInputStream(data + terminator + afterTerminator, "ascii")
      .asInstanceOf[ByteArrayInputStream]
    val delimitedStream =
      new BoundaryMarkLimitingInputStream(is, terminator, iso8859, targetChunkSize = 1)
    val b64 = java.util.Base64.getMimeDecoder().wrap(delimitedStream)
    val actualData = IOUtils.toString(b64, iso8859)
    assertEquals("ple", actualData)
    val actualAfterData = IOUtils.toString(is, iso8859)
    assertEquals(afterTerminator, actualAfterData)
  }

  /**
   * Test for lines of text containing \r\n\t or \r\n\x20 (CRLF + space)
   * but terminated by a \r\n NOT followed by \t or space.
   */
  @Test def testRegexDelimStream1() = {
    val beforeDelim = "12345\r\n\t67890\r\n\tabcde"
    val delim = crlf
    val afterDelim = "fghij"
    val inputString = beforeDelim + delim + afterDelim
    val inputBytes = inputString.getBytes("utf-8")
    val bais = new ByteArrayInputStream(inputBytes)
    val rls = new RegexLimitingInputStream(bais, crlfRegex, crlf, utf8)
    val actualBeforeDelim = IOUtils.toString(rls, utf8)
    // after the regex match the bais stream should be positioned immediately after the \r\n delim match.
    val actualAfterDelim = IOUtils.toString(bais, utf8)
    assertEquals(beforeDelim, actualBeforeDelim)
    assertEquals(afterDelim, actualAfterDelim)
  }

  /**
   * Shows that the match, if the delim regex isn't matched at all,
   * contains the entire available input data.
   */
  @Test def testRegexDelimStream2() = {
    val beforeDelim = "12345\r\n\t67890\r\n\tabcde"
    val inputString = beforeDelim
    val inputBytes = inputString.getBytes("utf-8")
    val bais = new ByteArrayInputStream(inputBytes)
    val rls = new RegexLimitingInputStream(bais, crlfRegex, "\r\n\t", utf8)
    val actualBeforeDelim = IOUtils.toString(rls, utf8)
    assertEquals(beforeDelim, actualBeforeDelim)
  }

  /**
 * Shows that the match if the delim is immediately followed by end-of-data
 * that also matches.
 */
  @Test def testRegexDelimStream3() = {
    val beforeDelim = "12345\r\n\t67890\r\n\tabcde"
    val inputString = beforeDelim + crlf
    val inputBytes = inputString.getBytes("utf-8")
    val bais = new ByteArrayInputStream(inputBytes)
    val rls = new RegexLimitingInputStream(bais, crlfRegex, "\r\n\t", utf8)
    val actualBeforeDelim = IOUtils.toString(rls, utf8)
    assertEquals(beforeDelim, actualBeforeDelim)
  }

  /**
   * Shows that the match if the delim is never found
   */
  @Test def testRegexDelimStream4() = {
    val beforeDelim = "1234567890tabcde"
    val inputString = beforeDelim
    val inputBytes = inputString.getBytes("utf-8")
    val bais = new ByteArrayInputStream(inputBytes)
    val rls = new RegexLimitingInputStream(bais, crlfRegex, "\r\n\t", utf8)
    val actualBeforeDelim = IOUtils.toString(rls, utf8)
    assertEquals(beforeDelim, actualBeforeDelim)
  }
}
