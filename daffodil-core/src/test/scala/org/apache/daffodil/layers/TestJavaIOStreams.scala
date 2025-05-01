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

import java.io.BufferedInputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.Scanner
import scala.jdk.CollectionConverters._

import org.apache.daffodil.lib.exceptions.Assert

import org.apache.commons.io.IOUtils
import org.junit.Assert._
import org.junit.Test

/**
 * Characterizes the behavior of Java's java.io.InputStream and java.io.OutputStream
 * that we depend upon to get layering to work properly for stream-oriented
 * behaviors.
 *
 * For example, we depend on a principle called "precise consumption of bytes",
 * that is, say I am reading input from a gzip stream, which is itself consuming
 * from another input stream. We depend on the fact that the gzip stream, once it
 * finishes producing data, it has not consumed any more data from the underlying
 * input stream than is necessary to produce the gzip output.
 *
 * Under the right circumstances, and with some performance penalty (possibly substantial)
 * we can work around misbehavior here and force the proper behavior.
 *
 * Purpose of these tests is to determine if this forcing structure is needed, and
 * to insure that if the behavior of underlying Java I/O streams changes in such
 * a way that our assumptions are no longer valid, then these tests break, rather than
 * just some subtle failure in Daffodil that is hard to detect and debug.
 */
class TestJavaIOStreams {
  Assert.usage(scala.util.Properties.isJavaAtLeast("1.8"))

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
    IOUtils.write(text, gzs, StandardCharsets.ISO_8859_1)
    gzs.close()
    val bytes = baos.toByteArray()
    bytes
  }

  val additionalText = "This is text that shouldn't be read."

  /**
   * Insures that after a base64MimeDecoder is done decoding, it leaves the underlying
   * stream exactly before the next byte.
   *
   * Alas, this depends on the base64 data ending with an "=" padding character.
   * If the data happens to decode precisely (multiple of 3 characters long) without
   * any needed padding, then the decoder doesn't know to stop.
   */
  @Test def testBase64MIMEDecoderWrapDoesNotPreBuffer(): Unit = {
    val inputStream =
      IOUtils.toInputStream(b64Text + additionalText, StandardCharsets.ISO_8859_1)
    val expected = text
    val decodedStream = java.util.Base64.getMimeDecoder().wrap(inputStream)
    val lines = IOUtils.readLines(decodedStream, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(expected, lines(0))
    val additionalLines =
      IOUtils.readLines(inputStream, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(1, additionalLines.length)
    assertEquals(additionalText, additionalLines(0))
  }

  /**
   * Insures that base64MimeDecoder detects end of base64 without reading ahead.
   *
   * Does this by putting two base64 regions back to back without intervening
   * bytes. Again however, this only works because the base64 data happened
   * to end with an "=". That won't necessarily be the case.
   */
  @Test def testBase64MIMEDecoderDetectsEndRobustly1(): Unit = {
    val inputStream = IOUtils.toInputStream(b64Text + b64Text, StandardCharsets.ISO_8859_1)
    val expected = text
    val decodedStream = java.util.Base64.getMimeDecoder().wrap(inputStream)
    val lines = IOUtils.readLines(decodedStream, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(expected, lines(0))
    val decodedStream2 = java.util.Base64.getMimeDecoder().wrap(inputStream)
    val additionalLines =
      IOUtils.readLines(decodedStream2, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(expected, additionalLines(0))
  }

  @Test def testBase64ScanningForDelimiter1(): Unit = {
    val data = "cGxl" // encoding of "ple"
    val terminator = ";"
    val afterTerminator = "afterTerminator"
    val is = IOUtils
      .toInputStream(data + terminator + afterTerminator, "ascii")
      .asInstanceOf[ByteArrayInputStream]
    val scanner = new Scanner(is, StandardCharsets.ISO_8859_1.name())
    is.skip(3)
    is.mark(2)
    // Prior to changes for Java21, this test used non-capturing lookahead for the ";"
    // and group(2) was containing the match of the lookahead.
    // as of Java21 testing, the lookahead match is no longer made into a group it seems.
    // The lookahead is included in the "whole match" aka group(0)
    val matchString = scanner.findWithinHorizon("(.*?)(\\Q;\\E)", 2)
    is.reset()
    val m = scanner.`match`()
    assertEquals("l;", m.group(0))
    assertEquals("l", m.group(1))
    assertEquals(";", m.group(2))
  }

  /**
   * Characterizes behavior of GZIP input stream in Java.
   *
   * It doesn't have precise ending behavior. It reads
   * ahead at least two bytes beyond the data it needs.
   */
  @Test def testGZIPDecoderDoesPreBuffer1(): Unit = {
    val inputData = zipped ++ additionalText.getBytes(StandardCharsets.ISO_8859_1)
    val inputStream = new ByteArrayInputStream(inputData)
    val expected = text
    //
    // Even with a buffer size of 1, the gzip input stream still consumes beyond
    // the end of the stream.
    //
    val gzipBufferSize = 1
    val decodedStream = new java.util.zip.GZIPInputStream(inputStream, gzipBufferSize)
    val lines = IOUtils.readLines(decodedStream, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(1, lines.length)
    assertEquals(expected, lines(0))
    val additionalLines =
      IOUtils.readLines(inputStream, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(1, additionalLines.length)
    assertEquals(additionalText.drop(2), additionalLines(0))
  }

  /**
   * Characterizes behavior of GZIP input stream in Java.
   *
   * Same as above, but uses a BufferedInputStream - shows that GZIP isn't going
   * to reset the input back to immediately after the last required byte.
   *
   * Our conclusion is that it will require explicit length, or some way of isolating
   * the length of the compressed data so that we can limit how many bytes the gzip
   * stream can read.
   */
  @Test def testGZIPDecoderDoesPreBuffer2(): Unit = {
    val inputData = zipped ++ additionalText.getBytes(StandardCharsets.ISO_8859_1)
    val rawInput = new ByteArrayInputStream(inputData)
    val inputStream = new BufferedInputStream(rawInput)
    val expected = text
    //
    // Even with a buffer size of 1, and consuming from a buffered input stream where
    // it could, in principle, back up to push back the bytes it didn't need,
    // the gzip input stream still consumes beyond
    // the end of the data it actually needs.
    //
    val gzipBufferSize = 1
    val decodedStream = new java.util.zip.GZIPInputStream(inputStream, gzipBufferSize)
    val lines = IOUtils.readLines(decodedStream, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(1, lines.length)
    assertEquals(expected, lines(0))
    val additionalLines =
      IOUtils.readLines(inputStream, StandardCharsets.ISO_8859_1).asScala.toSeq
    assertEquals(1, additionalLines.length)
    assertEquals(additionalText.drop(2), additionalLines(0))
  }

}
