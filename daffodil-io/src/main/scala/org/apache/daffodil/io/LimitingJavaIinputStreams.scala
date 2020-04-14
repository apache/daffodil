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

import org.apache.daffodil.exceptions.Assert
import java.util.regex.Pattern
import java.util.Scanner
import java.nio.charset.Charset
import java.io.InputStream
import java.io.FilterInputStream
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets

/**
 * This class can be used with any InputStream to restrict what is
 * read from it to N bytes.
 *
 * This can be used to forcibly stop consumption of data from a stream at
 * a length obtained explicitly.
 *
 * Thread safety: This is inherently stateful - so not thread safe to use
 * this object from more than one thread.
 */
class ExplicitLengthLimitingStream(in: InputStream, limit: Int)
  extends FilterInputStream(in) {

  private var numRemaining = limit

  override def read(buf: Array[Byte], off: Int, len: Int) = {
    Assert.invariant(numRemaining >= 0)
    if (numRemaining == 0) -1
    else if (len == 0) 0
    else {
      val requestSize = math.min(numRemaining, len)
      val actualSize = in.read(buf, off, requestSize)
      if (actualSize == -1)
        numRemaining = 0
      else
        numRemaining -= actualSize
      actualSize
    }
  }

  private val readBuf = new Array[Byte](1)

  override def read(): Int = {
    readBuf(0) = 0
    val n = read(readBuf, 0, 1)
    if (n == -1)
      -1
    else {
      Assert.invariant(n == 1)
      val i = readBuf(0).toInt
      val b = i & 0xFF
      b
    }
  }
}

/**
 * Can be used with any InputStream to restrict what is
 * read from it to stop before a boundary mark string.
 *
 * The boundary mark string is exactly that, a string of characters. Not a
 * regex, nor anything involving DFDL Character Entities or Character Class
 * Entities. (No %WSP; no %NL; )
 *
 * This can be used to forcibly stop consumption of data from a stream at
 * a length obtained from a delimiter.
 *
 * The boundary mark string is consumed from the underlying stream (if found), and
 * the underlying stream is left positioned at the byte after the boundary mark
 * string.
 *
 * Thread safety: This is inherently stateful - so not thread safe to use
 * this object from more than one thread.
 */
object BoundaryMarkLimitingStream {

  def apply(inputStream: InputStream, boundaryMark: String, charset: Charset,
    targetChunkSize: Int = 32 * 1024) = {

    Assert.usage(targetChunkSize >= 1)
    Assert.usage(boundaryMark.length >= 1)

    val boundaryMarkIn8859 = new String(boundaryMark.getBytes(charset), StandardCharsets.ISO_8859_1)

    val quotedBoundaryMark = Pattern.quote(boundaryMarkIn8859) // in case pattern has non-regex-safe characters in it

    val result = new RegexLimitingStream(inputStream, quotedBoundaryMark, boundaryMarkIn8859, charset, targetChunkSize)
    result
  }

}

class StreamIterator[T](s: Stream[T])
  extends Iterator[T] {
  private var str = s
  override def hasNext = !str.isEmpty
  override def next() = {
    val res = str.head
    str = str.tail
    res
  }
}

/**
 * Can be used with any InputStream to restrict what is
 * read from it to stop before a particular regex match.
 *
 * The regex must have a finite maximum length match string.
 *
 * This can be used to forcibly stop consumption of data from a stream at
 * a length obtained from a delimiter that is described using a regex.
 *
 * The delimiter matching the regex is consumed from the underlying stream (if found), and
 * the underlying stream is left positioned at the byte after the regex match
 * string.
 *
 * IMPORTANT: The delimiter regex cannot contain any Capturing Groups!
 * Use (?: ... ) which is non-capturing, instead of regular ( ... ).
 * For example: this regex matches CRLF not followed by tab or space:
 * {{{
 *    """\r\n(?!(?:\t|\ ))"""
 * }}}
 * Notice use of the ?: to avoid a capture group around the alternatives of tab or space.
 *
 * Thread safety: This is inherently stateful - so not thread safe to use
 * this object from more than one thread.
 */
class RegexLimitingStream(inputStream: InputStream,
  regexForDelimiter: String,
  maximumLengthDelimiterExample: String,
  charset: Charset,
  targetChunkSize: Int = 32 * 1024)
  extends InputStream {

  Assert.usage(targetChunkSize >= 1)
  Assert.usage(maximumLengthDelimiterExample.length >= 1)

  private val in = inputStream

  /**
   * This only works because we lower the whole matching process to using iso-8859-1
   * which is equivalent to raw bytes. That way when we read a char to get a match
   * we know it consumes exactly one byte.
   *
   * This trick may be useful for dealing with DFDL's rawBytes feature.
   * In principle, a delimiter in DFDL can be a mixture of characters and
   * raw bytes. To match these, you have to lower the character parts to iso_8859_1
   * in the way done here, and then combine with the raw bytes to make a string
   * (of bytes) that can be matched pretending the data is iso_8859_1 data, when
   * it really isn't.
   *
   * Example of this might be if a UTF_8 string was delimited by bytes that
   * are illegal in UTF-8's encoding scheme. E.g., UTF-data delimited by say
   * bytes 00 and FF. So this could be %#x00;%#rFF; The 00, or NUL is a legal
   * UTF-8 code point. The FF is not.
   */
  private val maxDelimiterIn8859 = new String(maximumLengthDelimiterExample.getBytes(charset), StandardCharsets.ISO_8859_1)

  private val maxDelimiterLength =
    math.ceil(maxDelimiterIn8859.length *
      charset.newEncoder().maxBytesPerChar()).toInt

  private val chunkSize = math.max(targetChunkSize, maxDelimiterLength + 1)

  /**
   * This regex matches a chunk from zero to chunksize followed by boundaryMark, or
   * anything from zero to chunksize.
   * Group 1 is the chunk matched with group 2 containing the boundaryMark.
   * Group 3 is the chunk matched if boundaryMark is not found.
   */
  private val regex = """([\s\S]{0,""" + chunkSize + """}?)(?=(""" + regexForDelimiter + """))|([\s\S]{0,""" + chunkSize + """})"""
  private val pattern = Pattern.compile(regex)

  /**
   * The regex can match at most the chunkSize + the maxBoundaryMarkLength in size.
   */
  private val lookAheadMax = chunkSize + maxDelimiterLength

  private lazy val charsIter = {
    val cks = chunks
    val streamChars = cks.flatten
    val iter = new StreamIterator(streamChars)
    iter
  }

  override def read(): Int = {
    if (!charsIter.hasNext) -1
    else charsIter.next().toInt
  }

  override def available(): Int =
    if (charsIter.hasNext) 1 else 0

  override def close(): Unit = {
    //do nothing
  }

  private var noMoreChunks = false
  /**
   * This lazy stream stuff might look like a lot of overhead, but
   * consider that the overhead is once per chunk, so honestly the
   * regex match is of more concern.
   */
  private def chunks: Stream[String] = {
    if (noMoreChunks) Stream()
    else {
      in.mark(lookAheadMax)
      //
      // Unfortunately, we have to reconstruct these objects because they otherwise
      // might pre-cache underlying data from the input and we need them to
      // start from a precise byte location when the next scan begins.
      //
      val rdr = new InputStreamReader(in, StandardCharsets.ISO_8859_1)
      val scanner = new Scanner(rdr)
      val matchString = scanner.findWithinHorizon(pattern, 0)
      val matchLength = checkScan(matchString, scanner)
      //
      // the trick is that the length of the matchString could be shorter than the
      // number of characters (aka bytes) pulled from the input stream because in
      // scanning, the scanner or reader might buffer up extra decoded characters that
      // strictly speaking aren't needed to obtain the match.
      //
      // So we reset back to the start, and advance exactly that number of bytes.
      //
      // This only works because we have lowered everything to iso-8859-1 here.
      // Decoding errors, and the complexities they create over how big the string
      // is, vs. how many bytes were consumed... those can't happen with iso-8859-1.
      //
      in.reset() // might have to backup farther than the matchString length
      in.skip(matchString.length + matchLength) // advance exactly the right number of bytes
      if (matchLength > 0)
        noMoreChunks = true
      if (matchString.isEmpty())
        Stream()
      else
        matchString #:: chunks
    }
  }

  /**
   * Thorough checking that we understand the behavior of our regex and
   * scanner.
   */
  private def checkScan(matchString: String, scanner: Scanner) = {

    Assert.invariant(matchString ne null); // worst case it matches 0 length and we get ""
    //
    // Just do some error checking to be absolutely sure we understand
    // how the scanner works
    //
    val matcher = scanner.`match`()
    val beforeDelimMatch = matcher.end(1) // maybe avoids allocating the string
    val delimMatch = matcher.end(2)
    val noDelimMatch = matcher.end(3)
    val isFound = (beforeDelimMatch > -1)
    val delimMatchLength =
      if (isFound) {
        Assert.invariant(delimMatch > -1)
        Assert.invariant(noDelimMatch == -1)
        matcher.end(2) - matcher.start(2)
      } else {
        Assert.invariant(delimMatch == -1)
        Assert.invariant(noDelimMatch > -1)
        0
      }
    delimMatchLength
  }

}

