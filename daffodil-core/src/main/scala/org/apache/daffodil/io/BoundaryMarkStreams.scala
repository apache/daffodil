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

import java.io.FilterOutputStream
import java.io.InputStream
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import org.apache.daffodil.lib.exceptions.Assert

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
class BoundaryMarkLimitingInputStream(
  inputStream: InputStream,
  boundaryMark: String,
  charset: Charset,
  targetChunkSize: Int = 32 * 1024
) extends InputStream {

  Assert.usage(targetChunkSize >= 1)
  Assert.usage(boundaryMark.nonEmpty)

  private lazy val boundaryMarkIn8859 =
    new String(boundaryMark.getBytes(charset), StandardCharsets.ISO_8859_1)

  private lazy val quotedBoundaryMark =
    Pattern.quote(boundaryMarkIn8859) // in case pattern has non-regex-safe characters in it

  private lazy val delegateStream = new RegexLimitingInputStream(
    inputStream,
    quotedBoundaryMark,
    boundaryMarkIn8859,
    charset,
    targetChunkSize
  )

  override def read(): Int = delegateStream.read()

}

/**
 * A FilterOutputStream that inserts a boundary mark at the end of the layer's output stream.
 *
 * @param jos          The underlying Java OutputStream.
 * @param boundaryMark The boundary mark to be inserted at the end of the stream.
 * @param charset      The character set used to encode the boundary mark.
 */
class BoundaryMarkInsertingJavaOutputStream(
  jos: java.io.OutputStream,
  boundaryMark: String,
  charset: Charset
) extends FilterOutputStream(jos) {

  private var closed = false

  private lazy val boundaryMarkBytes = boundaryMark.getBytes(charset)

  override def close(): Unit = {
    if (!closed) {
      jos.write(boundaryMarkBytes)
      jos.flush()
      jos.close()
      closed = true
    }
  }

}
