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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.LocalStack
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._

abstract class LocalBuffer[T <: java.nio.Buffer] {
  protected def allocate(length: Long): T

  private var tempBuf: Maybe[T] = Nope

  def getBuf(length: Long) = {
    Assert.usage(length <= Int.MaxValue)
    if (tempBuf.isEmpty || tempBuf.get.capacity < length) {
      tempBuf = Maybe(allocate(length.toInt))
    }
    val buf = tempBuf.get
    buf.clear
    buf.limit(length.toInt)
    buf
  }
}

/**
 * Warning: Only mix this into thread-local state objects. If mixed into a regular
 * class this will end up sharing the local stack object across threads, which
 * is a very bad idea (not thread safe).
 */
// move to common state shared by everything
trait LocalBufferMixin {

  /**
   * Use with OnStack idiom for temporary char buffers
   */
  final class LocalCharBuffer extends LocalBuffer[CharBuffer] {
    protected def allocate(length: Long) = CharBuffer.allocate(length.toInt)
  }

  final class LocalByteBuffer extends LocalBuffer[ByteBuffer] {
    protected def allocate(length: Long) = ByteBuffer.allocate(length.toInt)
  }

  final lazy val withLocalCharBuffer = new LocalStack[LocalCharBuffer](new LocalCharBuffer)

  final lazy val withLocalByteBuffer = new LocalStack[LocalByteBuffer](new LocalByteBuffer)
}
