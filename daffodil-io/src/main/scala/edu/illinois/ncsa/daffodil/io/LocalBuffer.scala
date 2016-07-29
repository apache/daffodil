/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.nio.CharBuffer
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.util.LocalStack

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

  final val withLocalCharBuffer = new LocalStack[LocalCharBuffer](new LocalCharBuffer)

  final val withLocalByteBuffer = new LocalStack[LocalByteBuffer](new LocalByteBuffer)
}
