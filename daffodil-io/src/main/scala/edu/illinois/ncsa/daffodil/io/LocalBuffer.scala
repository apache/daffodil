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
