package edu.illinois.ncsa.daffodil.io

import java.nio.ByteBuffer

object Utils {
  def concatByteBuffers(bb1: ByteBuffer, bb2: ByteBuffer): ByteBuffer = {
    val bb = ByteBuffer.allocate(bb1.remaining + bb2.remaining)
    bb.put(bb1)
    bb.put(bb2)
    bb.flip
    bb
  }
}