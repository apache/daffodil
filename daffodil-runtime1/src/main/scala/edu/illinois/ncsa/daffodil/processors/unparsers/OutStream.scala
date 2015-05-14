package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import java.io.File
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import java.nio.charset.CharsetEncoder
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.ByteBuffer
import scala.collection.mutable

trait OutStream {
  def copyOutStream(): OutStream
  def bitPos0b: Long
  def bitPos1b = bitPos0b + 1
  def bitLimit0b: Long
  def wbc: DFDL.Output
  def encode(charset: Charset, str: String): Unit
  def writeByte(b: Int): Unit
  def writeBytes(bytes: Array[Byte]): Unit

  def setDebugging(b: Boolean): Unit
  def pastData(nBytesRequested: Int): ByteBuffer
}

class GeneralOutStream(
  out: DFDL.Output,
  bitStartPos0b: Long,
  numBitsLimit: Long, // a count, not a position
  bitOrd: BitOrder,
  var areDebugging: Boolean = false) extends OutStream {

  val outStr = java.nio.channels.Channels.newOutputStream(out)
  def copyOutStream() = new GeneralOutStream(out, bitStartPos0b, numBitsLimit, bitOrd, areDebugging)

  private var bitPos0b_ = bitStartPos0b
  private var bitOrder_ = bitOrd

  def wbc = out
  def bitLimit0b = numBitsLimit

  def bitPos0b = bitPos0b_
  def setBitPos0b(newBitPos0b: Long) {
    bitPos0b_ = newBitPos0b
  }

  def bitOrder = bitOrder_
  def setBitOrder(newBitOrder: BitOrder) {
    bitOrder_ = newBitOrder
  }

  // TODO: make this reuse a charBuffer and a byte buffer.
  // We should be able to go from the string directly to the output channel
  // without having to allocate anything. 
  // 
  // The buffers should self-grow adaptively within reason.
  // if the string is huge, then we should wrap it to get a char buffer.
  //
  def encode(charset: Charset, str: String): Unit = {
    Assert.usage(bitPos0b % 8 == 0)
    val bytes = str.getBytes(charset)
    writeBytes(bytes)
  }

  def writeByte(b: Int): Unit = {
    if (areDebugging) writeDebugByte(b)
    outStr.write(b)
    setBitPos0b(bitPos0b + 8)
  }

  def writeBytes(bytes: Array[Byte]): Unit = {
    if (areDebugging) writeDebugBytes(bytes)
    outStr.write(bytes)
    setBitPos0b(bitPos0b + (bytes.length * 8))
  }

  /*
   * Debug support.
   */

  def setDebugging(b: Boolean) { areDebugging = b }

  private val pastDataQueue = new mutable.Queue[Int] // must be saved/restored when state is assigned/copied
  private val maxDataQueueSize = 8192

  def pastData(nBytesRequested: Int): ByteBuffer = {
    if (!areDebugging) return ByteBuffer.allocate(0)
    val pastDataNotEOF = pastDataQueue.filterNot { i => i == -1 }.map { _.toByte }
    val nBytesOfQueue = pastDataNotEOF.takeRight(nBytesRequested)
    val bb = ByteBuffer.wrap(nBytesOfQueue.toArray)
    bb
  }

  private def writeDebugByte(b: Int) {
    if (pastDataQueue.size >= maxDataQueueSize) {
      pastDataQueue.dequeue()
    }
    pastDataQueue.enqueue(b)
  }

  private def writeDebugBytes(ba: Array[Byte]) {
    ba.foreach { writeDebugByte(_) }
  }

}
