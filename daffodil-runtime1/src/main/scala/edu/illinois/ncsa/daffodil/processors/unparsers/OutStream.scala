package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import java.io.File
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import java.nio.charset.CharsetEncoder
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.exceptions.Assert

trait OutStream {
  def bitPos0b: Long
  def bitPos1b = bitPos0b + 1
  def bitLimit0b: Long
  def wbc: DFDL.Output
  def encode(charset: Charset, str: String): Unit
}

class GeneralOutStream(
  out: DFDL.Output,
  bitStartPos0b: Long,
  numBitsLimit: Long, // a count, not a position
  bitOrd: BitOrder) extends OutStream {

  val outStr = java.nio.channels.Channels.newOutputStream(out)

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
    outStr.write(bytes)
    setBitPos0b(bitPos0b + (bytes.length * 8))
  }
}
