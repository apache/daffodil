package edu.illinois.ncsa.daffodil.io

import passera.unsigned.ULong
import java.nio.charset.CharsetEncoder
import java.nio.ByteBuffer
import java.nio.CharBuffer

trait DataOutputStream extends DataStreamCommon {

  def encoder: CharsetEncoder
  def setEncoder(encoder: CharsetEncoder): Unit

  /**
   * Sets the fill byte. Only the least significant 8 bits of the argument are used.
   */
  def setFillByte(fillByte: Int): Unit

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putLong(signedLong: Long, bitLengthFrom1To64: Int): Boolean

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int): Boolean

  /**
   * If bitLengthFrom1 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1 least significant bits of the bigInt using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1 is not greater than or equal to 1.
   *
   */
  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean

  /**
   * Returns number of bytes transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  def putByteBuffer(bb: ByteBuffer): Long
  def putBytes(ba: Array[Byte]): Long

  /**
   * Returns number of characters transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  def putString(str: String): Long
  def putCharBuffer(cb: CharBuffer): Long

  /**
   * Force buffered content to output if possible.
   */
  def flush(): Unit

  /**
   * close-out this output stream. No more writing to this after.
   */
  def setFinished(): Unit
  def isFinished: Boolean
}
