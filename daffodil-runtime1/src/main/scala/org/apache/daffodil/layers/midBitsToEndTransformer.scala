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

import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.processors.LayerLengthInBytesEv
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerTransformArgsEv
import java.io.InputStream
import java.io.OutputStream
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.parsers.Parser

import org.apache.daffodil.processors.ExplicitLengthEv
import org.apache.daffodil.io.ExplicitLengthLimitingStream
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.processors.Failure

class midBitsToEndTransformer(
  layerLengthInBytesEv: LayerLengthInBytesEv,
  layerTransformArgsEv: LayerTransformArgsEv)
  extends LayerTransformer {
  /*
   * For unparsing, it is more or less unavoidable that we cannot stream, as we need to reach
   * the end of the input before we can provide the mid-bits that were moved.
   * In practice, it is not expected that this transform will be used on "large" blocks,
   * so it is not worth the effort to figure out how to implement some form of streaming.
   */

  var length: Int = -1
  var firstBitIndex: Int = -1
  var numBits: Int = -1

  /*
   * Note the extra padding bytes. One of these bytes is just to accomodate rounding from integer division.
   * The other one is because, to simplify the code, a portion of the encoder may attempt to read up to 7 extra bits,
   * then discard them. (This would happen when the last bit that gets moved is the first bit of a byte in the encoded form;
   * the encoder will attempt to read enough bits to be able to fully replace the entire byte, then mask out the lower 7 bits)
   */
  def tmpByteCount = numBits / 8 + 2
  def buffSize = length + tmpByteCount

  def byteShift = numBits / 8
  def bitShift = numBits % 8

  def firstBitByteIndex = (firstBitIndex / 8)
  def lastBitByteIndex = ((firstBitIndex + numBits) / 8)
  def firstBitBitIndex = firstBitIndex % 8
  def lastBitBitIndex = (firstBitIndex + numBits) % 8

  private def buffToHex(buff: Array[Byte]): String = {
    buff.map(b => String.format("%02x", Byte.box(b))).mkString(" ")
  }

  private def initialize(state: ParseOrUnparseState) {
    length = layerLengthInBytesEv.evaluate(state).toInt
    val argsString = layerTransformArgsEv.evaluate(state)
      .trim()
      .split("\\ +")
    if (argsString.length != 2) {
      state.SDE(s"""midBitsToEnd layer transform requires exactly 2 arguements, recieved ${argsString.length}: "${argsString.mkString(",")}" """)
    }
    try {
      firstBitIndex = argsString(0).toInt
      numBits = argsString(1).toInt
    } catch {
      case _: NumberFormatException => state.SDE(s"""midBitsToEnd layer transform requires Integer arguements, recieved: "${argsString.mkString(",")}"""")
    }

  }

  /*
   * Scala does not seem to support bitwise operations on bytes; and instead silently upcasts to Int.
   * Spamming toByte is insufficient since negative numbers will get padded with 1 instead of 0
   * For this reason, we do most of our internal logic in Int, and mask out the upper 24 bits
   */

  val bitMask: Int = 0x000000ff

  private def doDecode(buff: Array[Byte]): Unit = {

    /*First, move the middle bits to the end.
     *
     * Since the layer is byte aligned, we know we are writing them a byte aligned location
     * We will end up moving a multiple of 8 bits. This is okay, since, after we shift,
     * any extra bytes will be left passed the length byte, and so will be ignored
     */
    var dstByteIndex = length
    var srcByteIndex = firstBitByteIndex
    while (dstByteIndex < buffSize) {
      val srcByte = buff(srcByteIndex) & bitMask
      val srcByteNext = buff(srcByteIndex + 1) & bitMask

      val byteUpper = (srcByte << firstBitBitIndex) & bitMask
      val byteLower = (srcByteNext >>> (8 - firstBitBitIndex))
      val byte = (byteLower | byteUpper).toByte
      buff(dstByteIndex) = byte
      dstByteIndex += 1
      srcByteIndex += 1
    }

    /*
     * When we do the shift, we will end up overiting all the bits in byte containing firstBitIndex.
     * We want to preserve all bits from before firstBitIndex
     */
    val savedBits = {
      val srcByte = buff(firstBitByteIndex) & bitMask
      ((srcByte >>> (8 - firstBitBitIndex)) << (8 - firstBitBitIndex)).toByte
    }

    /*
     * Next, perform the shift
     */
    dstByteIndex = firstBitByteIndex
    while (dstByteIndex < length) {
      val srcByte = buff(dstByteIndex + byteShift) & bitMask
      val srcByteNext = buff(dstByteIndex + byteShift + 1) & bitMask

      val byteUpper = srcByte << bitShift & bitMask
      val byteLower = srcByteNext >>> (8 - bitShift)
      val byte = (byteLower | byteUpper).toByte
      buff(dstByteIndex) = byte
      dstByteIndex += 1
    }

    /*
     * Finally, restore the upper bits of the byte containing firstBitIndex
     */
    {
      val srcByte = buff(firstBitByteIndex) & bitMask
      val byteLower = ((srcByte << firstBitBitIndex) & bitMask) >>> firstBitBitIndex
      val byteUpper = savedBits
      val byte = (byteLower | byteUpper).toByte
      buff(firstBitByteIndex) = byte
    }

  }

  private def doEncode(buff: Array[Byte]): Unit = {

    /*
     * When we shift, we move all the bits in byte containing firstBitIndex.
     */
    val savedBits1 = {
      val srcByte = buff(firstBitByteIndex) & bitMask
      ((srcByte >>> (8 - firstBitBitIndex)) << (8 - firstBitBitIndex)).toByte
    }

    /*
     * First, do the shift
     */

    var dstByteIndex = buffSize - 1
    while (dstByteIndex >= firstBitByteIndex) {
      val srcByte = buff(dstByteIndex - byteShift) & bitMask
      val prevIndex = dstByteIndex - byteShift - 1
      val srcBytePrev = if (prevIndex < 0) {
        0
      } else {
        buff(dstByteIndex - byteShift - 1) & bitMask
      }
      val byteLower = srcByte >>> bitShift
      val byteUpper = (srcBytePrev << (8 - bitShift)) & bitMask
      val byte = (byteLower | byteUpper).toByte
      buff(dstByteIndex) = byte
      dstByteIndex -= 1
    }

    /*
     * When we move the end bits to the middle, we override all other bits in the byte containing the last bit of interest.
     * (We also override all the other bits in the byte containing the first one, but we need to deal with that anyway thanks to the shift)
     */

    val savedBits2 = {
      val srcByte = buff(lastBitByteIndex) & bitMask
      (((srcByte << lastBitBitIndex) & bitMask) >>> lastBitBitIndex).toByte
    }

    /*
     * Next, we move the end bits to the middle
     */
    var numBitsRemaining = numBits
    var srcByteIndex = length
    dstByteIndex = firstBitByteIndex
    while (dstByteIndex <= lastBitByteIndex) {
      val srcByte = buff(srcByteIndex) & bitMask
      val srcBytePrev = buff(srcByteIndex - 1) & bitMask

      val byteUpper = srcBytePrev << (8 - firstBitBitIndex)
      val byteLower = srcByte >>> firstBitBitIndex
      val byte = (byteLower | byteUpper).toByte

      buff(dstByteIndex) = byte

      dstByteIndex += 1
      srcByteIndex += 1
    }

    /*
     * Restore the upper bits of the byte containing firstBitIndex
     */
    {
      val srcByte = buff(firstBitByteIndex) & bitMask
      val byteLower = ((srcByte << firstBitBitIndex) & bitMask) >>> firstBitBitIndex
      val byteUpper = savedBits1
      val byte = (byteLower | byteUpper).toByte
      buff(firstBitByteIndex) = byte
    }

    /*
     * Restore the lower bits of the byte containing firstBitIndex
     */
    {
      val srcByte = buff(lastBitByteIndex) & bitMask
      val byteLower = savedBits2
      val byteUpper = (srcByte >> (8 - lastBitBitIndex)) << (8 - lastBitBitIndex)
      val byte = (byteLower | byteUpper).toByte
      buff(lastBitByteIndex) = byte
    }
  }

  protected def wrapLayerDecoder(jis: InputStream, state: PState): InputStream = {
    val buff: Array[Byte] = new Array(buffSize)
    var bytesRemaining = length
    var offset = 0
    var break = false
    while (bytesRemaining > 0 && !break) {
      val bytesRead = jis.read(buff, offset, bytesRemaining)
      if (bytesRead <= 0) {
        break = true
      } else {
        offset += bytesRead
        bytesRemaining -= bytesRead
      }
    }
    if (bytesRemaining > 0) {
      state.processor.asInstanceOf[Parser].PENotEnoughBits(state, length*8, MaybeULong((length - bytesRemaining)*8))
    } else {
      doDecode(buff)
    }
    /*
     * Even in the error path, we  still return buff and just rely on the fact that we marked this as a failure
     */
    new ByteArrayInputStream(buff)
  }
  protected def wrapLayerEncoder(jos: OutputStream, state: UState): OutputStream = {

    new ByteArrayOutputStream() {

      var written = 0

      private def maybeFinish(): Unit = {
        if (written == length) {
          while (written < buffSize) {
            /*
             * Add the expected padding bits so the encoder has some scratch space
             * We use super.write, because we have exceeded the amount that "this" is allowed to hold
             */
            super.write(0)
            written += 1
          }
          val buff = toByteArray()
          doEncode(buff)
          jos.write(buff, 0, length)
        }
      }

      override def write(b: Int): Unit = {
        //Only writes the lower 8 bits
        written += 1
        Assert.invariant(written <= length)
        super.write(b)
        maybeFinish()
      }
      override def write(b: Array[Byte]): Unit = {
        written += b.length
        Assert.invariant(written <= length)
        super.write(b)
        maybeFinish()

      }
      override def write(b: Array[Byte], off: Int, len: Int): Unit = {
        written += len
        Assert.invariant(written <= length)
        super.write(b, off, len)
        maybeFinish()
      }
    }
  }
  protected def wrapLimitingStream(jos: OutputStream, state: UState): OutputStream = {
    initialize(state)
    jos
  }
  protected def wrapLimitingStream(jis: InputStream, state: PState): InputStream = {
    initialize(state)
    new ExplicitLengthLimitingStream(jis, length)
  }

}

object midBitsToEndTransformerFactory
  extends LayerTransformerFactory("midBitsToEnd") {

  override def newInstance(
    maybeLayerCharsetEv:       Maybe[LayerCharsetEv],
    maybeLayerLengthKind:      Maybe[LayerLengthKind],
    maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
    maybeLayerLengthUnits:     Maybe[LayerLengthUnits],
    maybeLayerBoundaryMarkEv:  Maybe[LayerBoundaryMarkEv],
    maybeLayerTransformArgsEv: Maybe[LayerTransformArgsEv],
    trd:                       TermRuntimeData): LayerTransformer = {

    val xformer = new midBitsToEndTransformer(maybeLayerLengthInBytesEv.get, maybeLayerTransformArgsEv.get)
    xformer
  }
}