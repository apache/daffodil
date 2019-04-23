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
import org.apache.daffodil.schema.annotation.props.gen.BitOrder.MostSignificantBitFirst

class midBitsToEndTransformer(
  layerLengthInBytesEv: LayerLengthInBytesEv,
  layerTransformArgsEv: LayerTransformArgsEv,
  trd:                  TermRuntimeData)
  extends LayerTransformer {
  /*
   * For unparsing, it is more or less unavoidable that we cannot stream, as we need to reach
   * the end of the input before we can provide the mid-bits that were moved.
   * In practice, it is not expected that this transform will be used on "large" blocks,
   * so it is not worth the effort to figure out how to implement some form of streaming.
   */

  var length: Int = -1
  
  var firstBitIndex0b: Int = -1
  var numBits: Int = -1

  val isMSBF = trd.defaultBitOrder == MostSignificantBitFirst

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

  def firstBitByteIndex0b = (firstBitIndex0b / 8)
  def lastBitByteIndex0b = ((firstBitIndex0b + numBits) / 8)
  def firstBitBitIndex0b = if (isMSBF) {
    firstBitIndex0b % 8
  } else {
    7 - (firstBitIndex0b % 8)
  }
  def lastBitBitIndex0b = if (isMSBF) {
    (firstBitIndex0b + numBits) % 8
  } else {
    7 - ((firstBitIndex0b + numBits) % 8)
  }

  private def buffToHex(buff: Array[Byte]): String = {
    buff.map(b => String.format("%02x", Byte.box(b))).mkString(" ")
  }

  private def initialize(state: ParseOrUnparseState) {
    length = layerLengthInBytesEv.evaluate(state).toInt
    val argsString = layerTransformArgsEv.evaluate(state)
      .trim()
      .split("\\ +")
    if (argsString.length != 2) {
      state.SDE(
        "midBitsToEnd layer transform requires exactly 2 arguments, recieved %s: %s}",
        argsString.length, argsString.mkString(","))
    }
    try {
      //DFDL is 1-indexed, we are working 0-indexed
      firstBitIndex0b = argsString(0).toInt - 1
      numBits = argsString(1).toInt
    } catch {
      case _: NumberFormatException => state.SDE(
        "midBitsToEnd layer transform requires Integer arguements, recieved: %s",
        argsString.mkString(","))
    }

  }

  /*
   * Scala does not seem to support bitwise operations on bytes; and instead silently upcasts to Int.
   * Spamming toByte is insufficient since negative numbers will get padded with 1 instead of 0
   * For this reason, we do most of our internal logic in Int, and mask out the upper 24 bits
   */

  val bitMask: Int = 0x000000ff

  /*
   * For the purposes of comments, assume we have have firstBitIndex0b=7, length=2.
   * That is to say, the bits marked X below are what is getting shifted if we are MSBF
   *
   * bbbb bbbX
   * Xbbb bbbb
   */

  private def doDecode(buff: Array[Byte]): Unit = {

    /*First, move the middle bits to the end.
     *
     * Since the layer is byte aligned, we know we are writing them a byte aligned location
     * We will end up moving a multiple of 8 bits. This is okay, since, after we shift,
     * any extra bytes will be left passed the length byte, and so will be ignored
     */
    var dstByteIndex0b = length
    var srcByteIndex0b = firstBitByteIndex0b
    while (dstByteIndex0b < buffSize) {
      val srcByte = buff(srcByteIndex0b) & bitMask
      val srcByteNext = buff(srcByteIndex0b + 1) & bitMask

      val byte = if (isMSBF) {
        /*
         * Given:
         * bbbb bbbX
         * Ybbb bbbb
         *
         * Compute:
         * XYbb bbbb
         */
        val byteUpper = (srcByte << firstBitBitIndex0b) & bitMask
        val byteLower = (srcByteNext >>> (8 - firstBitBitIndex0b))
        (byteLower | byteUpper).toByte
      } else {
        /*
         * Given:
         * Xbbb bbbb
         * bbbb bbbY
         *
         * Compute:
         * bbbb bbYX
         */
        val byteUpper = (srcByteNext << firstBitBitIndex0b) & bitMask
        val byteLower = (srcByte >>> (8 - firstBitBitIndex0b))
        (byteLower | byteUpper).toByte
      }
      buff(dstByteIndex0b) = byte
      dstByteIndex0b += 1
      srcByteIndex0b += 1
    }

    /*
     * When we do the shift, we will end up overwriting all the bits in byte containing firstBitIndex.
     * We want to preserve all bits from before firstBitIndex
     */
    val savedBits = {
      val srcByte = buff(firstBitByteIndex0b) & bitMask
      if (isMSBF) {
        ((srcByte >>> (8 - firstBitBitIndex0b)) << (8 - firstBitBitIndex0b)).toByte
      } else {
        ((srcByte << firstBitBitIndex0b) >> firstBitBitIndex0b).toByte
      }
    }

    /*
     * Next, perform the shift
     */
    dstByteIndex0b = firstBitByteIndex0b
    while (dstByteIndex0b < length) {
      val srcByte = buff(dstByteIndex0b + byteShift) & bitMask
      val srcByteNext = buff(dstByteIndex0b + byteShift + 1) & bitMask

      val byte = if (isMSBF) {
        val byteUpper = (srcByte << bitShift) & bitMask
        val byteLower = srcByteNext >>> (8 - bitShift)
        (byteLower | byteUpper).toByte
      } else {
        val byteUpper = (srcByteNext << (8 - bitShift)) & bitMask
        val byteLower = srcByte >>> bitShift
        (byteLower | byteUpper).toByte
      }
      buff(dstByteIndex0b) = byte
      dstByteIndex0b += 1
    }

    /*
     * Finally, restore the upper bits of the byte containing firstBitIndex
     * (or lower bits if LSBF)
     */
    {
      val srcByte = buff(firstBitByteIndex0b) & bitMask

      val byte = if (isMSBF) {
        val byteLower = ((srcByte << firstBitBitIndex0b) & bitMask) >>> firstBitBitIndex0b
        val byteUpper = savedBits
        (byteLower | byteUpper).toByte
      } else {
        val byteLower = savedBits
        val byteUpper = ((srcByte >>> (7 - firstBitBitIndex0b)) << (7 - firstBitBitIndex0b)) & bitMask
        (byteLower | byteUpper).toByte
      }
      buff(firstBitByteIndex0b) = byte
    }

  }

  private def doEncode(buff: Array[Byte]): Unit = {
   
    /*
     * When we shift, we move all the bits in byte containing firstBitIndex.
     */
    val savedBits1 = {
      val srcByte = buff(firstBitByteIndex0b) & bitMask
      if (isMSBF) {
        ((srcByte >>> (8 - firstBitBitIndex0b)) << (8 - firstBitBitIndex0b)).toByte
      } else {
        (((srcByte << (firstBitBitIndex0b+1)) & bitMask) >> (firstBitBitIndex0b+1)).toByte
      }
    }

    /*
     * First, do the shift
     */
    var dstByteIndex0b = buffSize - 1
    while (dstByteIndex0b >= firstBitByteIndex0b) {
      val srcByte = buff(dstByteIndex0b - byteShift) & bitMask
      val prevIndex0b = dstByteIndex0b - byteShift - 1
      val srcBytePrev = if (prevIndex0b < 0) {
        0
      } else {
        buff(dstByteIndex0b - byteShift - 1) & bitMask
      }

      val byte = if (isMSBF) {
        val byteLower = srcByte >>> bitShift
        val byteUpper = (srcBytePrev << (8 - bitShift)) & bitMask
        (byteLower | byteUpper).toByte
      } else {
        val byteLower = (srcBytePrev >>> (8 - bitShift))
        val byteUpper = (srcByte << bitShift) & bitMask
        (byteLower | byteUpper).toByte
      }
      buff(dstByteIndex0b) = byte
      dstByteIndex0b -= 1
    }

    /*
     * When we move the end bits to the middle, we override all other bits in the byte containing the last bit of interest.
     * (We also override all the other bits in the byte containing the first one, but we need to deal with that anyway thanks to the shift)
     */

    val savedBits2 = {
      val srcByte = buff(lastBitByteIndex0b) & bitMask
      if (isMSBF) {
        (((srcByte << lastBitBitIndex0b) & bitMask) >>> lastBitBitIndex0b).toByte
      } else {
        (((srcByte >>> (7 - lastBitBitIndex0b)) << (7-lastBitBitIndex0b)) & bitMask).toByte
      }
    }

    /*
     * Next, we move the end bits to the middle
     */
    var numBitsRemaining = numBits
    var srcByteIndex0b = length
    dstByteIndex0b = firstBitByteIndex0b
    while (dstByteIndex0b <= lastBitByteIndex0b) {
      val srcByte = buff(srcByteIndex0b) & bitMask
      val srcBytePrev = buff(srcByteIndex0b - 1) & bitMask

      val byte = if (isMSBF) {
        val byteUpper = srcBytePrev << (8 - firstBitBitIndex0b)
        val byteLower = srcByte >>> firstBitBitIndex0b
        (byteLower | byteUpper).toByte
      } else {
        /*
         * The off-by-one here does look awkward.
         * Note, however, that the constants still differ by 8, which is what we would expect.
         * One way of understanding the off-by-one compared to the previous case, is that the shift
         * used with srcByte should be in the range [0,7],
         * while the shift used with srcBytePrev should be in [1,8]
         * This reflects the fact that, when we are byte aligned, we expect
         * to get data from srcByte, and full mask out srcBytePrev
         */
        val byteUpper = srcByte << (7 - firstBitBitIndex0b)
        val byteLower = srcBytePrev >>> (1 + firstBitBitIndex0b)
        (byteLower | byteUpper).toByte
      }

      buff(dstByteIndex0b) = byte

      dstByteIndex0b += 1
      srcByteIndex0b += 1
    }

    /*
     * Restore the upper bits of the byte containing firstBitIndex
     * (or lower if MSBF)
     */
    {
      val srcByte = buff(firstBitByteIndex0b) & bitMask

      val byte = if (isMSBF) {
        val byteUpper = savedBits1
        val byteLower = ((srcByte << firstBitBitIndex0b) & bitMask) >>> firstBitBitIndex0b
        (byteLower | byteUpper).toByte
      } else {
        val byteUpper = ((srcByte >>> (7-firstBitBitIndex0b)) << (7-firstBitBitIndex0b)) & bitMask
        val byteLower = savedBits1
        (byteLower | byteUpper).toByte
      }
      buff(firstBitByteIndex0b) = byte
    }

    /*
     * Restore the lower bits of the byte containing firstBitIndex
     * (Or upper if MSBF)
     */
    {
      val srcByte = buff(lastBitByteIndex0b) & bitMask

      val byte = if (isMSBF) {
        val byteUpper = (srcByte >>> (8 - lastBitBitIndex0b)) << (8 - lastBitBitIndex0b)
        val byteLower = savedBits2
        (byteLower | byteUpper).toByte
      } else {
        val byteUpper = savedBits2
        val byteLower = ((srcByte << lastBitBitIndex0b) & bitMask) >>> lastBitBitIndex0b
        (byteLower | byteUpper).toByte
      }
      buff(lastBitByteIndex0b) = byte
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
      state.processor.asInstanceOf[Parser].PENotEnoughBits(state, length * 8, MaybeULong((length - bytesRemaining) * 8))
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

    val xformer = new midBitsToEndTransformer(maybeLayerLengthInBytesEv.get, maybeLayerTransformArgsEv.get, trd)
    xformer
  }
}
