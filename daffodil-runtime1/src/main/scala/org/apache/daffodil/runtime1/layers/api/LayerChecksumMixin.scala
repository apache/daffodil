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
package org.apache.daffodil.runtime1.layers.api

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.util.Optional

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.layers.FixedLengthLimiter
import org.apache.daffodil.runtime1.layers.LayerNotEnoughDataException

import org.apache.commons.io.IOUtils

/**
 * Suitable only for checksums computed over small sections of data, not large data streams or whole files.
 *
 * The entire region of data the checksum is being computed over, will be pulled into a byte buffer in memory.
 */
trait LayerChecksumMixin { self: Layer =>

  /**
   * Define this if the layer has a small fixed length built into it
   * so it supports only LayerLengthKind 'implicit'
   * @return
   */
  protected def optLayerBuiltInConstantLength: Optional[Int] = Optional.empty()

  override def layerLimiter(layerPropertyInfo: LayerPropertyInfo): Optional[LayerLimiter] = {
    if (optLayerBuiltInConstantLength.isPresent) {
      Optional.of(new FixedLengthLimiter(optLayerBuiltInConstantLength.get))
    } else {
      Optional.empty() // means use the normal limiters
    }
  }

  /**
   * Override to compute the checksum of a buffer of data. The value computed is assigned to the first
   * DFDL variable defined by the layer in the LayerInfo object.
   *
   * @param layerRuntime layer context for the computation
   * @param isUnparse true if the direction is unparsing. Used because in some cases the computed checksum must
   *                  be written into the byte buffer in a specific location.
   * @param byteBuffer the bytes over which the checksum is to be computed. This can be modified, (for example so as
   *                   to embed the computed checksum in the middle of the data somewhere) and the resulting
   *                   bytes become the data that is written when unparsing.
   *                   TODO: what is the behavior if these bytes are written when parsing? Are those side-effects
   *                    seen or not?
   * @return the checksum value as an Int (32-bit signed integer)
   */
  def compute(
    layerRuntime: LayerRuntime,
    isUnparse: Boolean,
    byteBuffer: ByteBuffer,
  ): Int

  override def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream = {
    new ChecksumDecoderInputStream(self, jis, lr, this.optLayerBuiltInConstantLength)
  }

  override def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream = {
    new ChecksumEncoderOutputStream(self, jos, lr, this.optLayerBuiltInConstantLength)
  }
}

class ChecksumDecoderInputStream(
  layer: LayerChecksumMixin,
  jis: InputStream,
  lr: LayerRuntime,
  optLayerBuiltInConstantLength: Optional[Int],
) extends InputStream {

  private def doubleCheckLength(actualDataLen: Int): Unit = {
    if (optLayerBuiltInConstantLength.isPresent) {
      val neededLen = optLayerBuiltInConstantLength.get()
      if (neededLen > actualDataLen)
        lr.processingError(new LayerNotEnoughDataException(neededLen, actualDataLen))
    } else {
      lr.layerLengthKind() match {
        case JLayerLengthKind.Implicit => // ok. Could mean "as long as it can be"
        case JLayerLengthKind.Explicit => {
          //
          // These don't have to be equal. The actual length of data (bufLen) could
          // be shorter than the value of the explicit layerLength expression/constant.
          // The test test_checkDigit01e is a negative test that has this situation.
          Assert.invariant(lr.layerLength() >= actualDataLen)
        }
        case JLayerLengthKind.BoundaryMark =>
          Assert.invariantFailed("Can't be layerLengthKind 'boundaryMark'.")
      }
    }
  }

  private lazy val bais = {
    val ba = IOUtils.toByteArray(jis)
    doubleCheckLength(ba.length)
    val buf = ByteBuffer.wrap(ba)
    val checksum = layer.compute(lr, isUnparse = false, buf)
    // assign checksum to the first variable.
    lr.setInt(lr.variable(0), checksum)
    new ByteArrayInputStream(ba)
  }

  override def read(): Int = bais.read()
}

class ChecksumEncoderOutputStream(
  layer: LayerChecksumMixin,
  jos: OutputStream,
  lr: LayerRuntime,
  optLayerBuiltInConstantLength: Optional[Int],
) extends OutputStream {

  private lazy val bufLen: Long =
    if (optLayerBuiltInConstantLength.isPresent)
      optLayerBuiltInConstantLength.get()
    else
      lr.layerLengthKind() match {
        case JLayerLengthKind.Explicit => lr.layerLength()
        case JLayerLengthKind.Implicit => -1
        case JLayerLengthKind.BoundaryMark =>
          Assert.invariantFailed("Can't be layerLengthKind 'boundaryMark'.")
      }

  private lazy val baos = new ByteArrayOutputStream

  private var count: Long = 0

  override def write(b: Int): Unit = {
    baos.write(b)
    count += 1
    if (bufLen != -1)
      if (count == bufLen) {
        // we can auto-close it in this case
        close()
      } else if (count > bufLen) {
        //
        // This could happen if the layer logically unparses as one of two choice branches where they
        // are supposed to be all the same length, but one is in fact longer than expected by the bufLen.
        lr.processingError(
          new IndexOutOfBoundsException(
            "Written data amount exceeded fixed layer length of $bufLen.",
          ),
        )
      } else {
        // Assert.invariant(count < bufLen)
        // ok. We're still accumulating data
      }
  }

  private var isOpen: Boolean = true

  override def close(): Unit = {
    if (isOpen) { // allow multiple closes
      isOpen = false
      val ba = baos.toByteArray
      val baLen = ba.length
      if (bufLen != -1)
        Assert.invariant(baLen <= bufLen)
      val buf = ByteBuffer.wrap(ba)
      val checksum = layer.compute(lr, isUnparse = true, buf)
      jos.write(ba)
      // assign checksum to the first variable.
      lr.setInt(lr.variable(0), checksum)
      jos.close() // required so that closes propagate, and the buffering output streams recombine/collapse again.
    }
  }
}
