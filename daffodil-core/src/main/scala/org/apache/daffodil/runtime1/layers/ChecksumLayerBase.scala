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
package org.apache.daffodil.runtime1.layers

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.layers.api.Layer

import org.apache.commons.io.IOUtils

/**
 * A base class for ChecksumLayer - hides details from the API user who is deriving a
 * concrete class from ChecksumLayer.
 *
 * Suitable only for checksums computed over small sections of data, not large data streams or whole files.
 */
abstract class ChecksumLayerBase(
  localName: String,
  targetNamespace: String
) extends Layer(localName, targetNamespace) {

  private var checksum: Int = -1

  private var length: Int = -1
  private var isInitialized: Boolean = false

  final def getLength: Int = length

  /**
   * Must be called to specify the length, or it is a fatal API usage error.
   * @param len The fixed length over which the checksum is computed.
   */
  final protected def setLength(len: Int): Unit = {
    this.length = len
    if (len < 0) processingError(s"The layer length is negative: $len.")
    isInitialized = true
  }

  final protected def getChecksum: Int = checksum
  final private[layers] def setChecksum(checksum: Int): Unit = { this.checksum = checksum }

  def compute(
    isUnparse: Boolean,
    byteBuffer: ByteBuffer
  ): Int

  private def checkInitialized() = {
    if (!isInitialized) {
      // this is a RuntimeException, so will be fatal whether parsing or unparsing.
      throw new IllegalStateException(
        "ChecksumLayer API usage: setLength method was never called."
      )
    }
  }

  final override def wrapLayerInput(jis: InputStream): InputStream = {
    checkInitialized()
    new ChecksumDecoderInputStream(this, jis)
  }

  final override def wrapLayerOutput(jos: OutputStream): OutputStream = {
    checkInitialized()
    new ChecksumEncoderOutputStream(this, jos)
  }
}

class ChecksumDecoderInputStream(
  layer: ChecksumLayerBase,
  jis: InputStream
) extends InputStream {

  private def doubleCheckLength(actualDataLen: Int): Unit = {
    val neededLen = layer.getLength
    if (neededLen > actualDataLen)
      layer.processingError(new LayerNotEnoughDataException(neededLen, actualDataLen))
  }

  private lazy val bais = {
    val ba = new Array[Byte](layer.getLength)
    val nRead = IOUtils.read(jis, ba)
    doubleCheckLength(nRead)
    val buf = ByteBuffer.wrap(ba)
    layer.setChecksum(layer.compute(isUnparse = false, buf))
    new ByteArrayInputStream(ba)
  }

  override def read(): Int = bais.read()
}

class ChecksumEncoderOutputStream(
  layer: ChecksumLayerBase,
  jos: OutputStream
) extends OutputStream {

  private lazy val baos = new ByteArrayOutputStream(layer.getLength)

  private var count: Long = 0

  override def write(b: Int): Unit = {
    baos.write(b)
    count += 1
    if (count > layer.getLength) {
      //
      // This could happen if the layer logically unparses as one of two choice branches where they
      // are supposed to be all the same length, but one is in fact longer than expected by the bufLen.
      layer.processingError(
        new IndexOutOfBoundsException(
          s"Written data amount exceeded fixed layer length of ${layer.getLength}."
        )
      )
    }
  }

  private var isOpen: Boolean = true

  override def close(): Unit = {
    if (isOpen) { // allow multiple closes
      isOpen = false
      val ba = baos.toByteArray
      val baLen = ba.length
      Assert.invariant(baLen <= layer.getLength)
      val buf = ByteBuffer.wrap(ba)
      layer.setChecksum(layer.compute(isUnparse = true, buf))
      jos.write(ba)
      jos.flush()
    }
  }
}
