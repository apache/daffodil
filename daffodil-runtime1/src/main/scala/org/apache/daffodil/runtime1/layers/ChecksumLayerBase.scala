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
import org.apache.daffodil.runtime1.layers.api.LayerNotEnoughDataException
import org.apache.daffodil.runtime1.layers.api.LayerRuntime

import org.apache.commons.io.IOUtils

/**
 * A base class for ChecksumLayer - hides details from the API user who is deriving a
 * concrete class from ChecksumLayer.
 *
 * Suitable only for checksums computed over small sections of data, not large data streams or whole files.
 */
abstract class ChecksumLayerBase(
  localName: String,
  namespace: String,
) extends Layer(localName, namespace) {

  private var checksum: Int = -1

  private var length: Int = -1
  private var isInitialized: Boolean = false

  final def getLength: Int = length

  /**
   * Must be called to specify the length, or it is a schema definition error.
   * @param len The fixed length over which the checksum is computed.
   */
  final protected def setLength(len: Int): Unit = {
    this.length = len
    if (len < 1) throw new Exception(s"The length is not greater than zero: $len.")
    isInitialized = true
  }

  final protected def getChecksum: Int = checksum
  final private[layers] def setChecksum(checksum: Int): Unit = { this.checksum = checksum }

  def compute(
    layerRuntime: LayerRuntime,
    isUnparse: Boolean,
    byteBuffer: ByteBuffer,
  ): Int

  final override def wrapLayerInput(jis: InputStream, lr: LayerRuntime): InputStream = {
    if (!isInitialized) lr.runtimeSchemaDefinitionError("setLength method was never called.")
    new ChecksumDecoderInputStream(this, jis, lr)
  }

  final override def wrapLayerOutput(jos: OutputStream, lr: LayerRuntime): OutputStream = {
    if (!isInitialized) lr.runtimeSchemaDefinitionError("setLength method was never called.")
    new ChecksumEncoderOutputStream(this, jos, lr)
  }
}

class ChecksumDecoderInputStream(
  layer: ChecksumLayerBase,
  jis: InputStream,
  lr: LayerRuntime,
) extends InputStream {

  private def doubleCheckLength(actualDataLen: Int): Unit = {
    val neededLen = layer.getLength
    if (neededLen > actualDataLen)
      lr.processingError(new LayerNotEnoughDataException(lr, neededLen, actualDataLen))
  }

  private lazy val bais = {
    val ba = new Array[Byte](layer.getLength)
    val nRead = IOUtils.read(jis, ba)
    doubleCheckLength(nRead)
    val buf = ByteBuffer.wrap(ba)
    layer.setChecksum(layer.compute(lr, isUnparse = false, buf))
    new ByteArrayInputStream(ba)
  }

  override def read(): Int = bais.read()
}

class ChecksumEncoderOutputStream(
  layer: ChecksumLayerBase,
  jos: OutputStream,
  lr: LayerRuntime,
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
      lr.processingError(
        new IndexOutOfBoundsException(
          s"Written data amount exceeded fixed layer length of ${layer.getLength}.",
        ),
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
      layer.setChecksum(layer.compute(lr, isUnparse = true, buf))
      jos.write(ba)
      jos.flush()
    }
  }
}
