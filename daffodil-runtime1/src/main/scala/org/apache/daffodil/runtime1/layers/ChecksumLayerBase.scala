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
import org.apache.daffodil.runtime1.api.DFDLPrimType
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerCompileInfo
import org.apache.daffodil.runtime1.layers.api.LayerNotEnoughDataException
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.layers.api.LayerVariable

import org.apache.commons.io.IOUtils

/**
 * Suitable only for checksums computed over small sections of data, not large data streams or whole files.
 *
 * The entire region of data the checksum is being computed over, will be pulled into a byte buffer in memory.
 */

abstract class ChecksumLayerBase(layerName: String, protected final val layerNamespace: String)
  extends Layer(layerName) {

  var checksumVar: LayerVariable = _
  var checksumLayerLengthVar: LayerVariable = _

  override def check(lci: LayerCompileInfo): Unit = {
    if (checksumVar == null) {
      checksumVar = lci.layerVariable(layerNamespace, layerName)
      checksumLayerLengthVar = lci.layerVariable(layerNamespace, layerName + "LayerLength")
      if (checksumVar.dfdlPrimType != DFDLPrimType.UnsignedShort)
        lci.schemaDefinitionError(s"Layer variable '$layerName' is not an 'xs:unsignedShort.")
      if (checksumLayerLengthVar.dfdlPrimType != DFDLPrimType.UnsignedShort)
        lci.schemaDefinitionError(
          s"Layer variable '${layerName + "LayerLength"}' is not an 'xs:unsignedShort'.",
        )
    }
  }

  def compute(
    layerRuntime: LayerRuntime,
    isUnparse: Boolean,
    byteBuffer: ByteBuffer,
  ): Int

  final override def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream = {
    check(lr)
    new ChecksumDecoderInputStream(this, jis, lr)
  }

  final override def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream = {
    check(lr)
    new ChecksumEncoderOutputStream(this, jos, lr)
  }
}

class ChecksumDecoderInputStream(
  layer: ChecksumLayerBase,
  jis: InputStream,
  lr: LayerRuntime,
) extends InputStream {

  private val checksumLayerLength = {
    val res = lr.getInt(layer.checksumLayerLengthVar)
    Assert.invariant(res > 0)
    res
  }

  private def doubleCheckLength(actualDataLen: Int): Unit = {
    val neededLen = checksumLayerLength
    if (neededLen > actualDataLen)
      lr.processingError(new LayerNotEnoughDataException(neededLen, actualDataLen))
  }

  private lazy val bais = {
    val ba = new Array[Byte](checksumLayerLength)
    val nRead = IOUtils.read(jis, ba)
    doubleCheckLength(nRead)
    val buf = ByteBuffer.wrap(ba)
    val checksum = layer.compute(lr, isUnparse = false, buf)
    lr.setInt(layer.checksumVar, checksum)
    new ByteArrayInputStream(ba)
  }

  override def read(): Int = bais.read()
}

class ChecksumEncoderOutputStream(
  layer: ChecksumLayerBase,
  jos: OutputStream,
  lr: LayerRuntime,
) extends OutputStream {

  private val checksumLayerLength = {
    val res = lr.getInt(layer.checksumLayerLengthVar)
    Assert.invariant(res > 0)
    res
  }

  private lazy val baos = new ByteArrayOutputStream(checksumLayerLength)

  private var count: Long = 0

  override def write(b: Int): Unit = {
    baos.write(b)
    count += 1
    if (count == checksumLayerLength) {
      // we can auto-close it in this case
      close()
    } else if (count > checksumLayerLength) {
      //
      // This could happen if the layer logically unparses as one of two choice branches where they
      // are supposed to be all the same length, but one is in fact longer than expected by the bufLen.
      lr.processingError(
        new IndexOutOfBoundsException(
          s"Written data amount exceeded fixed layer length of $checksumLayerLength.",
        ),
      )
    } else {
      // Assert.invariant(count < checksumLayerLength)
      // ok. We're still accumulating data
    }
  }

  private var isOpen: Boolean = true

  override def close(): Unit = {
    if (isOpen) { // allow multiple closes
      isOpen = false
      val ba = baos.toByteArray
      val baLen = ba.length
      Assert.invariant(baLen <= checksumLayerLength)
      val buf = ByteBuffer.wrap(ba)
      val checksum = layer.compute(lr, isUnparse = true, buf)
      jos.write(ba)
      // assign checksum to the first variable.
      lr.setInt(layer.checksumVar, checksum)
      jos.close() // required so that closes propagate, and the buffering output streams recombine/collapse again.
    }
  }
}
