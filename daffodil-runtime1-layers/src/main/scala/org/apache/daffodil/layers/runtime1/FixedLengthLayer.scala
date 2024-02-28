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
package org.apache.daffodil.layers.runtime1

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.api.DFDLPrimType
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerCompileInfo
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.layers.api.LayerVariable

import org.apache.commons.io.IOUtils

/**
 * Suitable only for small sections of data, not large data streams or whole files.
 *
 * The entire fixed length region of the data will be pulled into a byte buffer in memory.
 *
 * TODO: Enhance to make this streaming.
 */
final class FixedLengthLayer extends Layer("fixedLength") {

  var layerLengthVar: LayerVariable = _

  private def init(lci: LayerCompileInfo): Unit = {
    if (layerLengthVar == null) {
      layerLengthVar = lci.layerVariable("urn:org.apache.daffodil.layers.fixedLength", name())
      if (layerLengthVar.dfdlPrimType != DFDLPrimType.UnsignedInt)
        lci.schemaDefinitionError(
          "Layer variable 'fixedLength' is not an 'xs:unsignedInt'.",
        )
    }
  }

  override def check(lci: LayerCompileInfo): Unit = init(lci)

  override def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream = {
    init(lr)
    new FixedLengthInputStream(this, jis, lr)
  }

  override def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream = {
    init(lr)
    new FixedLengthOutputStream(this, jos, lr)
  }
}

class FixedLengthInputStream(
  layer: FixedLengthLayer,
  jis: InputStream,
  lr: LayerRuntime,
) extends InputStream {

  private val layerLength = {
    val res = lr.getLong(layer.layerLengthVar)
    Assert.invariant(res > 0)
    res
  }

  private lazy val bais = {
    val ba = new Array[Byte](layerLength.toInt)
    val nRead = IOUtils.read(jis, ba)
    if (nRead < layerLength)
      lr.processingError(s"Insufficient data for fixed-length layer. Needed $layerLength bytes, but only $nRead were available.")
    val buf = ByteBuffer.wrap(ba)
    new ByteArrayInputStream(ba)
  }

  override def read(): Int = bais.read()
}

class FixedLengthOutputStream(
  layer: FixedLengthLayer,
  jos: OutputStream,
  lr: LayerRuntime,
) extends OutputStream {

  private val layerLength = {
    val res = lr.getLong(layer.layerLengthVar)
    Assert.invariant(res > 0)
    res
  }

  private lazy val baos = new ByteArrayOutputStream(layerLength.toInt)

  private var count: Long = 0

  override def write(b: Int): Unit = {
    baos.write(b)
    count += 1
    if (count == layerLength) {
      // we can auto-close it in this case
      close()
    } else if (count > layerLength) {
      //
      // This could happen if the layer logically unparses as one of two choice branches where they
      // are supposed to be all the same length, but one is in fact longer than expected by the bufLen.
      lr.processingError(
        new IndexOutOfBoundsException(
          s"Written data amount exceeded fixed layer length of $layerLength.",
        ),
      )
    } else {
      // Assert.invariant(count < layerLength)
      // ok. We're still accumulating data
    }
  }

  private var isOpen: Boolean = true

  override def close(): Unit = {
    if (isOpen) { // allow multiple closes
      isOpen = false
      val ba = baos.toByteArray
      val baLen = ba.length
      if (baLen != layerLength)
        lr.processingError(s"Insufficient output data for fixed-length layer. Needed $layerLength bytes, but only $baLen were unparsed.")
      jos.write(ba)
      // assign checksum to the first variable.
      jos.close() // required so that closes propagate, and the buffering output streams recombine/collapse again.
    }
  }
}
