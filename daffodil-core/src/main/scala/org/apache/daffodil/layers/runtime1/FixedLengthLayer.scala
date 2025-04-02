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
import org.apache.daffodil.runtime1.layers.LayerNotEnoughDataException
import org.apache.daffodil.runtime1.layers.api.Layer

import org.apache.commons.io.IOUtils

/**
 * Suitable only for small sections of data, not large data streams or whole files.
 * See the maxFixedLength value defined herein for the maximum.
 *
 * The entire fixed length region of the data will be pulled into a byte buffer in memory.
 *
 * TODO: Someday, enhance to make this streaming.
 *
 * One DFDL Variable is a parameter
 *   - fixedLength - an unsignedInt giving the fixed length of this layer.
 *   This length is enforced on both parsing and unparsing the layer.
 * There are no output/result DFDL variables from this layer.
 */
final class FixedLengthLayer
  extends Layer("fixedLength", "urn:org.apache.daffodil.layers.fixedLength") {

  private var fixedLength: Int = -1

  private def maxFixedLength = Short.MaxValue

  /**
   * Captures the fixedLength DFDL variable value and saves it.
   *
   * Also validates whether it is in acceptable range. Because the fixedLength variable
   * can be populated from an expression that consumes input data, it's possible for the
   * fixedLength to be out of range as a result of speculative parsing down
   * an incorrect path. Hence, it is a processing error, not a schema definition error
   * if the fixedLength variable value is out of range.
   *
   * @param fixedLength matches the name of the DFDL variable which will supply this value.
   */
  private[layers] def setLayerVariableParameters(fixedLength: Long): Unit = {
    this.fixedLength = fixedLength.toInt
    Assert.invariant(fixedLength >= 0) // variable is unsignedInt, so this can't be negative
    if (fixedLength > maxFixedLength)
      processingError(
        s"fixedLength value of $fixedLength is above the maximum of $maxFixedLength."
      )
  }

  override def wrapLayerInput(jis: InputStream): InputStream = {
    new FixedLengthInputStream(jis)
  }

  override def wrapLayerOutput(jos: OutputStream): OutputStream = {
    new FixedLengthOutputStream(jos)
  }

  class FixedLengthInputStream(jis: InputStream) extends InputStream {

    private lazy val bais = {
      val ba = new Array[Byte](fixedLength.toInt)
      val nRead = IOUtils.read(jis, ba)
      if (nRead < fixedLength)
        processingError(new LayerNotEnoughDataException(fixedLength, nRead))
      val buf = ByteBuffer.wrap(ba)
      new ByteArrayInputStream(ba)
    }

    override def read(): Int = bais.read()
  }

  class FixedLengthOutputStream(jos: OutputStream) extends OutputStream {

    private lazy val baos = new ByteArrayOutputStream(fixedLength)

    private var count: Long = 0

    override def write(b: Int): Unit = {
      baos.write(b)
      count += 1
      if (count == fixedLength) {
        // we can auto-close it in this case
        close()
      } else if (count > fixedLength) {
        //
        // This could happen if the layer logically unparses as one of two choice branches where they
        // are supposed to be all the same length, but one is in fact longer than expected by the bufLen.
        processingError(
          new IndexOutOfBoundsException(
            s"Written data amount exceeded fixed layer length of $fixedLength."
          )
        )
      } else {
        // ok. We're still accumulating data
      }
    }

    private var isOpen: Boolean = true

    override def close(): Unit = {
      if (isOpen) { // allow multiple closes
        isOpen = false
        val ba = baos.toByteArray
        val baLen = ba.length
        if (baLen != fixedLength)
          processingError(
            s"Insufficient output data for fixed-length layer. Needed $fixedLength bytes, but only $baLen were unparsed."
          )
        jos.write(ba)
      }
    }
  }
}
