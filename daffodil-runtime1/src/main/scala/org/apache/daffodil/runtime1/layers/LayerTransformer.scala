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

import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.io.DataInputStream.Mark
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerCompileInfo
import org.apache.daffodil.runtime1.processors.unparsers.UState

import passera.unsigned.ULong

/**
 * Shared functionality of all LayerTransformers.
 *
 * A layer transformer is created at runtime as part of a single parse/unparse call.
 * Hence, they can be stateful without causing thread-safety issues.
 */
class LayerTransformer(layerCompileInfo: LayerCompileInfo, layer: Layer) {

  private def wrapJavaInputStream(
    s: InputSourceDataInputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ): InputStream =
    new JavaIOInputStream(s, layerRuntimeImpl.state)

  private def wrapJavaOutputStream(
    s: DataOutputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ): OutputStream =
    new JavaIOOutputStream(s, layerRuntimeImpl.state)

  /**
   *  Override these if we ever have transformers that don't have these
   *  requirements.
   */
  val mandatoryLayerAlignmentInBits: Int = 8

  final def addLayer(
    s: InputSourceDataInputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ): InputSourceDataInputStream = {
    val jis = wrapJavaInputStream(s, layerRuntimeImpl)
    val decodedInputStream = layer.wrapLayerDecoder(jis, layerRuntimeImpl)
    val newDIS = InputSourceDataInputStream(decodedInputStream)
    newDIS.cst.setPriorBitOrder(
      BitOrder.MostSignificantBitFirst,
    ) // must initialize priorBitOrder
    newDIS.setDebugging(s.areDebugging)
    newDIS
  }

  final def removeLayer(s: InputSourceDataInputStream): Unit = {
    // nothing for now
  }

  final def addLayer(
    s: DataOutputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ): DirectOrBufferedDataOutputStream = {
    val jos = wrapJavaOutputStream(s, layerRuntimeImpl)
    val encodedOutputStream = layer.wrapLayerEncoder(jos, layerRuntimeImpl)
    val newDOS = DirectOrBufferedDataOutputStream(
      encodedOutputStream,
      null,
      isLayer = true,
      s.chunkSizeInBytes,
      s.maxBufferSizeInBytes,
      s.tempDirPath,
    )
    newDOS.setPriorBitOrder(BitOrder.MostSignificantBitFirst)
    newDOS.setAbsStartingBitPos0b(ULong(0L))
    newDOS.setDebugging(s.areDebugging)
    newDOS
  }

  final def removeLayer(s: DirectOrBufferedDataOutputStream, state: UState): Unit = {
    //
    // Because the layer may have suspensions that will write to it long after
    // this unparser has left the stack, it is not clear we can do any
    // cleanup of resources here.
    //
  }
}

class JavaIOInputStream(s: InputSourceDataInputStream, finfo: FormatInfo) extends InputStream {

  private lazy val id = Misc.getNameFromClass(this)

  override def read(): Int = {
    if (!s.isDefinedForLength(8)) -1
    else {
      val ul = s.getUnsignedLong(8, finfo)
      val byte: Int = ul.toInt & 0xff
      byte
    }
  }

  override def available(): Int = 0

  override def close(): Unit = {
    // do nothing
  }

  override def mark(readlimit: Int): Unit = {
    Assert.usage(maybeSavedMark.isEmpty)
    maybeSavedMark = One(s.mark(id))
  }

  private var maybeSavedMark: Maybe[Mark] = Nope

  override def reset(): Unit = {
    Assert.usage(maybeSavedMark.isDefined)
    s.reset(maybeSavedMark.get)
    maybeSavedMark = Nope
  }

  override def markSupported() = true
}

class JavaIOOutputStream(dos: DataOutputStream, finfo: FormatInfo) extends OutputStream {

  private var closed = false

  def nBytesWritten = nBytes

  private var nBytes = 0

  override def write(b: Int): Unit = {
    val wasWritten = dos.putLong(b, 8, finfo)
    if (wasWritten) nBytes += 1
  }

  override def close(): Unit = {
    if (!closed) {
      dos.setFinished(finfo)
      closed = true
    }
  }
}
