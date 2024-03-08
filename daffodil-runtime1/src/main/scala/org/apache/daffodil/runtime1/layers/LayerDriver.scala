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

import java.io.FilterInputStream
import java.io.FilterOutputStream
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
import org.apache.daffodil.runtime1.processors.unparsers.UState

import passera.unsigned.ULong

/**
 * Mandatory factory to create and initialize the layer driver object.
 */
object LayerDriver {
  def apply(
    layerRuntimeImpl: LayerRuntimeImpl,
    layer: Layer,
    layerVarsRuntime: LayerVarsRuntime,
  ): LayerDriver = {
    val instance = new LayerDriver(layerRuntimeImpl, layer, layerVarsRuntime)
    instance.init()
    instance
  }
}

/**
 * Driving mechanism that incorporates a layer at runtime into the I/O streams
 * to transform the data stream and optionally receive parameter values from,
 * and populate result values back into DFDL variables.
 *
 * A layer driver is created at runtime as part of a single parse/unparse call.
 * Hence, they can be stateful without causing thread-safety issues.
 *
 * @param layerRuntimeImpl runtime state including I/O streams to wrap, and the actual variable contents to read
 *                         for initializing the layer from DFDL variables, and writable variables to write back
 *                         and results from the layer to DFDL variables.
 * @param layer            The Layer instance to parameterize, drive, and get results back from.
 * @param layerVarsRuntime Result of the LayerRuntimeCompiler's compilation of the Layer class with the layer's
 *                         DFDL Variables.
 */
class LayerDriver private (
  layerRuntimeImpl: LayerRuntimeImpl,
  layer: Layer,
  layerVarsRuntime: LayerVarsRuntime,
) {

  private def init(): Unit =
    layerVarsRuntime.callParamSetterWithParameterVars(layer, layerRuntimeImpl)

  private def wrapJavaInputStream(
    s: InputSourceDataInputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ) =
    new JavaIOInputStream(s, layerRuntimeImpl.finfo)

  private def wrapJavaOutputStream(s: DataOutputStream, layerRuntimeImpl: LayerRuntimeImpl) =
    new JavaIOOutputStream(s, layer, layerRuntimeImpl, layerVarsRuntime)

  /**
   *  Override these if we ever have transformers that don't have these
   *  requirements.
   */
  val mandatoryLayerAlignmentInBits: Int = 8

  final def addInputLayer(
    s: InputSourceDataInputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ): InputSourceDataInputStream = {
    val jis = wrapJavaInputStream(s, layerRuntimeImpl)
    val decodedInputStream =
      new AssuredCloseInputStream(layer.wrapLayerInput(jis, layerRuntimeImpl), jis)
    val newDIS = InputSourceDataInputStream(decodedInputStream)
    // must initialize priorBitOrder
    newDIS.cst.setPriorBitOrder(BitOrder.MostSignificantBitFirst)
    newDIS.setDebugging(s.areDebugging)
    newDIS
  }

  /**
   * Parsing works as a tree traversal, so when the parser unwinds from
   * parsing the layer we can invoke this to handle cleanups, and
   * finalization issues like assigning the result variables
   */
  final def removeInputLayer(
    s: InputSourceDataInputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ): Unit =
    layerVarsRuntime.callGettersToPopulateResultVars(layer, layerRuntimeImpl)

  final def addOutputLayer(
    s: DataOutputStream,
    layerRuntimeImpl: LayerRuntimeImpl,
  ): DirectOrBufferedDataOutputStream = {
    val jos = wrapJavaOutputStream(s, layerRuntimeImpl)
    val encodedOutputStream =
      new AssuredCloseOutputStream(layer.wrapLayerOutput(jos, layerRuntimeImpl), jos)
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

  /**
   * Unparsing is very asynchronous due to suspensions.
   * So what is done here has to arrange for the layer stream to be closed,
   * but in the future when everything that has been written to it
   * but was suspended, has actually been written.
   */
  final def removeOutputLayer(s: DirectOrBufferedDataOutputStream, state: UState): Unit = {
    // do nothing
  }

  private final class AssuredCloseInputStream(stream: InputStream, inner: InputStream)
    extends FilterInputStream(stream) {
    override def close(): Unit = {
      stream.close()
      inner.close()
    }
  }

  private final class AssuredCloseOutputStream(stream: OutputStream, inner: OutputStream)
    extends FilterOutputStream(stream) {
    override def close(): Unit = {
      stream.close()
      inner.close()
    }
  }

}

/**
 * Turns Daffodil's bits-capable InputSourceDataInputStream objects into ordinary
 * java InputStream API for byte-oriented reads.
 *
 * @param s the Daffodil InputSourceDataInputStream
 * @param finfo the format info (a view trait on a PState/UState) needed for bit order considerations.
 * @return a java.io.InputStream which when read from, delegates to the InputSourceDataInputStream
 */
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

  /**
   * @param readlimit Ignored. The limits of daffodil's input system are specified
   *                  elsewhere. See BucketingInputSource in the daffodil-io module.
   */
  override def mark(readlimit: Int): Unit = {
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

/**
 * Turns a Daffodil DataOutputStream into an ordinary java.io.OutputStream.
 *
 * @param dos   The DataOutputStream to write the data to.
 */
class JavaIOOutputStream(
  dos: DataOutputStream,
  layer: Layer,
  lri: LayerRuntimeImpl,
  lvr: LayerVarsRuntime,
) extends OutputStream {

  private def finfo = lri.finfo

  private var closed = false

  private var nBytes = 0

  override def write(b: Int): Unit = {
    val wasWritten = dos.putLong(b, 8, finfo)
    if (wasWritten) nBytes += 1
  }

  override def close(): Unit = {
    if (!closed) {
      dos.setFinished(finfo)
      //
      // This is where we can reliably wrap-up the layer processing.
      //
      lvr.callGettersToPopulateResultVars(layer, lri)
      closed = true
    }
  }
}
