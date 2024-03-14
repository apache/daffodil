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
import java.lang.reflect.InvocationTargetException

import org.apache.daffodil.io.DataInputStream.Mark
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.unparsers.UState

import passera.unsigned.ULong

/**
 * Mandatory factory to create and initialize the layer driver object.
 */
object LayerDriver {
  def apply(state: ParseOrUnparseState, lrd: LayerRuntimeData): LayerDriver = {
    val lvr = LayerRuntimeCompiler.compile(lrd) // could fail if layer not SPI loaded
    val lr = new LayerRuntime(state, lrd, lvr) // can't fail
    val newLayer = lvr.constructInstance() // can't fail
    newLayer.setLayerRuntime(lr) // can't fail
    val instance = new LayerDriver(newLayer) // can't fail
    lvr.callParamSetterWithParameterVars(newLayer) // could fail if user's method causes errors
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
 * @param layer            The Layer instance to parameterize, drive, and get results back from.
 */
class LayerDriver private (val layer: Layer) {

  def layerRuntime: LayerRuntime = layer.getLayerRuntime
  private def layerVarsRuntime = layerRuntime.layerVarsRuntime

  private def wrapJavaInputStream(s: InputSourceDataInputStream) =
    new JavaIOInputStream(s, layer)

  private def wrapJavaOutputStream(s: DataOutputStream) =
    new JavaIOOutputStream(s, layer)

  /**
   *  Override these if we ever have transformers that don't have these
   *  requirements.
   */
  val mandatoryLayerAlignmentInBits: Int = 8

  final def addInputLayer(s: InputSourceDataInputStream): InputSourceDataInputStream = {
    val jis = wrapJavaInputStream(s)
    // This wrapLayerInput could fail via a throw. It's calling layer code.
    val wrapped = layer.wrapLayerInput(jis)
    // This assured close thing is just in case the user's layer stream doesn't propagate
    // the close. That would be incorrect of it, but we want to tolerate that not happening.
    val decodedInputStream = new AssuredCloseInputStream(wrapped, jis)
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
  final def removeInputLayer(s: InputSourceDataInputStream): Unit =
    try {
      layerVarsRuntime.callGettersToPopulateResultVars(layer) // could throw.
    } catch {
      case ite: InvocationTargetException =>
        layer.processingError(ite.getCause)
    } finally {
      s.close()
    }

  final def addOutputLayer(s: DataOutputStream): DirectOrBufferedDataOutputStream = {
    val jos = wrapJavaOutputStream(s)
    val encodedOutputStream =
      new AssuredCloseOutputStream(layer.wrapLayerOutput(jos), jos)
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
 * @return a java.io.InputStream which when read from, delegates to the InputSourceDataInputStream
 */
class JavaIOInputStream(s: InputSourceDataInputStream, layer: Layer) extends InputStream {
  private def finfo = layer.getLayerRuntime.finfo
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
    // do nothing. Important. We do not want the close to cascade beyond here.
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
 * Turns a Daffodil bits-capable DataOutputStream into an ordinary java.io.OutputStream.
 *
 * @param dos   The DataOutputStream to write the data to.
 */
class JavaIOOutputStream(dos: DataOutputStream, layer: Layer) extends OutputStream {

  private def lr = layer.getLayerRuntime
  private def lvr = lr.layerVarsRuntime
  private def finfo = lr.finfo

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
      try {
        lvr.callGettersToPopulateResultVars(layer)
      } catch {
        case ite: InvocationTargetException =>
          layer.processingError(ite.getCause)
      } finally {
        closed = true
      }
    }
  }
}
