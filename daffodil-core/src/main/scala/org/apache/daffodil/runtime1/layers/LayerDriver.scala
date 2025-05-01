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
import java.io.InputStream
import java.io.OutputStream
import scala.jdk.CollectionConverters._

import org.apache.daffodil.io.DataInputStream.Mark
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessingError

import passera.unsigned.ULong

/**
 * Mandatory factory to create and initialize the layer driver object.
 */
object LayerDriver {
  def apply(state: ParseOrUnparseState, lrd: LayerRuntimeData): LayerDriver = {
    val lvr =
      state.dataProc.get.ssrd.getLayerVarsRuntime(lrd) // could fail if layer not SPI loaded
    val lr = new LayerRuntime(state, lrd, lvr) // can't fail
    val newLayer = lvr.constructInstance() // can't fail
    newLayer.setLayerRuntime(lr) // can't fail
    val instance = new LayerDriver(newLayer) // can't fail
    instance
  }

  def handleThrowableWithoutLayer(t: Throwable) = {
    t match {
      case u: UnsuppressableException => throw t
      case pe: ProcessingError => throw pe // already a PE. Do not further mess with it.
      case rsde: RuntimeSchemaDefinitionError =>
        throw rsde
      case re: RuntimeException =>
        throw new LayerFatalException(re)
      case e: Exception =>
        throw new LayerFatalException(e)
    }
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

  def init(): Unit = {
    try {
      // timing here is subtle.
      // This could throw, but the steps after this are necessary
      // in order to properly handle the thrown exception
      //
      layer.getLayerRuntime.layerVarsRuntime.callParamSetterWithParameterVars(this.layer)
    } finally {
      //
      // because we want to deal with RuntimeException as if it was NOT a subclass of
      // Exception, we first divide up the classes as to whether they are RuntimeExceptions or not.
      val (runtimeExceptionSubClasses, regularExceptionSubclasses) =
        layer.getProcessingErrorExceptions.asScala.partition { c =>
          classOf[RuntimeException].isAssignableFrom(c)
        }

      setLayerThrowHandler(new LayerThrowHandler() {
        override def handleThrow(t: Throwable): Unit = {
          val tc = t.getClass
          val isRuntimeException = classOf[RuntimeException].isAssignableFrom(tc)
          if (isRuntimeException) {
            if (runtimeExceptionSubClasses.exists { ec => ec.isAssignableFrom(tc) })
              layer.processingError(t)
          } else {
            if (regularExceptionSubclasses.exists { ec => ec.isAssignableFrom(tc) })
              layer.processingError(t)
          }
        }
      })
    }
  }

  def layerRuntime: LayerRuntime = layer.getLayerRuntime

  private def layerVarsRuntime = layerRuntime.layerVarsRuntime

  private def wrapJavaInputStream(s: InputSourceDataInputStream) =
    new JavaIOInputStream(s, layer)

  private def wrapJavaOutputStream(s: DataOutputStream) =
    new JavaIOOutputStream(s, layer)

  /**
   * Override these if we ever have transformers that don't have these
   * requirements.
   */
  val mandatoryLayerAlignmentInBits: Int = 8

  final def addInputLayer(s: InputSourceDataInputStream): InputSourceDataInputStream = {
    val jis = wrapJavaInputStream(s)
    // This wrapLayerInput could fail via a throw. It's calling layer code.
    val wrapped = layer.wrapLayerInput(jis)
    // This assured close thing is just in case the user's layer stream doesn't propagate
    // the close. That would be incorrect of it, but we want to tolerate that not happening.
    val decodedInputStream = new ThrowProtectedAssuredCloseInputStream(wrapped, jis)
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
    } finally {
      s.close()
    }

  /**
   * Note that there is addOutputLayer, but there is no remove output layer because
   * unparsing is very asynchronous due to suspensions. The unwinding of this
   * layering for unparsing can only happen in the close() of the layer stream.
   */
  final def addOutputLayer(s: DataOutputStream): DirectOrBufferedDataOutputStream = {
    val jos = wrapJavaOutputStream(s)
    val wrappedStream = layer.wrapLayerOutput(jos)
    val encodedOutputStream = new ThrowProtectedAssuredCloseOutputStream(wrappedStream, jos)
    val newDOS = DirectOrBufferedDataOutputStream(
      encodedOutputStream,
      null,
      isLayer = true,
      s.chunkSizeInBytes,
      s.maxBufferSizeInBytes,
      s.tempDirPath
    )
    newDOS.setPriorBitOrder(BitOrder.MostSignificantBitFirst)
    newDOS.setAbsStartingBitPos0b(ULong(0L))
    newDOS.setDebugging(s.areDebugging)
    newDOS
  }

  def handleThrowable(t: Throwable) = {
    t match {
      case u: UnsuppressableException => throw t
      case pe: ProcessingError => throw pe // already a PE. Do not further mess with it.
      case _ =>
        // if the layer supplies a handler, then
        // we invoke it.
        val layerHandler = getLayerThrowHandler
        if (layerHandler ne null)
          layerHandler.handleThrow(t)
        // if we get here, the layer handler didn't exist or
        // it didn't handle the throwable.
        LayerDriver.handleThrowableWithoutLayer(t)
    }
  }

  private final class ThrowProtectedAssuredCloseInputStream(
    stream: InputStream,
    inner: InputStream
  ) extends FilterInputStream(stream) {

    override def read(): Int = {
      try {
        super.read()
      } catch {
        case t: Throwable => handleThrowable(t)
      }
    }

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      try {
        super.read(b, off, len)
      } catch {
        case t: Throwable => handleThrowable(t)
      }
    }

    override def close(): Unit = {
      try {
        stream.close()
        inner.close()
      } catch {
        case t: Throwable => handleThrowable(t)
      }
    }
  }

  /**
   * Besides making sure the inner stream gets closed, this also
   * provides handling of exceptions thrown by the layer-author's code
   * (or things that that code calls).
   *
   * This is needed because unparsing can involve suspensions, so these
   * calls can happen long after the call stack of unparsers is over; hence,
   * any nested try/catches around the actual unparser calls may not be
   * surrounding these actions.
   *
   * To get sensible behavior even if these throws happen late due to
   * suspensions, we have to encapsulate here with our own throw/catches.
   *
   * @param stream the stream we're encapsulating
   * @param inner  the stream we're assuring will be closed even if a close on the primary stream doesn't propagate the stream
   */
  private final class ThrowProtectedAssuredCloseOutputStream(
    stream: OutputStream,
    inner: OutputStream
  ) extends OutputStream {

    /**
     * We hope this isn't get used because a try-catch around every individual write of a byte
     * is inefficient certainly.
     *
     * @param b
     */
    override def write(b: Int): Unit = {
      try {
        stream.write(b)
      } catch {
        case t: Throwable =>
          handleThrowable(t)
      }
    }

    /**
     * We hope this is the way most writes are done and with fairly large buffers to amortize the
     * cost of the try/catch logic over many bytes of output.
     *
     * @param b
     * @param off
     * @param len
     */
    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      try {
        stream.write(b, off, len)
      } catch {
        case t: Throwable =>
          handleThrowable(t)
      }
    }

    override def close(): Unit = {
      try {
        stream.close()
        inner.close()
      } catch {
        case t: Throwable =>
          handleThrowable(t)
      }
    }
  }

  private var lth: LayerThrowHandler = null

  trait LayerThrowHandler {
    def handleThrow(t: Throwable): Unit
  }

  /**
   * If you set a layer throw handler, then if the layer code throws
   * anything, the handler is invoked and can convert specific kinds of
   * exceptions into processing errors or runtime SDEs.
   *
   * If the handler does not throw or call processingError or
   * runtimeSchemaDefinitionError, then the throw is treated as
   * if there was no handler, and propagates generally as a fatal error.
   *
   * @param lth the throw handler
   */
  def setLayerThrowHandler(lth: LayerThrowHandler): Unit = {
    this.lth = lth
  }

  def getLayerThrowHandler: LayerThrowHandler = {
    return this.lth
  }

}

/**
 * This is used to encapsulate runtime exceptions that are thrown out of layer code.
 */
class LayerFatalException(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
  def this(msg: String) = this(msg, null)

  /**
   * When constructed with just a cause, we also pass a message if the cause provides one
   * because the SLF4J logging system doesn't synthesize a message from the cause if only
   * a cause is provided.
   * @param cause
   */
  def this(cause: Throwable) =
    this(
      {
        if (cause.getMessage == null)
          Misc.getNameFromClass(cause)
        else
          cause.getMessage
      },
      cause
    )
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
      } finally {
        closed = true
      }
    }
  }
}
