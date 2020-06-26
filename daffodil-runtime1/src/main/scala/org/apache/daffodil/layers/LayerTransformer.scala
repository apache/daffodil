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

package org.apache.daffodil.layers

import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.processors.LayerLengthInBytesEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import java.util.HashMap
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.util.NonAllocatingMap
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.DataInputStream.Mark
import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import passera.unsigned.ULong
import org.apache.daffodil.dsom.DPathCompileInfo

/**
 * Factory for a layer transformer.
 *
 * Responsible for digesting the various args, erroring if the wrong ones
 * are specified, and ultimately constructing the LayerTransformer
 * of the correct type with the parameters it needs.
 */
abstract class LayerTransformerFactory(nom: String)
  extends Serializable {

  val name = nom.toUpperCase()

  def newInstance(
    maybeLayerCharsetEv: Maybe[LayerCharsetEv],
    maybeLayerLengthKind: Maybe[LayerLengthKind],
    maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
    maybeLayerLengthUnits: Maybe[LayerLengthUnits],
    maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
    tci: DPathCompileInfo): LayerTransformer
}

/**
 * Transformers have factories. This lets you find the transformer factory
 * by the name obtained from dfdlx:layerTransform.
 */
object LayerTransformerFactory {

  private lazy val transformerMap = new NonAllocatingMap(new HashMap[String, LayerTransformerFactory])

  def register(factory: LayerTransformerFactory): Unit = {
    transformerMap.put(factory.name, factory)
  }
  /**
   * Given name, finds the factory for the transformer. SDE otherwise.
   *
   * The state is passed in order to provide diagnostic context if not found.
   */
  def find(name: String, state: ParseOrUnparseState): LayerTransformerFactory = {
    val maybeFactory = transformerMap.get(name)
    if (maybeFactory.isEmpty) {
      val choices = transformerMap.keySet.mkString(", ")
      state.SDE("The dfdlx:layerTransform '%s' was not found. Available choices are: %s", name, choices)
    } else {
      maybeFactory.get
    }
  }

  /**
   * All transformers must be registered so they are available by name.
   *
   * It is possible to package a transformer in a separate jar also, but then
   * someone has to register it by calling the register method.
   *
   * Transformers built into the primary Daffodil jars/packages should be
   * registered here.
   */
  register(Base64MIMETransformerFactory)
  register(GZIPTransformerFactory)
  register(IMFLineFoldedTransformerFactory)
  register(ICalendarLineFoldedTransformerFactory)
  register(AISPayloadArmoringTransformerFactory)
  register(FourByteSwapTransformerFactory)
}

/**
 * Shared functionality of all LayerTransformers.
 */
abstract class LayerTransformer()
  extends Serializable {

  protected def wrapLayerDecoder(jis: java.io.InputStream): java.io.InputStream

  protected def wrapLimitingStream(jis: java.io.InputStream, state: PState): java.io.InputStream

  def wrapJavaInputStream(s: InputSourceDataInputStream, fInfo: FormatInfo): java.io.InputStream = {
    new JavaIOInputStream(s, fInfo)
  }

  protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream

  protected def wrapLimitingStream(jis: java.io.OutputStream, state: UState): java.io.OutputStream

  def wrapJavaOutputStream(s: DataOutputStream, fInfo: FormatInfo): java.io.OutputStream = {
    new JavaIOOutputStream(s, fInfo)
  }
  /**
   *  Override these if we ever have transformers that don't have these
   *  requirements.
   */
  val mandatoryLayerAlignmentInBits: Int = 8
  val mandatoryLengthUnit: LayerLengthUnits = LayerLengthUnits.Bytes

  def addLayer(s: InputSourceDataInputStream, state: PState): InputSourceDataInputStream = {
    val jis = wrapJavaInputStream(s, state)
    val limitedJIS = wrapLimitingStream(jis, state)
    val decodedInputStream = wrapLayerDecoder(limitedJIS)

    val newDIS = InputSourceDataInputStream(decodedInputStream)
    newDIS.cst.setPriorBitOrder(BitOrder.MostSignificantBitFirst) // must initialize priorBitOrder
    newDIS.setDebugging(s.areDebugging)
    newDIS
  }

  def removeLayer(s: InputSourceDataInputStream): Unit = {
    // nothing for now
  }

  def addLayer(s: DataOutputStream, state: UState): DataOutputStream = {
    val jos = wrapJavaOutputStream(s, state)
    val limitedJOS = wrapLimitingStream(jos, state)
    val encodedOutputStream = wrapLayerEncoder(limitedJOS)
    val newDOS = DirectOrBufferedDataOutputStream(
      encodedOutputStream,
      null,
      isLayer = true,
      s.chunkSizeInBytes,
      s.maxBufferSizeInBytes,
      s.tempDirPath)
    newDOS.setPriorBitOrder(BitOrder.MostSignificantBitFirst)
    newDOS.setAbsStartingBitPos0b(ULong(0L))
    newDOS.setDebugging(s.areDebugging)
    newDOS
  }

  def removeLayer(s: DirectOrBufferedDataOutputStream, state: UState): Unit = {
    //
    // Because the layer may have suspensions that will write to it long after
    // this unparser has left the stack, it is not clear we can do any
    // cleanup of resources here.
    //
  }

  // These were very useful for debugging. Note that they stop with a ???
  // error. That's because dumping streams changes their state.
  //
  // Keeping these around commented out, for now. While this feature is still
  // new and may need further debugging.
  //
  //  def dumpLayer(is: InputStream) {
  //    val str = Stream.continually(is.read).takeWhile(_ != -1).map(_.toChar).mkString
  //    println("dump length " + str.length + " = " + str)
  //    ???
  //  }
  //
  //  def hexDumpLayer(is: InputStream) {
  //    val str = hexify(is)
  //    println("hex dump = " + str)
  //    ???
  //  }
  //
  //  def hexify(is: InputStream): String =
  //    Stream.continually(is.read).takeWhile(_ != -1).map(x => "%02x".format(x.toInt)).mkString(" ")
  //
  //  def hexDumpLayer(is: InputSourceDataInputStream, finfo: FormatInfo) {
  //    val jis = new JavaIOInputStream(is, finfo)
  //    val str = hexify(jis)
  //    println("DIS hex dump = " + str)
  //    ???
  //  }
}

class JavaIOInputStream(s: InputSourceDataInputStream, finfo: FormatInfo)
  extends java.io.InputStream {

  private lazy val id = Misc.getNameFromClass(this)

  override def read(): Int = {
    if (!s.isDefinedForLength(8)) -1
    else {
      val ul = s.getUnsignedLong(8, finfo)
      val byte: Int = ul.toInt & 0xFF
      byte
    }
  }

  override def available(): Int = 1

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

class JavaIOOutputStream(dos: DataOutputStream, finfo: FormatInfo)
  extends java.io.OutputStream {

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
