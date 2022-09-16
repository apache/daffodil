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

import org.apache.commons.io.IOUtils
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.ThinDiagnostic
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.processors.LayerLengthEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.DataInputStream.Mark
import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.infoset.DataValue
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.processors.Evaluatable
import passera.unsigned.ULong
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.SuspendableOperation
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.processors.charset.BitsCharsetJava
import org.apache.daffodil.util.ByteBufferOutputStream

import java.io.ByteArrayInputStream
import java.io.EOFException
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.charset.Charset

/**
 * Shared functionality of all LayerTransformers.
 *
 * A layer transformer is created at runtime as part of a single parse/unparse call.
 * Hence, they can be stateful without causing thread-safety issues.
 */
abstract class LayerTransformer(layerName: String, layerRuntimeInfo: LayerRuntimeInfo) {

  protected def wrapLayerDecoder(jis: InputStream): InputStream

  protected def wrapLimitingStream(state: ParseOrUnparseState, jis: InputStream): InputStream

  final def wrapJavaInputStream(s: InputSourceDataInputStream, fInfo: FormatInfo): InputStream = {
    new JavaIOInputStream(s, fInfo)
  }

  protected def wrapLayerEncoder(jos: OutputStream): OutputStream

  protected def wrapLimitingStream(state: ParseOrUnparseState, jis: OutputStream): OutputStream

  final def wrapJavaOutputStream(s: DataOutputStream, fInfo: FormatInfo): OutputStream = {
    new JavaIOOutputStream(s, fInfo)
  }
  /**
   *  Override these if we ever have transformers that don't have these
   *  requirements.
   */
  val mandatoryLayerAlignmentInBits: Int = 8
  val mandatoryLengthUnit: LayerLengthUnits = LayerLengthUnits.Bytes

  final def addLayer(s: InputSourceDataInputStream, state: PState): InputSourceDataInputStream = {
    val jis = wrapJavaInputStream(s, state)
    val limitedJIS = wrapLimitingStream(state, jis)
    val decodedInputStream = wrapLayerDecoder(limitedJIS)

    val newDIS = InputSourceDataInputStream(decodedInputStream)
    newDIS.cst.setPriorBitOrder(BitOrder.MostSignificantBitFirst) // must initialize priorBitOrder
    newDIS.setDebugging(s.areDebugging)
    newDIS
  }

  final def removeLayer(s: InputSourceDataInputStream): Unit = {
    // nothing for now
  }

  final def addLayer(s: DataOutputStream, state: UState, finfo: FormatInfo): DirectOrBufferedDataOutputStream = {
    val jos = wrapJavaOutputStream(s, finfo)
    val limitedJOS = wrapLimitingStream(state, jos)
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

  final def removeLayer(s: DirectOrBufferedDataOutputStream, state: UState): Unit = {
    //
    // Because the layer may have suspensions that will write to it long after
    // this unparser has left the stack, it is not clear we can do any
    // cleanup of resources here.
    //
  }

  /**
   * Enables the layer transformer to take necessary state-maniuplating actions before
   * the parsing of a layer starts.
   *
   * Examples might be setting variable values.
   *
   * At the time this is called, the state contains the layered I/O stream already.
   * @param s The parser state, which may be modified.
   */
  def startLayerForParse(s: PState):Unit = () // nothing by default


  /**
   * Enables the layer transformer to take necessary state-maniuplating actions after
   * the unparsing of a layer ends.
   *
   * Note that if variables without values are read by this method, then this must create
   * suspended calculations that clone the necessary state, which are evaluated later.
   *
   * Examples might be setting variable values.
   * @param s The unparser state, which may be modified.
   */
  def endLayerForUnparse(s: UState): Unit = () // nothing by default
}

class JavaIOInputStream(s: InputSourceDataInputStream, finfo: FormatInfo)
  extends InputStream {

  private lazy val id = Misc.getNameFromClass(this)

  override def read(): Int = {
    if (!s.isDefinedForLength(8)) -1
    else {
      val ul = s.getUnsignedLong(8, finfo)
      val byte: Int = ul.toInt & 0xFF
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

class JavaIOOutputStream(dos: DataOutputStream, finfo: FormatInfo)
  extends OutputStream {

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

abstract class LayerException(
  val schemaFileLocation: SchemaFileLocation,
  dataLocation: DataLocation,
  maybeCause: Maybe[Throwable],
  maybeFormatString: Maybe[String],
  args: Any*)
extends ThinDiagnostic(One(schemaFileLocation), One(dataLocation), maybeCause, maybeFormatString, args: _*)

case class LayerNotEnoughDataException(sfl: SchemaFileLocation, dataLocation: DataLocation, cause: Throwable, nBytesRequired: Int)
  extends LayerException(sfl, dataLocation, One(cause), None, nBytesRequired) {
  override def isError = true
  override def modeName = "Parse"
  }

/**
 * Allows access to all the layer properties, if defined, including
 * evaluating expressions if the properties values are defined as expressions.
 */
final class LayerRuntimeInfo(
  srd: SequenceRuntimeData,
  val maybeLayerCharsetEv: Maybe[LayerCharsetEv],
  val maybeLayerLengthKind: Maybe[LayerLengthKind],
  val maybeLayerLengthEv: Maybe[LayerLengthEv],
  val maybeLayerLengthUnits: Maybe[LayerLengthUnits],
  val maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv])
  extends Serializable
{

  def evaluatables: Seq[Evaluatable[AnyRef]] =
    maybeLayerCharsetEv.toScalaOption.toSeq ++
      maybeLayerLengthEv.toScalaOption.toSeq ++
      maybeLayerBoundaryMarkEv.toScalaOption.toSeq

  def runtimeData: SequenceRuntimeData = srd

  def optLayerCharset(state: ParseOrUnparseState): Option[Charset] = maybeLayerCharsetEv.toScalaOption.map {
    _.evaluate(state)
  } match {
    case Some(bitsCharsetJava: BitsCharsetJava) => Some(bitsCharsetJava.javaCharset)
    case None => None
    case Some(other) => None // only java charsets are supported.
  }

  def optLayerLengthKind: Option[LayerLengthKind] = maybeLayerLengthKind.toScalaOption

  def optLayerLength(state: ParseOrUnparseState): Option[Long] = maybeLayerLengthEv.toScalaOption.map{ _.evaluate(state) }

  def optLayerLengthUnits: Option[LayerLengthUnits] = maybeLayerLengthUnits.toScalaOption

  def optLayerBoundaryMark(state: ParseOrUnparseState): Option[String] = maybeLayerBoundaryMarkEv.toScalaOption.map{ _.evaluate(state) }

  def schemaFileLocation = srd.schemaFileLocation
}


abstract class ByteBufferExplicitLengthLayerTransform[T](
  layerRuntimeInfo: LayerRuntimeInfo,
  layerName: String,
  inputVars: Seq[VariableRuntimeData],
  outputVar: VariableRuntimeData)
  extends LayerTransformer(layerName, layerRuntimeInfo) {

  /**
   * Override to specify the length exactly.
   */
  protected def layerBuiltInConstantLength: Option[Long]

  private var explicitLengthInBytes_ : Long = _

  final protected def explicitLengthInBytes = explicitLengthInBytes_

  /**
   * The header data will be captured here. All the limiting streams and
   * the byte/short buffers are all aliases into this same object.
   */
  private var byteArr: Array[Byte] = _

  /**
   * ByteBuffer view of the header bytes.
   */
  private var byteBuf : ByteBuffer = _

  private var limitingOutputStream: ByteBufferOutputStream = _

  protected def compute(s: ParseOrUnparseState, isUnparse: Boolean, inputs: Seq[Any], byteBuffer:ByteBuffer): T

  /**
   * Assigned by wrapLimitingStream for parsing to capture the original source
   * of the header bytes for parsing.
   */
  private var optOriginalInputStream: Maybe[InputStream] = Maybe.Nope

  /**
   * Assigned by wrapLimitingStream for unparsing to capture the original
   * output stream to which the bytes are ultimately written.
   */
  private var optOriginalOutputStream: Maybe[OutputStream] = Maybe.Nope

  private var optLayerCharset_ : Option[Charset] = _

  protected def optLayerCharset = {
    Assert.invariant(optLayerCharset_ ne null)
    optLayerCharset_
  }

  protected def wrapLayerDecoder(jis: InputStream) = jis

  private def setup(state: ParseOrUnparseState, layerRuntimeInfo: LayerRuntimeInfo): Unit = {
    val olc = layerRuntimeInfo.optLayerCharset(state)
    optLayerCharset_ = olc
    explicitLengthInBytes_  =
      if (layerBuiltInConstantLength.isDefined) layerBuiltInConstantLength.get
      else layerRuntimeInfo.optLayerLength(state).getOrElse {
        state.SDE("The layer does not have a built in length and the dfdl:layerLengthKind is 'explicit' yet no dfdlx:layerLength was provided.")
      }
    byteArr = new Array[Byte](explicitLengthInBytes_ .toInt)
    byteBuf = ByteBuffer.wrap(byteArr)
  }

  protected def wrapLimitingStream(state: ParseOrUnparseState, jis: InputStream) = {
    setup(state, layerRuntimeInfo)
    optOriginalInputStream = One(jis)
    val limitingInputStream = new ByteArrayInputStream(byteArr)
    limitingInputStream
  }

  protected def wrapLayerEncoder(jos: OutputStream) = jos

  protected def wrapLimitingStream(state: ParseOrUnparseState, jos: OutputStream) = {
    setup(state, layerRuntimeInfo)
    optOriginalOutputStream = One(jos)
    limitingOutputStream = new ByteBufferOutputStream(byteBuf)
    limitingOutputStream
  }

  final override def startLayerForParse(s: PState): Unit = {
    //
    // For parsing, all the hard work happens here, allowing the layered input stream to
    // just deliver the bytes
    //
    Assert.invariant(optOriginalInputStream.isDefined)

    try
      IOUtils.readFully(optOriginalInputStream.get, byteArr)
    catch {
      case eof: EOFException =>
        throw new LayerNotEnoughDataException(layerRuntimeInfo.schemaFileLocation, s.currentLocation, eof, explicitLengthInBytes_ .toInt)
    }
    val inputs = inputVars.map{ inputVar => s.getVariable(inputVar, layerRuntimeInfo.runtimeData).getAnyRef }
    val checksum: T = compute(s, isUnparse = false, inputs, byteBuf)
    s.setVariable(outputVar, DataValue.unsafeFromAnyRef(checksum.asInstanceOf[AnyRef]), layerRuntimeInfo.runtimeData) // assign to result variable.
  }

  final class SuspendableChecksumLayerOperation()
    extends SuspendableOperation {
    override def rd = layerRuntimeInfo.runtimeData

    /**
     * Test succeeds if all the data required has been written to the layer
     */
    protected def test(ustate: UState) = {
      //
      // Note: there is no unparse equivalent of not-enough-data error, because the unparser
      // will just deadlock here waiting for more data.
      //
      limitingOutputStream.size() == explicitLengthInBytes_
    }

    /**
     * Computes checksum and overwrites that part of the layer data
     * with the new checksum, writes the layer data out,
     * and assigns the output variable.
     */
    protected def continuation(ustate: UState): Unit = {
      Assert.invariant(optOriginalOutputStream.isDefined)
      byteBuf.position(0).limit(byteBuf.capacity())
      val inputs = inputVars.map{ inputVRD => ustate.getVariable(inputVRD, layerRuntimeInfo.runtimeData).getAnyRef }
      val finalChecksum = compute(ustate, isUnparse = true, inputs, byteBuf)
      ustate.setVariable(outputVar,
        DataValue.unsafeFromAnyRef(finalChecksum.asInstanceOf[AnyRef]),
        layerRuntimeInfo.runtimeData) // assign to the result variable.
      // write out the layer data (which has recomputed checksum in it.
      optOriginalOutputStream.get.write(byteArr)
      optOriginalOutputStream.get.close()
    }
  }

  private lazy val suspendableOperation = new SuspendableChecksumLayerOperation()

  final override def endLayerForUnparse(s: UState): Unit = {
    suspendableOperation.run(s)
  }

}
