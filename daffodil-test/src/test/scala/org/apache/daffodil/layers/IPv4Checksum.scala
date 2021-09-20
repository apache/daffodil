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
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.LayerLengthEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState

import java.io._
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.SuspendableOperation
import org.apache.daffodil.util.ByteBufferOutputStream
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.RefQName
import passera.unsigned.UShort

import java.nio.ByteBuffer
import java.nio.ShortBuffer

/**
 *  The layer transform computes the checksum of the header data.
 *  per IETF RFC 791.
 *
 *  The data has a well-known fixed length, so layerLengthKind is always 'implicit'.
 */
final class IPv4Checksum(srd: SequenceRuntimeData)
extends LayerTransformer() {

  private def hdrLengthInBytes = 20

  private def chksumShortIndex = 5

  private val finalChecksumVRD = {
    val varNamespace = NS("urn:org.apache.daffodil.layers.IPv4Checksum")
    val finalChecksumVarQName = RefQName(Some("chksum"), "IPv4Checksum", varNamespace).toGlobalQName
    val vrd = srd.variableMap.getVariableRuntimeData(finalChecksumVarQName).getOrElse {
      srd.SDE("Variable '%s' is not defined.", finalChecksumVarQName.toExtendedSyntax)
    }
    srd.schemaDefinitionUnless(vrd.primType == PrimType.UnsignedShort,
      "Variable '%s' is not of type 'xs:unsignedShort'.", finalChecksumVarQName.toExtendedSyntax)
    vrd
  }

  /**
   * The header data will be captured here. All the limiting streams and
   * the byte/short buffers are all aliases into this same object.
   */
  private val hdrByteArr = new Array[Byte](hdrLengthInBytes)

  /**
   * Stream from which the header will be parsed.
   */
  private val limitingInputStream = new ByteArrayInputStream(hdrByteArr)


  /**
   * ByteBuffer view of the header bytes.
   */
  private val byteBuf = ByteBuffer.wrap(hdrByteArr) // bigEndian byte order by default
  private val shortBuf = byteBuf.asShortBuffer()

  /**
   * Stream to which the header will be unparsed.
   */
  private val limitingOutputStream = new ByteBufferOutputStream(byteBuf)

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

  protected def wrapLayerDecoder(jis: InputStream) = jis

  protected def wrapLimitingStream(jis: InputStream, state: PState) = {
    optOriginalInputStream = One(jis)
    limitingInputStream
  }

  protected def wrapLayerEncoder(jos: OutputStream) = jos

  protected def wrapLimitingStream(jos: OutputStream, state: UState) = {
    optOriginalOutputStream = One(jos)
    limitingOutputStream
  }

  /**
   * Shared by both parsing and unparsing. Contains the checksum algorithm.
   */
  private def computeChecksum(shortBuf: ShortBuffer): Int = {
    var i = 0
    var chksum: Int = 0
    val nShorts = hdrLengthInBytes / 2
    while (i < nShorts) {
      if (i == chksumShortIndex) {
        // for the checksum calculation treat the incoming checksum field of the data as 0
        // so we just don't do an addition here.
      } else {
        chksum += UShort(shortBuf.get(i)).toInt
      }
      Assert.invariant(chksum >= 0)
      i += 1
    }
    //
    // now combine the carry bits in the most significant 16 bits into the lower 16 bits.
    //
    val checksumLow = chksum & 0xFFFF
    val checksumHigh = chksum >>> 16
    val checksumTotal: Int = checksumLow + checksumHigh
    Assert.invariant(checksumTotal <= 0xFFFF && checksumTotal >= 0)
    val checksumTotalShort = UShort(checksumTotal.toShort)
    val checksum = checksumTotalShort.toInt
    //
    // take ones complement to get the final checksum
    //
    val finalChecksum: Int = (~checksum) & 0xFFFF
    finalChecksum
  }

  override def startLayerForParse(s: PState): Unit = {
    //
    // For parsing, all the hard work happens here, allowing the layered input stream to
    // just deliver the bytes
    //
    Assert.invariant(s.bitPos0b % 8 == 0) // we are byte aligned.
    Assert.invariant(optOriginalInputStream.isDefined)

    try
      IOUtils.readFully(optOriginalInputStream.get, hdrByteArr)
    catch {
      case eof: EOFException =>
        throw new LayerNotEnoughDataException(srd, s, eof, hdrLengthInBytes)
    }
    val checksumInDataStream = shortBuf.get()
    val checksum = computeChecksum(shortBuf)
    s.setVariable(finalChecksumVRD, checksum, srd) // assign to result variable.
  }

  final class SuspendableChecksumLayerOperation()
    extends SuspendableOperation {
    override def rd = srd

    /**
     * Test succeeds if all the data required has been written to the layer
     */
    protected def test(ustate: UState) = {
      //
      // Note: there is no unparse equivalent of not-enough-data error, because the unparser
      // will just deadlock here waiting for more data.
      //
      limitingOutputStream.size() == hdrLengthInBytes
    }

    /**
     * Computes checksum and overwrites that part of the layer data
     * with the new checksum, writes the layer data out,
     * and assigns the output variable.
     */
    protected def continuation(ustate: UState): Unit = {
      Assert.invariant(optOriginalOutputStream.isDefined)
      val finalChecksum = computeChecksum(shortBuf)
      ustate.setVariable(finalChecksumVRD, finalChecksum, srd) // assign to the result variable.
      //
      // clobber the byte buffer bytes corresponding to the checksum with
      // the recomputed value
      //
      shortBuf.put(chksumShortIndex, finalChecksum.toShort)
      //
      // write out the layer data (which has recomputed checksum in it.
      optOriginalOutputStream.get.write(hdrByteArr)
      optOriginalOutputStream.get.close()
    }
  }

  private lazy val suspendableOperation = new SuspendableChecksumLayerOperation()

  override def endLayerForUnparse(s: UState): Unit = {
    suspendableOperation.run(s)
  }

}

  object IPv4Checksum
    extends LayerTransformerFactory("IPv4Checksum") {

    override def newInstance(
      maybeLayerCharsetEv: Maybe[LayerCharsetEv],
      maybeLayerLengthKind: Maybe[LayerLengthKind],
      maybeLayerLengthEv: Maybe[LayerLengthEv],
      maybeLayerLengthUnits: Maybe[LayerLengthUnits],
      maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
      srd: SequenceRuntimeData) = {
      srd.schemaDefinitionUnless(maybeLayerLengthKind.isEmpty ||
        maybeLayerLengthKind.get == LayerLengthKind.Implicit,
        "Must have dfdlx:layerLengthKind undefined or 'implicit', but was '%s'.",
        maybeLayerLengthKind.get.toString)
      //
      // ToDo: We can't issue SDW because we don't have a context object
      // for that sort of compilation time warning. We *should* have that.
      // That requires a layer factory method called at schema compile time with the
      // Sequence term (not just term runtime data) as an argument.
      //
      // We would like to warn if maybeLayerLengthUnits is defined here and is not bytes
      // We would like to warn if maybeLayerBoundaryMarkEv is defined (since it is unused by this layer)
      // We would like to warn if maybeLayerCharsetEv is defined (since it is unused by this layer)
      // We would like to warn if maybeLayerLengthEv is defined (since it is unused by this layer)
      //
      val xformer = new IPv4Checksum(srd)
      xformer
    }
  }
