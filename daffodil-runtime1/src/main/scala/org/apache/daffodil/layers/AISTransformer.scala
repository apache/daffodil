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

import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeInt
import org.apache.daffodil.processors.LayerLengthInBytesEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.charset.BitsCharsetAISPayloadArmoring
import java.nio._
import java.nio.charset._
import java.io._
import org.apache.commons.io.IOUtils
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.io.BoundaryMarkLimitingStream
import org.apache.daffodil.io.LayerBoundaryMarkInsertingJavaOutputStream
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.dsom.DPathCompileInfo

object AISPayloadArmoringTransformer {
  val iso8859 = StandardCharsets.ISO_8859_1
}

class AISPayloadArmoringTransformer()
  extends LayerTransformer() {
  import AISPayloadArmoringTransformer._

  /**
   * Decoding AIS payload armoring is encoding the ASCII text into the
   * underlying binary data.
   */
  override def wrapLayerDecoder(jis: java.io.InputStream) = {
    new AISPayloadArmoringInputStream(jis)
  }

  override def wrapLimitingStream(jis: java.io.InputStream, state: PState) = {
    val layerBoundaryMark = ","
    val s = BoundaryMarkLimitingStream(jis, layerBoundaryMark, iso8859)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    new AISPayloadArmoringOutputStream(jos)
  }

  override protected def wrapLimitingStream(jos: java.io.OutputStream, state: UState): java.io.OutputStream = {
    val layerBoundaryMark = ","
    val newJOS = new LayerBoundaryMarkInsertingJavaOutputStream(jos, layerBoundaryMark, iso8859)
    newJOS
  }
}

class AISPayloadArmoringInputStream(jis: InputStream)
  extends InputStream {
  import AISPayloadArmoringTransformer._

  private lazy val enc = BitsCharsetAISPayloadArmoring.newEncoder()

  private lazy val bais = {
    val armoredText = IOUtils.toString(jis, iso8859)
    val cb = CharBuffer.wrap(armoredText)

    val numBytes = (6 * armoredText.length) / 8
    val ba = new Array[Byte](numBytes + 1)
    val bb = ByteBuffer.wrap(ba)
    val cr = enc.encode(cb, bb, true)
    //
    // We made bb have one extra byte in it.
    // So this should end on UNDERFLOW meaning it could take more characters.
    //
    Assert.invariant(cr.isUnderflow())

    val bais = new ByteArrayInputStream(ba, 0, numBytes)
    bais
  }

  override def read(): Int = {
    bais.read()
  }
}

class AISPayloadArmoringOutputStream(jos: java.io.OutputStream)
  extends OutputStream {
  import AISPayloadArmoringTransformer._

  private lazy val dec = BitsCharsetAISPayloadArmoring.newDecoder()

  private val baos = new ByteArrayOutputStream()

  private var closed = false

  class FormatInfoForAISDecode extends FormatInfo {
    override def byteOrder: ByteOrder = ByteOrder.BigEndian
    override def bitOrder: BitOrder = BitOrder.MostSignificantBitFirst
    override def encodingErrorPolicy: EncodingErrorPolicy = EncodingErrorPolicy.Replace

    private def doNotUse = Assert.usageError("Should not be used")
    override def encoder: BitsCharsetEncoder = doNotUse
    override def decoder: BitsCharsetDecoder = doNotUse
    override def fillByte: Byte = doNotUse
    override def binaryFloatRep: BinaryFloatRep = doNotUse
    override def maybeCharWidthInBits: MaybeInt = doNotUse
    override def maybeUTF16Width: Maybe[UTF16Width] = doNotUse
    override def encodingMandatoryAlignmentInBits: Int = doNotUse
    override def tunable: DaffodilTunables = doNotUse
    override def regexMatchBuffer: CharBuffer = doNotUse
    override def regexMatchBitPositionBuffer: LongBuffer = doNotUse
  }

  override def close(): Unit = {
    if (!closed) {
      val ba = baos.toByteArray()
      val dis = InputSourceDataInputStream(ba)
      val finfo = new FormatInfoForAISDecode()
      val cb = CharBuffer.allocate(256)
      while ({ val numDecoded = dec.decode(dis, finfo, cb); numDecoded > 0 }) {
        cb.flip()
        IOUtils.write(cb, jos, iso8859)
        cb.clear()
      }
      jos.close()
      closed = true
    }
  }

  override def write(bInt: Int): Unit = {
    baos.write(bInt)
  }

}

object AISPayloadArmoringTransformerFactory
  extends LayerTransformerFactory("aisPayloadArmor") {

  override def newInstance(
    maybeLayerCharsetEv: Maybe[LayerCharsetEv],
    maybeLayerLengthKind: Maybe[LayerLengthKind],
    maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
    maybeLayerLengthUnits: Maybe[LayerLengthUnits],
    maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
    tci: DPathCompileInfo): LayerTransformer = {

    val xformer = new AISPayloadArmoringTransformer()
    xformer
  }
}
