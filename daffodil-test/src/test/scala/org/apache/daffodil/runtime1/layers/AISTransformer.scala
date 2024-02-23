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

import java.io._
import java.nio._
import java.nio.charset._
import java.util.Optional
import scala.collection.JavaConverters._

import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.processors.charset.BitsCharsetAISPayloadArmoring
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.io.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.layers.runtime1.BoundaryMarkRegexLimiter
import org.apache.daffodil.lib.api.DaffodilTunables
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.runtime1.layers.api.JLayerLengthKind
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerLimiter
import org.apache.daffodil.runtime1.layers.api.LayerPropertyInfo
import org.apache.daffodil.runtime1.layers.api.LayerRuntime

import org.apache.commons.io.IOUtils

final class AISPayloadArmoringLayer
  extends Layer(
    layerName = "aisPayloadArmor",
    supportedLayerLengthKinds = Seq(JLayerLengthKind.BoundaryMark).asJava,
    supportedLayerLengthUnits = Seq().asJava,
    isRequiredLayerEncoding = true,
    optLayerVariables = Optional.empty(),
  ) {

  /**
   * Decoding AIS payload armoring is encoding the ASCII text into the
   * underlying binary data.
   */
  override def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream =
    new AISPayloadArmoringInputStream(jis)

  override def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream =
    new AISPayloadArmoringOutputStream(jos)

  override def layerLimiter(layerPropertyInfo: LayerPropertyInfo): Optional[LayerLimiter] =
    Optional.of(new BoundaryMarkRegexLimiter {
      override protected def regexForBoundaryMarkMatch: String = ","

      override protected def boundaryMarkToInsert: String = ","

      override protected def maximumLengthBoundaryMark: String = ","
    })
}

class AISPayloadArmoringInputStream(jis: InputStream) extends InputStream {

  private lazy val enc = BitsCharsetAISPayloadArmoring.newEncoder()

  private lazy val bais = {
    val armoredText = IOUtils.toString(jis, StandardCharsets.ISO_8859_1)
    val cb = CharBuffer.wrap(armoredText)

    val numBytes = (6 * armoredText.length) / 8
    val ba = new Array[Byte](numBytes + 1)
    val bb = ByteBuffer.wrap(ba)
    val cr = enc.encode(cb, bb, endOfInput = true)
    //
    // We made bb have one extra byte in it.
    // So this should end on UNDERFLOW meaning it could take more characters.
    //
    Assert.invariant(cr.isUnderflow)

    val bais = new ByteArrayInputStream(ba, 0, numBytes)
    bais
  }

  override def read(): Int = {
    bais.read()
  }
}

class AISPayloadArmoringOutputStream(jos: java.io.OutputStream) extends OutputStream {

  private lazy val dec = BitsCharsetAISPayloadArmoring.newDecoder()

  private val baos = new ByteArrayOutputStream()

  private var closed = false

  class FormatInfoForAISDecode extends FormatInfo {
    // TODO: do byteOrder and bitOrder even get used?
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
      val ba = baos.toByteArray
      val dis = InputSourceDataInputStream(ba)
      val finfo = new FormatInfoForAISDecode()
      val cb = CharBuffer.allocate(256)
      while ({ val numDecoded = dec.decode(dis, finfo, cb); numDecoded > 0 }) {
        cb.flip()
        IOUtils.write(cb, jos, StandardCharsets.ISO_8859_1)
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
