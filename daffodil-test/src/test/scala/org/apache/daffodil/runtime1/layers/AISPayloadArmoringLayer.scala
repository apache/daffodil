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
import scala.util.Using

import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.io.processors.charset.BitsCharsetDefinition
import org.apache.daffodil.io.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSize
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.runtime1.layers.api.Layer

import org.apache.commons.io.IOUtils

final class AISPayloadArmoringLayer
  extends Layer("aisPayloadArmor", "urn:org.apache.daffodil.layers.ais") {

  /**
   * Decoding AIS payload armoring is encoding the ASCII text into the
   * underlying binary data.
   */
  override def wrapLayerInput(jis: InputStream): InputStream =
    new AISPayloadArmoringInputStream(jis)

  override def wrapLayerOutput(jos: OutputStream): OutputStream =
    new AISPayloadArmoringOutputStream(jos)
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

  object FormatInfoForAISDecode extends FormatInfo {
    // Q: do byteOrder and bitOrder even get used?
    // A: yes, by decode verifying the consistency of the BitsCharset with this
    //    as the pre-existing context.
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
      Using.resource(InputSourceDataInputStream(ba)) { dis =>
        val finfo = FormatInfoForAISDecode
        val cb = CharBuffer.allocate(256)
        //
        // TODO: This test/example layer is depending on an internal Daffodil class
        //  (BitsCharset) and so is not using a supportable API, which makes it a bad example of
        //  how to write a layer.
        //  We do want to reuse the bitsCharset features of daffodil
        //  for this non-byte-sized charset decoding. But this finfo object (a trait on the ParseOrUnparseState)
        //  should not be part of any such API.
        //
        while ({
          val numDecoded = dec.decode(dis, finfo, cb); numDecoded > 0
        }) {
          cb.flip()
          IOUtils.write(cb, jos, StandardCharsets.ISO_8859_1)
          cb.clear()
        }
        jos.close() // closes jos, not the ISDIS which gets auto-closed.
        closed = true
      }
    }
  }

  override def write(bInt: Int): Unit = {
    baos.write(bInt)
  }

}

/**
 * Special purpose. This is not used for decoding anything.
 * The encoder is used to convert strings using the characters
 * allowed, into binary data using the AIS Payload Armoring
 * described here:
 *
 * http://catb.org/gpsd/AIVDM.html#_aivdm_aivdo_payload_armoring
 *
 * To convert a string of length N bytes, You will get 6N bits.
 *
 * The decoder can be used for unit testing, but the point of this class
 * is to make the encoder available for use in un-doing the AIS Payload
 * armoring when parsing, and performing this armoring when unparsing.
 *
 * When encoding from 8-bit say, ascii, or iso-8859-1, this can only encode
 * things that stay within the 64 allowed characters.
 * dfdl:encodingErrorPolicy='error' would check this (once implemented), otherwise
 * where this is used the checking needs to be done separately somehow.
 */
object BitsCharsetAISPayloadArmoring extends BitsCharsetNonByteSize {
  override lazy val name = "X-DAFFODIL-AIS-PAYLOAD-ARMORING"
  override lazy val bitWidthOfACodeUnit = 6
  override lazy val decodeString =
    """0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVW'abcdefghijklmnopqrstuvw"""
  override lazy val replacementCharCode = 0x30
  override lazy val requiredBitOrder = BitOrder.MostSignificantBitFirst
}

final class BitsCharsetAISPayloadArmoringDefinition
  extends BitsCharsetDefinition(BitsCharsetAISPayloadArmoring)
