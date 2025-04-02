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

package org.apache.daffodil.io

import java.nio.CharBuffer
import java.nio.LongBuffer
import java.nio.charset.CodingErrorAction

import org.apache.daffodil.io.processors.charset.BitsCharset
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.io.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSize
import org.apache.daffodil.io.processors.charset.StandardBitsCharsets
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeInt

object FormatInfoForUnitTest {
  def apply() = {
    val obj = new FormatInfoForUnitTest()
    obj.init()
    obj
  }
}

class FormatInfoForUnitTest private () extends FormatInfo {
  var priorEncoding: BitsCharset = StandardBitsCharsets.UTF_8

  var encoder: BitsCharsetEncoder = priorEncoding.newEncoder()
  var decoder: BitsCharsetDecoder = priorEncoding.newDecoder()

  var byteOrder: ByteOrder = ByteOrder.BigEndian
  var bitOrder: BitOrder = BitOrder.MostSignificantBitFirst
  var fillByte: Byte = 0x00.toByte
  var binaryFloatRep: BinaryFloatRep = BinaryFloatRep.Ieee
  var maybeCharWidthInBits: MaybeInt = MaybeInt.Nope
  var maybeUTF16Width: Maybe[UTF16Width] = Maybe.Nope
  var encodingMandatoryAlignmentInBits: Int = 8
  var encodingErrorPolicy: EncodingErrorPolicy = EncodingErrorPolicy.Replace
  var tunable: DaffodilTunables = DaffodilTunables()
  var regexMatchBuffer = CharBuffer.allocate(1024)
  var regexMatchBitPositionBuffer = LongBuffer.allocate(1024)

  def reset(cs: BitsCharset): Unit = {
    priorEncoding = cs
    init()
  }

  def init(): Unit = {
    encoder = priorEncoding.newEncoder()
    encoder.onMalformedInput(CodingErrorAction.REPLACE)
    encoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
    decoder = priorEncoding.newDecoder()
    priorEncoding match {
      case decoderWithBits: BitsCharsetNonByteSize => {
        encodingMandatoryAlignmentInBits = 1
        maybeCharWidthInBits = MaybeInt(decoderWithBits.bitWidthOfACodeUnit)
      }
      case _ => {
        encodingMandatoryAlignmentInBits = 8
        val maxBytes = encoder.maxBytesPerChar()
        if (maxBytes == encoder.averageBytesPerChar()) {
          maybeCharWidthInBits = MaybeInt((maxBytes * 8).toInt)
        } else {
          maybeCharWidthInBits = MaybeInt.Nope
        }
      }
    }
  }
}

/**
 * Supplies only bitOrder and byteOrder. Everything else unimplemented.
 * @param bitOrder
 * @param byteOrder
 */
class FakeFormatInfo(val bitOrder: BitOrder, val byteOrder: ByteOrder) extends FormatInfo {
  def encoder: BitsCharsetEncoder = ???
  def decoder: BitsCharsetDecoder = ???
  def reportingDecoder: BitsCharsetDecoder = ???
  def replacingDecoder: BitsCharsetDecoder = ???
  def fillByte: Byte = ???

  def binaryFloatRep: BinaryFloatRep = ???
  def maybeCharWidthInBits: MaybeInt = ???
  def maybeUTF16Width: Maybe[UTF16Width] = ???
  def encodingMandatoryAlignmentInBits: Int = ???
  def encodingErrorPolicy: EncodingErrorPolicy = ???
  def tunable: DaffodilTunables = ???
  def regexMatchBuffer: CharBuffer = ???
  def regexMatchBitPositionBuffer: LongBuffer = ???
}

object FakeFormatInfo_MSBF_BE
  extends FakeFormatInfo(BitOrder.MostSignificantBitFirst, ByteOrder.BigEndian)
object FakeFormatInfo_LSBF_LE
  extends FakeFormatInfo(BitOrder.LeastSignificantBitFirst, ByteOrder.LittleEndian)
