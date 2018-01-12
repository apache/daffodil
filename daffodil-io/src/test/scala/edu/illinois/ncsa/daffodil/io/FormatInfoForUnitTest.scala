package edu.illinois.ncsa.daffodil.io

import java.nio.charset.CodingErrorAction
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.processors.charset.BitsCharset
import edu.illinois.ncsa.daffodil.processors.charset.BitsCharsetDecoder
import edu.illinois.ncsa.daffodil.processors.charset.BitsCharsetEncoder
import edu.illinois.ncsa.daffodil.processors.charset.StandardBitsCharsets
import edu.illinois.ncsa.daffodil.processors.charset.NBitsWidth_BitsCharset

object FormatInfoForUnitTest {
  def apply() = {
    val obj = new FormatInfoForUnitTest()
    obj.init()
    obj
  }
}

class FormatInfoForUnitTest private ()
  extends FormatInfo {
  var priorEncoding: BitsCharset = StandardBitsCharsets.UTF_8

  var encoder: BitsCharsetEncoder = priorEncoding.newEncoder()
  var decoder: BitsCharsetDecoder = priorEncoding.newDecoder()
  var reportingDecoder: BitsCharsetDecoder = _

  var replacingDecoder: BitsCharsetDecoder = _

  var byteOrder: ByteOrder = ByteOrder.BigEndian
  var bitOrder: BitOrder = BitOrder.MostSignificantBitFirst
  var fillByte: Byte = 0x00.toByte
  var binaryFloatRep: BinaryFloatRep = BinaryFloatRep.Ieee
  var maybeCharWidthInBits: MaybeInt = MaybeInt.Nope
  var maybeUTF16Width: Maybe[UTF16Width] = Maybe.Nope
  var encodingMandatoryAlignmentInBits: Int = 8
  var encodingErrorPolicy: EncodingErrorPolicy = EncodingErrorPolicy.Replace

  def reset(cs: BitsCharset): Unit = {
    priorEncoding = cs
    init()
  }

  def init(): Unit = {
    encoder = priorEncoding.newEncoder()
    encoder.onMalformedInput(CodingErrorAction.REPLACE)
    encoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
    decoder = priorEncoding.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPLACE)
    decoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
    reportingDecoder = {
      val d = priorEncoding.newDecoder()
      d.onMalformedInput(CodingErrorAction.REPORT)
      d.onUnmappableCharacter(CodingErrorAction.REPORT)
      d
    }
    replacingDecoder = {
      val d = priorEncoding.newDecoder()
      d.onMalformedInput(CodingErrorAction.REPLACE)
      d.onUnmappableCharacter(CodingErrorAction.REPLACE)
      d
    }
    priorEncoding match {
      case decoderWithBits: NBitsWidth_BitsCharset => {
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
