package edu.illinois.ncsa.daffodil.io

import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction
import java.nio.charset.CharsetEncoder
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy

object FormatInfoForUnitTest {
  def apply() = {
    val obj = new FormatInfoForUnitTest()
    obj.init()
    obj
  }
}

class FormatInfoForUnitTest private ()
  extends FormatInfo {
  var priorEncoding: Charset = java.nio.charset.StandardCharsets.UTF_8

  var encoder: CharsetEncoder = priorEncoding.newEncoder()
  var decoder: CharsetDecoder = priorEncoding.newDecoder()
  var reportingDecoder: CharsetDecoder = _

  var replacingDecoder: CharsetDecoder = _

  var byteOrder: ByteOrder = ByteOrder.BigEndian
  var bitOrder: BitOrder = BitOrder.MostSignificantBitFirst
  var fillByte: Byte = 0x00.toByte
  var binaryFloatRep: BinaryFloatRep = BinaryFloatRep.Ieee
  var maybeCharWidthInBits: MaybeInt = MaybeInt.Nope
  var maybeUTF16Width: Maybe[UTF16Width] = Maybe.Nope
  var encodingMandatoryAlignmentInBits: Int = 8
  var encodingErrorPolicy: EncodingErrorPolicy = EncodingErrorPolicy.Replace

  def reset(cs: Charset): Unit = {
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
      case decoderWithBits: NonByteSizeCharset => {
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
