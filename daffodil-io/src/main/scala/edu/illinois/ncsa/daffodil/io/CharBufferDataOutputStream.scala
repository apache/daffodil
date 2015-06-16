package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.nio.charset.CharsetEncoder
import java.nio.charset.StandardCharsets
import java.nio.charset.CodingErrorAction
import passera.unsigned.ULong
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.CoderResult
import java.nio.channels.Channels
import java.io.ByteArrayOutputStream
import org.apache.commons.io.output.TeeOutputStream
import java.nio.CharBuffer

private[io] class CharBufferDataOutputStreamState extends DataOutputStreamStateImplMixin
/**
 * Used to implement unparsing when length is specified in lengthUnits 'characters' when
 * the encoding is variable width (e.g., utf-8)
 */
class CharBufferDataOutputStream
  extends DataOutputStream with DataOutputStreamCharImplMixin {
  import DataStreamCommon._

  private var target: CharBuffer = null

  def setCharBuffer(cb: CharBuffer) {
    target = cb
  }

  protected final val st = new CharBufferDataOutputStreamState
  protected final def cst: DataStreamCommonState = st

  private def notToBeUsed = Assert.usageError("not to be used")

  def putCharBuffer(cb: java.nio.CharBuffer): Long = {
    Assert.usage(target != null)
    val numCharsTransferred = cb.read(target)
    numCharsTransferred
  }

  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean = notToBeUsed
  def putBytes(ba: Array[Byte], byteStartOffset0b: Int, lengthInBytes: Int): Long = notToBeUsed
  def putBytes(ba: Array[Byte]) = notToBeUsed
  def putByteBuffer(bb: java.nio.ByteBuffer): Long = notToBeUsed
  def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  def putLong(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  def skip(nBits: Long): Boolean = notToBeUsed
  def bitLimit0b: Maybe[Long] = notToBeUsed
  def bitPos0b: Long = notToBeUsed
  def setBitPos0b(bitPos0b: Long) = notToBeUsed
  def futureData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  def pastData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  def setBitLimit0b(bitLimit0b: Maybe[Long]): Boolean = notToBeUsed
  private[io] def resetBitLimit0b(saved: Maybe[Long]): Unit = notToBeUsed
  def setByteOrder(byteOrder: ByteOrder): Unit = notToBeUsed
  def byteOrder: ByteOrder = notToBeUsed
  def validateFinalStreamState { /* do nothing */ }
}