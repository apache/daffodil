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
import edu.illinois.ncsa.daffodil.util.MaybeULong

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

  override def putCharBuffer(cb: java.nio.CharBuffer): Long = {
    Assert.usage(target != null)
    val numCharsTransferred = cb.read(target)
    numCharsTransferred
  }

  override def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean = notToBeUsed
  // override def putBytes(ba: Array[Byte], byteStartOffset0b: Int, lengthInBytes: Int): Long = notToBeUsed
  override def putBytes(ba: Array[Byte]) = notToBeUsed
  override def putByteBuffer(bb: java.nio.ByteBuffer): Long = notToBeUsed
  override def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  override def putLong(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  override def skip(nBits: Long): Boolean = notToBeUsed
  override def bitLimit0b: MaybeULong = notToBeUsed
  override def bitPos0b: Long = notToBeUsed
  // override def setBitPos0b(bitPos0b: Long) = notToBeUsed
  override def futureData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  override def pastData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  override def setBitLimit0b(bitLimit0b: MaybeULong): Boolean = notToBeUsed
  private[io] override def resetBitLimit0b(saved: MaybeULong): Unit = notToBeUsed
  override def setByteOrder(byteOrder: ByteOrder): Unit = notToBeUsed
  override def byteOrder: ByteOrder = notToBeUsed
  override def validateFinalStreamState { /* do nothing */ }
}