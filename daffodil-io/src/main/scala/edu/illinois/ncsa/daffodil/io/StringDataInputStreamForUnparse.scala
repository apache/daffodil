package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.util.MaybeULong

/**
 * When unparsing, we reuse all the DFA logic to identify delimiters within
 * the data that need to be escaped, so we need to treat the
 * string data being unparsed as a DataInputStream.
 */
final class StringDataInputStreamForUnparse
  extends DataInputStream {
  import DataInputStream._

  var str: String = null
  var dis: DataInputStream = null

  private val utf8Decoder = StandardCharsets.UTF_8.newDecoder()

  // TODO: Performance - we could keep a BBDIS around, and "reset" it by
  // putting it onto a byte buffer that we fill from the string. That way
  // we wouldn't have to allocate a BBDIS unless the string requires a bigger
  // byte buffer.
  //
  def reset(str: String) {
    this.str = str
    val ba = str.getBytes(StandardCharsets.UTF_8)
    dis = ByteBufferDataInputStream(ba)
    dis.setDecoder(utf8Decoder)
  }

  private def doNotUse = Assert.usageError("Not to be called on " + Misc.getNameFromClass(this))

  def areDebugging: Boolean = false

  def asIteratorChar: DataInputStream.CharIterator = {
    Assert.usage(dis != null, "Must call reset(str) before any other method.")
    dis.asIteratorChar
  }

  override def align(bitAlignment1b: Int): Boolean = dis.align(bitAlignment1b)
  override def bitLimit0b = dis.bitLimit0b
  override def bitPos0b: Long = dis.bitPos0b
  override def discard(mark: DataInputStream.Mark): Unit = dis.discard(mark)
  override def fillByteBuffer(bb: java.nio.ByteBuffer): MaybeInt = doNotUse
  override def fillCharBuffer(cb: java.nio.CharBuffer) = dis.fillCharBuffer(cb)
  override def futureData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  override def getBinaryDouble(): Double = doNotUse
  override def getBinaryFloat(): Float = doNotUse
  override def getSignedBigInt(bitLengthFrom1: Int): BigInt = doNotUse
  override def getSignedLong(bitLengthFrom1To64: Int): Long = doNotUse
  override def getUnsignedBigInt(bitLengthFrom1: Int): BigInt = doNotUse
  override def getUnsignedLong(bitLengthFrom1To64: Int): passera.unsigned.ULong = doNotUse
  override def isAligned(bitAlignment1b: Int): Boolean = dis.isAligned(bitAlignment1b)
  override def limits: DataStreamCommon.Limits = dis.limits
  override def lookingAt(matcher: java.util.regex.Matcher, initialRegexMatchLimitInChars: Long): Boolean =
    dis.lookingAt(matcher, initialRegexMatchLimitInChars)
  override def mark: DataInputStream.Mark = dis.mark
  override def markPos = dis.markPos
  override def pastData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  override def reset(mark: DataInputStream.Mark): Unit = dis.reset(mark)
  override def resetPos(m: MarkPos) = dis.resetPos(m)
  override def setBinaryFloatRep(binaryFloatRep: BinaryFloatRep): Unit = doNotUse
  override def setBitLimit0b(bitLimit0b: MaybeULong): Boolean = doNotUse
  override def setBitOrder(bitOrder: BitOrder): Unit = doNotUse
  override def setByteOrder(byteOrder: ByteOrder): Unit = doNotUse
  override def byteOrder: ByteOrder = doNotUse
  // override def setCharWidthInBits(charWidthInBits: MaybeInt): Unit = doNotUse
  override def setDebugging(setting: Boolean): Unit = doNotUse
  override def getDecoder = doNotUse
  override def setDecoder(decoder: java.nio.charset.CharsetDecoder): Unit = doNotUse
  override def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = doNotUse
  // override def setEncodingMandatoryAlignment(bitAlignment: Int): Unit = doNotUse
  override def setLimits(newLimits: DataStreamCommon.Limits): Unit = doNotUse
  override def setMaybeUTF16Width(maybeUTF16Width: Maybe[UTF16Width]): Unit = doNotUse
  override def isDefinedForLength(length: Long): Boolean = doNotUse
  override def skip(nBits: Long): Boolean = doNotUse
  override def skipChars(nChars: Long): Boolean = getString(nChars).isDefined
  override def remainingBits: MaybeULong = doNotUse
  override protected[io] def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit = doNotUse
  override def validateFinalStreamState {} // does nothing
}
