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

/**
 * When unparsing, we reuse all the DFA logic to identify delimiters within
 * the data that need to be escaped, so we need to treat the
 * string data being unparsed as a DataInputStream.
 */
class StringDataInputStreamForUnparse
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

  def align(bitAlignment1b: Int): Boolean = dis.align(bitAlignment1b)
  def bitLimit0b: Maybe[Long] = dis.bitLimit0b
  def bitPos0b: Long = dis.bitPos0b
  def discard(mark: DataInputStream.Mark): Unit = dis.discard(mark)
  def fillByteBuffer(bb: java.nio.ByteBuffer): Maybe[Int] = doNotUse
  def fillCharBuffer(cb: java.nio.CharBuffer): Maybe[Long] = dis.fillCharBuffer(cb)
  def futureData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  def getBinaryDouble(): Maybe[Double] = doNotUse
  def getBinaryFloat(): Maybe[Float] = doNotUse
  def getSignedBigInt(bitLengthFrom1: Int): Maybe[BigInt] = doNotUse
  def getSignedLong(bitLengthFrom1To64: Int): Maybe[Long] = doNotUse
  def getUnsignedBigInt(bitLengthFrom1: Int): Maybe[BigInt] = doNotUse
  def getUnsignedLong(bitLengthFrom1To64: Int): Maybe[passera.unsigned.ULong] = doNotUse
  def isAligned(bitAlignment1b: Int): Boolean = dis.isAligned(bitAlignment1b)
  def limits: DataStreamCommon.Limits = dis.limits
  def lookingAt(matcher: java.util.regex.Matcher, initialRegexMatchLimitInChars: Long): Boolean =
    dis.lookingAt(matcher, initialRegexMatchLimitInChars)
  def mark: DataInputStream.Mark = dis.mark
  def markPos = dis.markPos
  def pastData(nBytesRequested: Int): java.nio.ByteBuffer = doNotUse
  def reset(mark: DataInputStream.Mark): Unit = dis.reset(mark)
  def resetPos(m: MarkPos) = dis.resetPos(m)
  def setBinaryFloatRep(binaryFloatRep: BinaryFloatRep): Unit = doNotUse
  def setBitLimit0b(bitLimit0b: Maybe[Long]): Boolean = doNotUse
  def setBitOrder(bitOrder: BitOrder): Unit = doNotUse
  def setByteOrder(byteOrder: ByteOrder): Unit = doNotUse
  def byteOrder: ByteOrder = doNotUse
  def setCharWidthInBits(charWidthInBits: Maybe[Int]): Unit = doNotUse
  def setDebugging(setting: Boolean): Unit = doNotUse
  def getDecoder = doNotUse
  def setDecoder(decoder: java.nio.charset.CharsetDecoder): Unit = doNotUse
  def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = doNotUse
  def setEncodingMandatoryAlignment(bitAlignment: Int): Unit = doNotUse
  def setLimits(newLimits: DataStreamCommon.Limits): Unit = doNotUse
  def setMaybeUTF16Width(maybeUTF16Width: Maybe[UTF16Width]): Unit = doNotUse
  def isDefinedForLength(length: Long): Boolean = doNotUse
  def skip(nBits: Long): Boolean = doNotUse
  def skipChars(nChars: Long): Boolean = getString(nChars).isDefined
  def withBitLengthLimit(lengthLimitInBits: Long)(body: => Unit): Boolean = doNotUse
  def remainingBits: Maybe[Long] = doNotUse
  def validateFinalStreamState {} // does nothing
}
