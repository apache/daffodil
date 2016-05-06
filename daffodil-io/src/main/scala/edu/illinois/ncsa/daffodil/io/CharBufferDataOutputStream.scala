package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import passera.unsigned.ULong
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.util.Misc

private[io] class CharBufferDataOutputStreamState extends DataOutputStreamState
/**
 * Used to implement unparsing when length is specified in lengthUnits 'characters' when
 * the encoding is variable width (e.g., utf-8)
 */
final class CharBufferDataOutputStream
  extends DataOutputStream with DataOutputStreamImplMixin {

  private[io] def isBuffering: Boolean = true

  private def doNotUse = Assert.usageError("Not to be called on " + Misc.getNameFromClass(this))

  protected def getJavaOutputStream(): java.io.OutputStream = doNotUse
  protected def setJavaOutputStream(newOutputStream: java.io.OutputStream): Unit = doNotUse

  private var target: CharBuffer = null

  def setCharBuffer(cb: CharBuffer) {
    target = cb
  }

  override def flush() {
    // do nothing
  }

  protected override val st = new CharBufferDataOutputStreamState

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
  override def putBinaryFloat(float: Float): Boolean = notToBeUsed
  override def putBinaryDouble(double: Double): Boolean = notToBeUsed
  final override protected def putLong_BE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  final override protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  final override protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = notToBeUsed
  override def skip(nBits: Long): Boolean = notToBeUsed
  override def futureData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  override def pastData(nBytesRequested: Int): ByteBuffer = notToBeUsed
  override def setByteOrder(byteOrder: ByteOrder): Unit = notToBeUsed
  override def byteOrder: ByteOrder = notToBeUsed
  override def validateFinalStreamState { /* do nothing */ }
}
