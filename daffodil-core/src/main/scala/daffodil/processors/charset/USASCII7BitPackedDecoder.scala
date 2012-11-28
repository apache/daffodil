package daffodil.processors.charset

import java.nio.charset.{ Charset, CoderResult, CharsetDecoder, CharsetEncoder }
import java.nio.ByteBuffer
import java.nio.CharBuffer
import daffodil.exceptions.Assert
import daffodil.util.Misc

object USASCII7BitPackedCharset
  extends java.nio.charset.Charset("US-ASCII-7-bit-packed", Array()) {

  final def contains(cs: Charset): Boolean = {
    false
  }

  final def newDecoder(): CharsetDecoder = {
    new USASCII7BitPackedDecoder
  }

  final def newEncoder(): CharsetEncoder = {
    new USASCII7BitPackedEncoder
  }
}

/**
 * You have to initialize one of these for a specific ByteBuffer because
 * the encoding is 7-bits wide, so we need additional state beyond just
 * the byte position and limit that a ByteBuffer provides in order to
 * properly sequence through the data.
 */
class USASCII7BitPackedDecoder
  extends java.nio.charset.CharsetDecoder(USASCII7BitPackedCharset, 1, 1) {

  override def implReset() {
    println("Reset")
    isInitialized = false
    buf = null
    bitLimit = Long.MaxValue
    bitPos = 0
    hasPriorByte = false
    priorByte = 0
    priorByteBitCount = 0

  }

  def initialize(b: ByteBuffer, bitLim: Long = Long.MaxValue) {
    Assert.usage(!isInitialized, "Already initialized. Cannot re-initialize.")
    buf = b
    bitLimit = bitLim
    isInitialized = true
  }

  var bitLimit: Long = Long.MaxValue
  var bitPos = 0

  private var isInitialized = false
  private var buf: ByteBuffer = _

  private var priorByte = 0
  private var hasPriorByte = false

  /**
   * When there is a prior byte, this gives the number of bits from it
   * that have not yet been consumed. Zero when there is no prior byte.
   *
   * Value ranges from 1 to 7. Can never be 8.
   */
  private var priorByteBitCount = 0

  val widthOfAByte = 7 // the whole point of this class is that we consume this many bits per byte.

  /**
   * Convert signed Byte type to Int that is the unsigned equivalent.
   */
  def asUnsignedByte(n: Byte): Int = {
    if (n < 0) 256 + n else n
  }

  // some constants to make the table dispatch below clearer.
  private val NoData = false
  private val YesData = true
  private val NoSpace = false
  private val YesSpace = true
  private val NoPrior = false
  private val YesPrior = true

  final def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult = {

    def output(charCode: Int) {
      println("charcode = %2x".format(charCode))
      val char = charCode.toChar
      out.put(char)
      bitPos += widthOfAByte
    }

    def asBits(b: Int) = {
      val hex = "%02x".format(b)
      // println(hex)
      Misc.hex2Bits(hex)
    }

    if (!isInitialized) initialize(in)
    Assert.usage(in eq buf, "decoding must use the same ByteBuffer supplied at initialization (or on first use).")
    Assert.invariant(bitPos <= bitLimit)

    while (true) {
      if (bitPos + this.widthOfAByte > bitLimit) {
        // not enough bits to create another character
        return CoderResult.UNDERFLOW
      }

      (hasPriorByte, priorByteBitCount, in.hasRemaining(), out.hasRemaining()) match {
        // 
        // Fresh start, and also every 56 bits we hit a clean
        // byte boundary again
        //
        case (NoPrior, 0, YesData, YesSpace) => {
          val currentByte = asUnsignedByte(in.get())
          println("no prior byte, current byte = %s".format(asBits(currentByte)))

          priorByte = currentByte
          priorByteBitCount = 8 - widthOfAByte
          hasPriorByte = true

          val currentCharCode = currentByte >> 1 // we take the most significant bits first.

          output(currentCharCode)

        }
        case (NoPrior, 0, NoData, _) => return CoderResult.UNDERFLOW
        case (NoPrior, 0, _, NoSpace) => return CoderResult.OVERFLOW
        case (NoPrior, n, _, _) => Assert.invariantFailed("priorByteBitCount should be 0 when there is no prior byte")
        case (YesPrior, 0, _, _) => Assert.invariantFailed("priorByteBitCount should not be 0 when there is a prior byte")
        case (YesPrior, n, _, _) if (n > 7) => Assert.invariantFailed("priorByteBitCount should be from 1 to 7 when there is a prior byte")
        case (YesPrior, 7, _, YesSpace) => {
          // Case where we previously used only 1 bit from the prior byte
          // so we can produce the next character from the remaining 7 bits of this byte.
          // We don't need more input.
          println("prior byte = %s, no current byte needed, priorByteBitCount = %d".format(asBits(priorByte), priorByteBitCount))

          val currentByte = priorByte
          val currentCharCode = currentByte & 0x7F // least significant bits for remainder
          output(currentCharCode)

          hasPriorByte = false
          priorByte = 0
          priorByteBitCount = 0
        }
        case (YesPrior, n, NoData, _) => {
          // We have a partial charcode in prior byte, but there are no more bytes to be had from the
          // ByteBuffer so we can't complete it without more data.
          return CoderResult.UNDERFLOW
        }
        case (YesPrior, n, YesData, YesSpace) => {
          // Straddling bytes. We need another input byte to make up a full character.
          val currentByte = asUnsignedByte(in.get())
          println("prior byte = %s, current byte = %s, priorByteBitCount = %d".format(asBits(priorByte), asBits(currentByte), priorByteBitCount))
          val priorMask = 0xFF >> (8 - priorByteBitCount)
          val priorBits = priorByte & priorMask // keeps LSBs we're going to use
          val currentByteBitCount = widthOfAByte - priorByteBitCount
          val currentBitsAlone = currentByte >> (8 - currentByteBitCount)
          val priorBitsInPosition = priorBits << currentByteBitCount
          val currentCharCode = priorBitsInPosition | currentBitsAlone
          priorByte = currentByte
          hasPriorByte = true // remains true
          priorByteBitCount = 8 - currentByteBitCount
          Assert.invariant(priorByteBitCount > 0)
          Assert.invariant(priorByteBitCount <= 7)

          output(currentCharCode)
        }
        case (_, _, _, NoSpace) => return CoderResult.OVERFLOW
      }

    } // end while loop

    Assert.impossible("Incorrect return from decodeLoop.")

  }
}

class USASCII7BitPackedEncoder
  extends java.nio.charset.CharsetEncoder(USASCII7BitPackedCharset, 1, 1) {

  def encodeLoop(cb: CharBuffer, bb: ByteBuffer): CoderResult = {
    Assert.notYetImplemented()
  }
}

//
//case class BitState(bitLimit: Long, bitPos: Long, byteWidth: Int) {
//  Assert.usage(byteWidth <= 8)
//  Assert.usage(bitPos >= 0)
//  Assert.usage(bitLimit >= bitPos)
//}

///**
// * This variant on ByteBuffer supports widths, that is, you can provide a width of 7, 
// * and it will consume 7 bits per 'byte' returned. It will only end when all bits have been 
// * used up, or there aren't enough bits. 
// */
//class ByteBufferWithBitSupport private (val state: BitState, private val delegate: ByteBuffer)
//  extends java.nio.ByteBufferBase {
//
//  def bitPos = state.bitPos
//  def bitLimit = state.bitLimit
//
//  def this(bitLimit: Long, byteWidth: Int) = this(BitState(bitLimit, 0, byteWidth), ByteBuffer.allocate(daffodil.compiler.Compiler.byteBufferInitialSize.toInt))
//
//  def get(): Byte = { delegate.get() }
//
//  def slice(): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def duplicate(): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def asReadOnlyBuffer(): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def put(b: Byte): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def get(index: Int): Byte = { Assert.usageError("operation not supported.") }
//
//  def put(index: Int, b: Byte): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def compact(): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def isDirect(): Boolean = { delegate.isDirect() }
//
//  def getChar(): Char = { Assert.usageError("operation not supported.") }
//
//  def putChar(value: Char): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def getChar(index: Int): Char = { Assert.usageError("operation not supported.") }
//
//  def putChar(index: Int, value: Char): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def asCharBuffer(): CharBuffer = { Assert.usageError("operation not supported.") }
//
//  def getShort(): Short = { Assert.usageError("operation not supported.") }
//
//  def putShort(value: Short): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def getShort(index: Int): Short = { Assert.usageError("operation not supported.") }
//
//  def putShort(index: Int, value: Short): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def asShortBuffer(): java.nio.ShortBuffer = { Assert.usageError("operation not supported.") }
//
//  def getInt(): Int = { Assert.usageError("operation not supported.") }
//
//  def putInt(value: Int): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def getInt(index: Int): Int = { Assert.usageError("operation not supported.") }
//
//  def putInt(index: Int, value: Int): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def asIntBuffer(): java.nio.IntBuffer = { Assert.usageError("operation not supported.") }
//
//  def getLong(): Long = { Assert.usageError("operation not supported.") }
//
//  def putLong(value: Long): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def getLong(index: Int): Long = { Assert.usageError("operation not supported.") }
//
//  def putLong(index: Int, value: Long): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def asLongBuffer(): java.nio.LongBuffer = { Assert.usageError("operation not supported.") }
//
//  def getFloat(): Float = { Assert.usageError("operation not supported.") }
//
//  def putFloat(value: Float): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def getFloat(index: Int): Float = { Assert.usageError("operation not supported.") }
//
//  def putFloat(index: Int, value: Float): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def asFloatBuffer(): java.nio.FloatBuffer = { Assert.usageError("operation not supported.") }
//
//  def getDouble(): Double = { Assert.usageError("operation not supported.") }
//
//  def putDouble(value: Double): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def getDouble(index: Int): Double = { Assert.usageError("operation not supported.") }
//
//  def putDouble(index: Int, value: Double): ByteBuffer = { Assert.usageError("operation not supported.") }
//
//  def asDoubleBuffer(): java.nio.DoubleBuffer = { Assert.usageError("operation not supported.") }
//
//  def compareTo(o: ByteBuffer): Int = { Assert.usageError("operation not supported.") }
//
//}