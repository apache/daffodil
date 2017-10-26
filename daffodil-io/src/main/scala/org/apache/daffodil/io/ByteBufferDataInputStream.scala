/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.nio.charset.CharsetDecoder
import java.nio.charset.StandardCharsets
import java.nio.charset.CodingErrorAction
import org.apache.commons.io.IOUtils
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.CharBuffer
import java.nio.channels.FileChannel
import java.nio.channels.Channels
import edu.illinois.ncsa.daffodil.util.Bits
import passera.unsigned.ULong
import java.io.InputStream
import java.nio.charset.CoderResult
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.Pool
import edu.illinois.ncsa.daffodil.util.MStackOf
import edu.illinois.ncsa.daffodil.api.DataStreamLimits

/**
 * Factory for creating this type of DataInputStream
 *
 * Examines channels and input streams to see if they are associated
 * to files. If so it memory maps the file.
 */
object ByteBufferDataInputStream {

  def defaultCodingErrorAction = CodingErrorAction.REPLACE

  /**
   * This is the only method that actually constructs the object.
   * The other methods just arrange to call this one.
   *
   * The initialBitPos0b is mostly there for testing purposes - to create small unit tests that
   * start with non-byte alignment and such.
   */
  def apply(byteBuffer: ByteBuffer, initialBitPos0b: Long): DataInputStream =
    new ByteBufferDataInputStream(byteBuffer, initialBitPos0b)

  /**
   * Mostly this is for unit testing
   */
  def apply(byteArray: Array[Byte], initialBitPos0b: Long = 0): DataInputStream = apply(ByteBuffer.wrap(byteArray), initialBitPos0b)

  /**
   * File becomes a direct mapped byte buffer.
   */
  def apply(file: java.io.File, initialBitPos0b: Long): DataInputStream = {
    Assert.usage(file.length() < Int.MaxValue)
    val path = file.toPath()
    val channel = FileChannel.open(path)
    apply(channel, initialBitPos0b)
  }

  /**
   * If it is a FileInputStream, get its channel so we can mmap it.
   * Otherwise copy into a byte array.
   */
  def apply(in: InputStream, initialBitPos0b: Long): DataInputStream = {
    in match {
      case fis: java.io.FileInputStream => {
        val fc = fis.getChannel()
        apply(fc, initialBitPos0b)
      }
      case _ => {
        // copy the contents of the stream into an array of bytes
        val bos = new ByteArrayOutputStream
        IOUtils.copy(in, bos)
        bos.flush()
        bos.close()
        in.close()
        val ba = bos.toByteArray
        apply(ba, initialBitPos0b)
      }
    }
  }

  /**
   * If it's a file channel, mmap it. Otherwise treat like an InputStream
   */
  def apply(channel: java.nio.channels.ReadableByteChannel, initialBitPos0b: Long): DataInputStream = {
    channel match {
      case fc: FileChannel => {
        val bbuf = fc.map(FileChannel.MapMode.READ_ONLY, 0L, fc.size())
        apply(bbuf, initialBitPos0b)
      }
      case _ => apply(Channels.newInputStream(channel), initialBitPos0b)
    }
  }

  def fromByteChannel(
    in: java.nio.channels.ReadableByteChannel,
    bitStartPos0b: Long,
    numBitsLimit: Long) = { // a count, not a position
    val dis = ByteBufferDataInputStream(in, bitStartPos0b)
    if (numBitsLimit > 0) dis.setBitLimit0b(MaybeULong(bitStartPos0b + numBitsLimit))
    dis
  }

}

/**
 * The state that should be merged into PState eventually.
 *
 * Some state is only needed per thread, so we don't need to copy it
 * over and over when backtracking to save/restore it. We just need
 * a separate one per thread.
 *
 * This is very unlike the state that must be saved/restored by the mark/reset discipline
 */
private[io] trait TLStateMixin {
  /*
   * Cache these so that rather than a hash-lookup for the thread-local each time we access this,
   * we're just accessing a data member.
   */
  val skipCharBuf = TLState.get.skipCharBuf
  val regexMatchBuffer = TLState.get.regexMatchBuffer
  val lengthDeterminationBuffer = TLState.get.lengthDeterminationBuffer
  val markStack = TLState.get.markStack
  val markPool = TLState.get.markPool
}

/**
 * In performance testing, we sometimes do runs on 20K files or more, which means many
 * PState instances and ByteBufferDataInputStream instances are allocated.
 *
 * To avoid allocating these fairly large objects more than once per thread,
 * we use a ThreadLocal here.
 */
private[io] object TLState extends ThreadLocal[TLState] {
  override def initialValue = new TLState
}

private[io] class TLState {
  val skipCharBuf = CharBuffer.allocate(BBSLimits.maximumSimpleElementSizeInCharacters.toInt)
  val regexMatchBuffer = CharBuffer.allocate(BBSLimits.maximumRegexMatchLengthInCharacters.toInt)
  val lengthDeterminationBuffer = CharBuffer.allocate(BBSLimits.maximumRegexMatchLengthInCharacters.toInt)
  val markStack = new MStackOf[MarkState]
  val markPool = new MarkPool()
}
/**
 * The state that must be saved and restored by mark/reset calls
 */
final class MarkState(initialBitPos0b: Long)
  extends DataStreamCommonState with DataInputStream.Mark {

  def defaultCodingErrorAction = ByteBufferDataInputStream.defaultCodingErrorAction

  def bitPos0b: Long = (savedBytePosition0b << 3) + bitOffset0b

  override def equals(other: Any) = other match {
    case ar: AnyRef => this eq ar // only if the same object
    case _ => false
  }

  var savedBytePosition0b: Int = 0
  var savedByteLimit0b: Int = 0
  /**
   * We store the bit position in two parts. The whole-byte part is the byte buffer's position().
   * The fraction-of-a-byte part is this bit offset. If one needs the bitPos0b, that's a method that
   * recombines the position() with this offset.
   */
  var bitOffset0b: Int = (initialBitPos0b % 8).toInt
  var savedByteOrder: java.nio.ByteOrder = _

  var maybeBitLimitOffset0b: MaybeULong = MaybeULong(0)

  var decoder: CharsetDecoder = {
    val dec = StandardCharsets.UTF_8.newDecoder()
    dec.onMalformedInput(defaultCodingErrorAction)
    dec.onUnmappableCharacter(defaultCodingErrorAction)
    dec
  }
  var codingErrorAction: CodingErrorAction = defaultCodingErrorAction

  var adaptedRegexMatchBufferLimit: Int = 0

  val charIteratorState = new CharIteratorState
  // any members added here must be added to assignFrom below.

  def assignFrom(other: MarkState): Unit = {
    super.assignFrom(other)
    // this.dis = other.dis
    this.savedBytePosition0b = other.savedBytePosition0b
    this.savedByteLimit0b = other.savedByteLimit0b
    this.bitOffset0b = other.bitOffset0b
    this.savedByteOrder = other.savedByteOrder
    this.maybeBitLimitOffset0b = other.maybeBitLimitOffset0b
    this.decoder = other.decoder
    this.codingErrorAction = other.codingErrorAction
    this.adaptedRegexMatchBufferLimit = other.adaptedRegexMatchBufferLimit
    this.charIteratorState.assignFrom(other.charIteratorState)
  }
}

private[io] class MarkPool() extends Pool[MarkState] {
  override def allocate = new MarkState(0)
}

/**
 * These magic numbers aren't used really except for unit test situations.
 * The DataProcessor calls setLimits(...) before parsing, which replaces this.
 * However, as this daffodil-io module is more primitive than the
 * daffodil runtime, this allows us to unit test things in isolation.
 */
private object BBSLimits extends DataStreamLimits {
  def maximumSimpleElementSizeInBytes: Long = 1024 * 1024
  def maximumSimpleElementSizeInCharacters: Long = 1024 * 1024
  def maximumForwardSpeculationLengthInBytes: Long = 1024 * 1024
  def maximumRegexMatchLengthInCharacters: Long = 1024 * 1024
  def defaultInitialRegexMatchLimitInChars: Long = maximumRegexMatchLengthInCharacters //32
}
/**
 * Simple realization of the DataInputStream API
 *
 * Underlying representation is just a ByteBuffer containing all input data.
 *
 * In many cases this can be a direct byte buffer, and so doesn't occupy storage
 * in the java heap.
 *
 * At top level (when creating one for all the data), the supplied byte buffer must have
 * position 0 and its limit should be equal to its capacity.
 *
 * The backward compatibility layers, however, until removed, make copies of
 * these objects, and those may have non-zero positions. E.g., see the def makeACopy.
 */
final class ByteBufferDataInputStream private (var data: ByteBuffer, initialBitPos0b: Long)
  extends DataInputStreamImplMixin with TLStateMixin {
  import DataInputStream._

  Assert.usage(initialBitPos0b >= 0)
  val initialBytePos0b = initialBitPos0b / 8

  Assert.usage(initialBytePos0b < data.capacity() ||
    (initialBytePos0b == data.capacity() && (initialBitPos0b % 8 == 0))) // when equal to capacity, bits of fragment partial byte can't spill over past the capacity.

  data.position((initialBitPos0b / 8).toInt) // set data position based on the initialBitPos0b

  override def toString = {
    val bp0b = bitPos0b
    val bl0b = bitLimit0b
    val bl0b1 = if (bl0b.isDefined) bl0b.get.toString else "none"
    val str = "DataInputStream(bitPos=" + bp0b +
      ", bitLimit=" + bl0b1 + ")"
    str
  }

  val st = new MarkState(initialBitPos0b)

  override final def cst = st

  private def bytePos0b_ = data.position

  def bitPos0b: Long = (bytePos0b_ << 3) + st.bitOffset0b

  def setBitPos0b(newBitPos0b: Long) {
    // threadCheck()
    Assert.invariant(newBitPos0b < (Int.MaxValue * 8L))
    Assert.invariant(newBitPos0b >= 0)
    val newBitOffset0b = (newBitPos0b & 0x7).toInt
    val newBytePos0b = (newBitPos0b >> 3).toInt
    val bitLim0b = bitLimit0b.get
    if (newBitPos0b <= bitLim0b) {
      data.position(newBytePos0b)
      st.bitOffset0b = newBitOffset0b
    } else {
      // in this case, we're asking to set the bit pos past the limit
      // clamp it to the limit
      setBitPos0b(bitLim0b)
    }
  }

  var bitLimit0b: MaybeULong = MaybeULong(calcBitLimit0b)

  def calcBitLimit0b: Long = {
    // threadCheck()
    // we always have a bitLimit in this implementation
    Assert.invariant(st.maybeBitLimitOffset0b.isDefined)
    (data.limit << 3) + st.maybeBitLimitOffset0b.get
  }

  override def setBitLimit0b(newBitLimit0b: MaybeULong): Boolean = {
    // threadCheck()
    Assert.invariant(newBitLimit0b.isDefined)
    val newBitLimit = newBitLimit0b.get
    if (newBitLimit > Int.MaxValue) return false
    Assert.usage(newBitLimit >= bitPos0b)
    if (st.maybeBitLimitOffset0b.isDefined) {
      if (newBitLimit > this.calcBitLimit0b) return false // new limit is beyond our available length.
    }
    resetBitLimit0b(newBitLimit0b)
    true
  }

  /*
   * Want package private - but macro usage requires public
   */
  final override def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit = {
    // threadCheck()
    Assert.invariant(savedBitLimit0b.isDefined)
    val newBitLimit = savedBitLimit0b.get
    val bitLim = newBitLimit.toInt
    val newBitLimitOffset0b = bitLim & 0x7
    val newByteLimit = bitLim >> 3
    data.limit(newByteLimit)
    st.maybeBitLimitOffset0b = MaybeULong(newBitLimitOffset0b)
    bitLimit0b = MaybeULong(calcBitLimit0b)
  }

  def setByteOrder(byteOrder: ByteOrder): Unit = {
    // threadCheck()
    byteOrder match {
      case ByteOrder.BigEndian => data.order(java.nio.ByteOrder.BIG_ENDIAN)
      case ByteOrder.LittleEndian => data.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    }
    st.savedByteOrder = data.order()
  }

  def byteOrder: ByteOrder = {
    data.order match {
      case java.nio.ByteOrder.BIG_ENDIAN => ByteOrder.BigEndian
      case java.nio.ByteOrder.LITTLE_ENDIAN => ByteOrder.LittleEndian
    }
  }

  def getDecoder = st.decoder

  def setDecoder(decoder: CharsetDecoder): Unit = {
    // threadCheck()
    if (st.decoder == decoder) return
    st.decoder = decoder
    st.priorEncoding = decoder.charset()
    st.decoder.onMalformedInput(st.codingErrorAction)
    st.decoder.onUnmappableCharacter(st.codingErrorAction)
    val cs = decoder.charset()
    if (cs == StandardCharsets.UTF_16 || cs == StandardCharsets.UTF_16BE || cs == StandardCharsets.UTF_16LE) {
      st.encodingMandatoryAlignmentInBits = 8
      if (st.maybeUTF16Width.isDefined && st.maybeUTF16Width.get == UTF16Width.Fixed) {
        st.maybeCharWidthInBits = MaybeInt(16)
      } else {
        st.maybeCharWidthInBits = MaybeInt.Nope
      }
    } else {
      cs match {
        case decoderWithBits: NonByteSizeCharset => {
          st.encodingMandatoryAlignmentInBits = 1
          st.maybeCharWidthInBits = MaybeInt(decoderWithBits.bitWidthOfACodeUnit)
        }
        case _ => {
          st.encodingMandatoryAlignmentInBits = 8
          val encoder = cs.newEncoder()
          val maxBytes = encoder.maxBytesPerChar()
          if (maxBytes == encoder.averageBytesPerChar()) {
            st.maybeCharWidthInBits = MaybeInt((maxBytes * 8).toInt)
          } else {
            st.maybeCharWidthInBits = MaybeInt.Nope
          }
        }
      }
    }
  }

  def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = {
    // threadCheck()
    st.codingErrorAction = eep match {
      case EncodingErrorPolicy.Replace => CodingErrorAction.REPLACE
      case EncodingErrorPolicy.Error => CodingErrorAction.REPORT
    }
    st.decoder.onMalformedInput(st.codingErrorAction)
    st.decoder.onUnmappableCharacter(st.codingErrorAction)
    ()
  }

  def getByteArray(bitLengthFrom1: Int): Array[Byte] = {
    // threadCheck()
    if (!isDefinedForLength(bitLengthFrom1)) throw DataInputStream.NotEnoughDataException(bitLengthFrom1)

    val arraySize = (bitLengthFrom1 + 7) / 8
    val array = new Array[Byte](arraySize)
    fillByteArray(array, bitLengthFrom1)
    array
  }

  // accepts a preallocated array and a bitLength. Reads the specified number
  // of bits and stores them in the array in big endian byte order and most
  // significant bit first bit order. The most significant byte is stored in
  // the zero'th index in the array. This means that if the array is larger
  // than the number of bytes needed for the specified number of bits, the
  // trailing bytes will be untouched and should be ignored by the caller.
  private def fillByteArray(array: Array[Byte], bitLengthFrom1: Int): Unit = {
    if (isAligned(8) && bitLengthFrom1 % 8 == 0) {
      fillByteArrayAlignedNoFragment(array, bitLengthFrom1)
    } else {
      fillByteArrayUnalignedOrFragment(array, bitLengthFrom1)
    }
    Assert.invariant(bitPos0b <= bitLimit0b.get)
  }

  private def fillByteArrayAlignedNoFragment(array: Array[Byte], bitLengthFrom1: Int): Unit = {
    // threadCheck()
    Assert.usage(isAligned(8))
    Assert.usage(bitLengthFrom1 % 8 == 0)

    val savedBitPos0b = bitPos0b

    val bytesToFill = bitLengthFrom1 / 8
    Assert.invariant(array.size >= bytesToFill)

    if (byteOrder == ByteOrder.BigEndian && st.bitOrder == BitOrder.MostSignificantBitFirst) {
      // bits & bytes are already in order, read them straight into the array
      data.get(array, 0, bytesToFill)
    } else {
      // we are either LittleEndian & MSBF or BigEndian & LSBF. In either case,
      // we just need to flip the bytes to make it BigEndian MSBF. The bits are
      // in the correct order.
      var i = bytesToFill - 1
      while (i >= 0) {
        array(i) = data.get()
        i -= 1
      }
    }

    setBitPos0b(savedBitPos0b + bitLengthFrom1)
  }

  private def fillByteArrayUnalignedOrFragment(array: Array[Byte], bitLengthFrom1: Int): Unit = {
    // threadCheck()
    Assert.usage(!isAligned(8) || bitLengthFrom1 % 8 != 0)

    // might need to increase the limit to get a fragment byte
    val nBytesNeeded = computeNBytesNeeded(bitLengthFrom1, st.bitOffset0b)
    val savedDataLimit = data.limit()
    val savedBitPos0b = bitPos0b
    data.limit(data.position() + nBytesNeeded)
    
    val bytesToFill = (bitLengthFrom1 + 7) / 8
    Assert.invariant(array.size >= bytesToFill)
    val nFragmentBits = bitLengthFrom1 % 8

    var priorByte = Bits.asUnsignedByte(data.get())
    var i = 0

    @inline
    def addFragmentByte() = {
      // This function is used at either the beginning or end of this function
      // to read a fragement byte and store it in the correct location in the
      // output array. It modifies variables outside of the function, such as
      // the array, array index (i), and potentialy the prirorByte, thus making
      // it a closure. Note that it is a nested function so that it can access
      // those local variables. Normally, this would would lead to an
      // allocation which is bad in this cricial code. However, marking it as
      // @inline prevents that. So this allows for consolidating duplicate code
      // keeping the code somewhat clean, and allows access to local variables
      // without the allocaiton overhead of closures.
      val bitsLeftInPriorByte = 8 - st.bitOffset0b
      val fragmentByte =
        if (nFragmentBits <= bitsLeftInPriorByte) {
          // all nFragmentBits can come from from the priror byte
          val composedByte = 
            st.bitOrder match {
              case BitOrder.MostSignificantBitFirst => ((priorByte << st.bitOffset0b) & 0xFF) >>> (8 - nFragmentBits)
              case BitOrder.LeastSignificantBitFirst => ((priorByte << (bitsLeftInPriorByte - nFragmentBits)) & 0xFF) >>> (8 - nFragmentBits)
            }
          composedByte
        } else {
          // we need all the fragment bits from prior plus some bits from cur
          val bitsToGetFromCur = nFragmentBits - bitsLeftInPriorByte
          val curByte = Bits.asUnsignedByte(data.get())
          val composedByte =
            st.bitOrder match {
              case BitOrder.MostSignificantBitFirst => {
                val priorContribution = ((priorByte << st.bitOffset0b) & 0xFF) >>> (st.bitOffset0b - bitsToGetFromCur)
                val curContribution = curByte >>> (8 - bitsToGetFromCur)
                priorContribution | curContribution
              }
              case BitOrder.LeastSignificantBitFirst => {
                val priorContribution = (priorByte >>> st.bitOffset0b)
                val curContribution = ((curByte << (8 - bitsToGetFromCur)) & 0xFF) >>> (8 - (bitsToGetFromCur + bitsLeftInPriorByte))
                priorContribution | curContribution
              }
            }
          priorByte = curByte
          composedByte
        }
      if (byteOrder =:= ByteOrder.LittleEndian) {
        array(bytesToFill - 1 - i) = Bits.asSignedByte(fragmentByte)
      } else {
        array(i) = Bits.asSignedByte(fragmentByte)
      }
      i += 1
    }

    val newByteOffset = 
      if (byteOrder =:= ByteOrder.BigEndian && nFragmentBits > 0) {
        addFragmentByte()
        (st.bitOffset0b + nFragmentBits) % 8
      } else {
        st.bitOffset0b
      }

    if (newByteOffset == 0) {
      byteOrder match {
        case ByteOrder.BigEndian => {
          // we just parsed a bigEndian fragment byte and it put us back on a byte
          // boundary, so we just need to read the rest of the full aligned bytes
          data.get(array, 1, bytesToFill - 1)
        } 
        case ByteOrder.LittleEndian => {
          // we're starting on a byte boundary, so lets just consume all the
          // whole bytes quickly, reversing positions in the array. We'll not
          // parse the last fragment byte and let that be done later
          while (i < bytesToFill - 1) {
            // we already got the first byte as a prior Byte, so just set it as
            // the whole byte and get the next priorByte
            array(bytesToFill - i - 1) = Bits.asSignedByte(priorByte)
            // this byte will now either be used in the next iteration of this
            // loop, or as the prior byte in the first iteration of consuming
            // the fragment byte below
            priorByte = Bits.asUnsignedByte(data.get())
            i += 1
          }
        }
      }
    } else {
      val priorShift = newByteOffset
      val curShift = 8 - priorShift

      // If the bitOrder is BE, then have alrady consumed a fragment byte
      // above, so we want to fill in the rest of the array, so fill in the
      // whole array (i.e. stop at the array size). If the byteOrder is LE,
      // then we only want to fill the number of full bytes, and then we'll
      // consume that last fragment byte afterwards
      val stopBytePosition =
        if (byteOrder =:= ByteOrder.BigEndian) bytesToFill
        else (bitLengthFrom1 / 8)

      // consume full bytes
      while (i < stopBytePosition) {
        val curByte = Bits.asUnsignedByte(data.get())
        val composedByte =
          st.bitOrder match {
            case BitOrder.MostSignificantBitFirst =>  ((priorByte << priorShift) & 0xFF) | ((curByte >>> curShift) & 0xFF)
            case BitOrder.LeastSignificantBitFirst => ((priorByte >>> priorShift) & 0xFF) | ((curByte << curShift) & 0xFF)
          }
        if (byteOrder =:= ByteOrder.LittleEndian) {
          array(bytesToFill - 1 - i) = Bits.asSignedByte(composedByte)
        } else {
          array(i) = Bits.asSignedByte(composedByte)
        }
        priorByte = curByte
        i += 1
      }
    }

    if (byteOrder =:= ByteOrder.LittleEndian && nFragmentBits > 0) {
      addFragmentByte()
    }

    data.limit(savedDataLimit) // restore data limit

    setBitPos0b(savedBitPos0b + bitLengthFrom1)
  }

  def getBinaryDouble(): Double = {
    Assert.usage(isAligned(8)) // aligned, so bitOrder doesn't matter
    val db = data.asDoubleBuffer() // note: byte order is inherited from data
    if (db.remaining() < 1) throw DataInputStream.NotEnoughDataException(64)
    else {
      val d = db.get()
      setBitPos0b(bitPos0b + 64)
      d
    }
  }

  def getBinaryFloat(): Float = {
    Assert.usage(isAligned(8)) // aligned, so bitOrder doesn't matter
    val db = data.asFloatBuffer() // note: byte order is inherited from data's current
    if (db.remaining() < 1) throw DataInputStream.NotEnoughDataException(32)
    else {
      val d = db.get()
      setBitPos0b(bitPos0b + 32)
      d
    }
  }

  def computeNBytesNeeded(bitLength: Int, bitOffset0b: Int) = {
    // threadCheck()
    val nBitsRemainingInFirstByte = (8 - bitOffset0b)
    val nAdditionalBitsNeeded = bitLength - nBitsRemainingInFirstByte
    val nAdditionalBytesNeeded = (nAdditionalBitsNeeded >> 3) +
      (if ((nAdditionalBitsNeeded & 0x7) > 0) 1 else 0)
    val needed = 1 + nAdditionalBytesNeeded
    needed
  }

  def signExtend(l: Long, bitLength: Int): Long = {
    Assert.usage(bitLength > 0 && bitLength <= 64)
    if (bitLength == 1) return l // a single bit has no sign to extend
    val shift = 64 - bitLength
    val res = ((l << shift) >> shift) // arithmetic shift right extends sign.
    res
  }

  def unSignExtend(l: Long, bitLength: Int): Long = {
    Assert.usage(bitLength > 0 && bitLength <= 64)
    val mask = if (bitLength == 64) -1L else (1L << bitLength) - 1
    l & mask
  }

  // used by getSignedLong and getUnsignedLong to get up to 8 bytes of data,
  // via fillByteArray, which is then converted to a long
  private val longArray = new Array[Byte](8)

  /**
   * This is the thing we can do fast with one tap on the data
   *
   * No allocation of objects occurs when calling this.
   */
  def getSignedLong(bitLengthFrom1To64: Int): Long = {
    // threadCheck()
    Assert.usage(bitLengthFrom1To64 >= 1)
    Assert.usage(bitLengthFrom1To64 <= 64)

    val res = getUnsignedLong(bitLengthFrom1To64)
    signExtend(res.longValue, bitLengthFrom1To64)
  }

  def getUnsignedLong(bitLengthFrom1To64: Int): ULong = {
    Assert.usage(bitLengthFrom1To64 >= 1)
    Assert.usage(bitLengthFrom1To64 <= 64)

    if (!isDefinedForLength(bitLengthFrom1To64)) throw DataInputStream.NotEnoughDataException(bitLengthFrom1To64)

    val numBytes = (bitLengthFrom1To64 + 7) / 8 

    // will result in the first numBytes the long array filled in
    fillByteArray(longArray, bitLengthFrom1To64)

    var res = Bits.asUnsignedByte(longArray(0)).toLong
    var i = 1
    while (i < numBytes) {
      res = res << 8
      res = res | Bits.asUnsignedByte(longArray(i))
      i += 1
    }
    Assert.invariant(bitPos0b <= bitLimit0b.get)
    ULong(res)
  }

  def getSignedBigInt(bitLengthFrom1: Int): BigInt = {
    // threadCheck()
    Assert.usage(bitLengthFrom1 >= 1)

    if (bitLengthFrom1 <= 64) {
      BigInt(getSignedLong(bitLengthFrom1))
    } else {
      val allBytes = getByteArray(bitLengthFrom1)
      val fragmentLength = bitLengthFrom1 % 8
      if (fragmentLength > 0) {
        // if there is a fragment byte, we need to sign extend the rest of the
        // most significant byte so that the BigInt constructor knows the sign
        // of the bytes
        val shift = 8 - fragmentLength
        allBytes(0) = ((allBytes(0) << shift).toByte >> shift).toByte // arithmetic shift right extends sign.
      }
      BigInt(allBytes)
    }
  }

  def getUnsignedBigInt(bitLengthFrom1: Int): BigInt = {
    Assert.usage(bitLengthFrom1 >= 1)
    val bytes = getByteArray(bitLengthFrom1)
    BigInt(1, bytes)
  }

  /**
   * Determines whether the input stream has this much more data.
   *
   * Does not advance the position
   */
  final def isDefinedForLength(nBits: Long): Boolean = {
    Assert.invariant(bitLimit0b.isDefined)
    val bitLim = bitLimit0b.get
    if (bitPos0b + nBits > bitLim) false
    else true
  }

  def skip(nBits: Long): Boolean = {
    // threadCheck()
    Assert.usage(nBits <= Int.MaxValue)
    if (!this.isDefinedForLength(nBits)) return false
    setBitPos0b(bitPos0b + nBits)
    true
  }

  def mark(requestorID: String): DataInputStream.Mark = {
    val m = markPool.getFromPool(requestorID)
    m.assignFrom(st)
    m.savedBytePosition0b = data.position()
    m.savedByteLimit0b = data.limit()
    m.savedByteOrder = data.order()
    markStack.push(m)
    m
  }

  private def releaseUntilMark(mark: DataInputStream.Mark) = {
    // threadCheck()
    Assert.usage(!markStack.isEmpty)
    Assert.usage(mark != null)
    var current = markStack.pop
    while (!(markStack.isEmpty) && (current ne mark)) {
      markPool.returnToPool(current)
      current = markStack.pop
    }
    Assert.invariant(current eq mark) // holds unless markStack was empty
    current
  }

  def reset(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    st.assignFrom(current)
    data.position(st.savedBytePosition0b)
    data.limit(st.savedByteLimit0b)
    data.order(st.savedByteOrder)
    markPool.returnToPool(current)
    charIterator.reset() // this also resets the decoder.
    bitLimit0b = MaybeULong(calcBitLimit0b)
  }

  def discard(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    markPool.returnToPool(current)
  }

  override def markPos: MarkPos = bitPos0b
  override def resetPos(m: MarkPos) {
    setBitPos0b(m)
  }

  def validateFinalStreamState {
    // threadCheck()
    markPool.finalCheck
  }

  private def stringLengthInbits(nBytesConsumed: Long, nCharsTransferred: Long): Long = {
    // threadCheck()
    //
    // There are two ways length is determined.
    //
    if (st.maybeCharWidthInBits.isEmpty) {
      // we don't have a character width in bits
      // that would be because the encoding is variable width
      // like utf-8 or utf-16BE/LE with dfdl:utf16Width='variable', or shift-JIS
      //
      // In all these cases, we assume that the mandatory alignment is 8,
      // and the character codes are decoded/encoded to one or more 8-bit bytes.
      //
      // Hence, we can determine the number of bits consumed by way of the
      // number of bytes consumed.
      8 * nBytesConsumed
    } else {
      val charWidthInBits = st.maybeCharWidthInBits.get
      // we have a character width in bits, and it is fixed
      //
      // Hence, we can determine the number of bits consumed by way of
      // the number of characters
      charWidthInBits * nCharsTransferred
    }
  }

  def decodeIt(decoder: CharsetDecoder, cb: java.nio.CharBuffer): CoderResult = {
    var cr: CoderResult = null
    var nCharsTransferred: Int = 0
    var nBytesConsumed: Int = 0
    val cbRemainingBefore = cb.remaining() // must capture this before the surrogate pair screwing around.
    val bbRemainingBefore = data.remaining()

    Assert.usage(cbRemainingBefore > 0)

    cr = decoder.decode(data, cb, true)
    nCharsTransferred = cbRemainingBefore - cb.remaining()
    nBytesConsumed = bbRemainingBefore - data.remaining()
    //
    // Deal with utf-8 4-byte codepoints, that need room for 2 code units.
    //
    // This is a very obscure corner case. So let's waste minimal time.
    // by checking the cheapest thing to check first
    //
    if (cbRemainingBefore == 1 && nCharsTransferred == 0 && nBytesConsumed == 0 && cr.isOverflow()) {
      Assert.invariant(bbRemainingBefore >= 4)
      Assert.invariant(st.decoder.charset() == StandardCharsets.UTF_8)
      val firstByte = data.get(data.position())
      Assert.invariant(firstByte == 0xF0.toByte) // F0 means 3 more bytes for a total of 4
      //
      // What's painful is that we have to detect this because any time F0 appears in
      // bad data that is UTF-8, we don't get an error unless we have room for 2
      // characters in the char buffer.
      //
      // The most common case is that F0 means there's broken data.
      //
      val tempCB = CharBuffer.allocate(2)
      cr = decoder.decode(data, tempCB, true)
      val nDecoded = 2 - tempCB.remaining()
      //
      // At this point either we got an error (most likely), or we
      // didn't because the data really did use one of these obscure 4-byte
      // characters.
      //
      val firstChar = tempCB.get(0)
      if (nDecoded == 1 && firstChar == 0xFFFD.toChar) {
        Assert.invariant(!cr.isError)
        //
        // we got exactly 1 substitution character
        //
        cb.put(0xFFFD.toChar)
        st.resetUTF8SurrogatePairCapture
      } else if (nDecoded == 2 && firstChar == 0xFFFD.toChar) {
        //
        // We got two characters because the decode error got substituted, and
        // after that, another character got decoded.
        //
        // We want to back up to before that 2nd character
        //
        // We know that this situation can only arise if the substitution
        // occurred for a 4-byte UTF-8 character, and it had to be broken at the 4th byte
        //
        data.position(data.position - 1) // back up 1 byte.
        cb.put(0xFFFD.toChar)
        st.resetUTF8SurrogatePairCapture
      } else if (nDecoded == 2 && firstChar.isHighSurrogate) {
        Assert.invariant(!cr.isError)
        // surrogate pair case
        // note that the decode operation has advanced data by 4 bytes.
        Assert.invariant(bbRemainingBefore - 4 =#= data.remaining)
        val leadingSurrogate = firstChar
        val trailingSurrogate = tempCB.get(1)
        Assert.invariant(trailingSurrogate.isLowSurrogate)
        st.maybeTrailingSurrogateForUTF8 = MaybeChar(trailingSurrogate)
        st.priorBitPos = bitPos0b
        cb.put(leadingSurrogate)
      } else {
        Assert.invariant(cr.isError)
        // error case -
        st.resetUTF8SurrogatePairCapture
      }
    }

    cr
  }

  def fillCharBuffer(cb: java.nio.CharBuffer): MaybeULong = {
    // threadCheck()
    if (!align(st.encodingMandatoryAlignmentInBits)) return MaybeULong.Nope

    //
    // Corner case stuff for utf-8 and surrogate pairs.
    //
    if (st.decoder.charset() == StandardCharsets.UTF_8) {
      if (st.maybeTrailingSurrogateForUTF8.isDefined &&
        st.priorBitPos == bitPos0b &&
        st.priorEncoding == StandardCharsets.UTF_8) {
        // We're utf-8, the prior character was a leading surrogate,
        // we're at the same location we were when we produced the
        // leading surrogate. So produce the trailing.
        val trailingSurrogate = st.maybeTrailingSurrogateForUTF8.get
        cb.put(trailingSurrogate)
        st.resetUTF8SurrogatePairCapture
      }
    }

    val bitPos0bBefore = bitPos0b
    //
    // irrespective of whether the user wants REPLACE semantics
    // we need REPORT semantics here.
    //
    st.decoder.onMalformedInput(CodingErrorAction.REPORT)
    st.decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    var cr: CoderResult = null
    var nCharsTransferred: Int = 0
    var nBytesConsumed: Int = 0
    st.decoder.reset()
    val decoder = st.decoder match {
      case decoderWithBits: NonByteSizeCharsetDecoder => {
        decoderWithBits.setInitialBitOffset(st.bitOffset0b)
        decoderWithBits.setFinalByteBitLimitOffset0b(st.maybeBitLimitOffset0b)
        decoderWithBits
      }
      case _ => st.decoder
    }
    val cbRemainingBefore = cb.remaining()
    val bbRemainingBefore = data.remaining()
    cr = decodeIt(decoder, cb)
    nCharsTransferred = cbRemainingBefore - cb.remaining()
    nBytesConsumed = bbRemainingBefore - data.remaining()

    //
    // Note that while we have been advancing through the bytes in
    // the data byte buffer, that doesn't tell us how much
    // data has been consumed because we could have stopped
    // in the middle of a byte.
    //
    if (cr.isError) {
      //
      // The way we deal with decode errors here is very important
      // Suppose we are decoding along and start reaching some
      // binary data. That is likely to cause a decode error.
      //
      // Perhaps we are just pre-filling a buffer of characters
      // and thereafter we will test if it matches a regular expression.
      // Well, what if the match could have been successful
      // on just the data prior to the decode error, and can't be
      // lost based on yet more characters. In that case it would
      // be a shame to decode characters past the decode error, because
      // it would just be wasting time.
      //
      // So we have a soft-stop at a decode error. This method
      // will return as if it had not encountered the decode error,
      // unless there are no characters transferred at all. In that
      // case the decode error is happening on the very first character
      // of the decode call. In that case we generate a replacement character
      // and continue decoding. The caller has in essence asked for more
      // characters, presumably because they are needed for parsing.
      // If encodingErrorPolicy is 'replace' then we can provide more characters
      // but if encodingErrorPolicy is 'error' then we throw an exception.
      //
      if (nCharsTransferred == 0) {
        // we got an error on the very first character
        if (st.codingErrorAction == CodingErrorAction.REPORT)
          cr.throwException()
        else {
          Assert.invariant(st.codingErrorAction == CodingErrorAction.REPLACE)
          //
          // it's possible bytes were consumed. Rewind those.
          setBitPos0b(bitPos0bBefore)
          st.decoder.onMalformedInput(CodingErrorAction.REPLACE)
          st.decoder.onUnmappableCharacter(CodingErrorAction.REPLACE)

          val cbRemainingBefore = cb.remaining()
          val bbRemainingBefore = data.remaining()
          cr = decodeIt(decoder, cb)
          nCharsTransferred = cbRemainingBefore - cb.remaining()
          nBytesConsumed = bbRemainingBefore - data.remaining()
        }
      } else {
        // we got a decode error, but some characters were transferred before
        // the error. So we're going to return with what we have.
        // If the caller wants more characters, and they call this method again
        // then they'll get either the throw or a replacement character.
        cr = CoderResult.OVERFLOW // setting this gets us a "normal" return that provides characters back.
      }
    }
    if (cr.isUnderflow && nCharsTransferred == 0) {
      // couldn't decode even one character
      //
      // it's possible bytes were consumed. Rewind those.
      setBitPos0b(bitPos0bBefore)
      MaybeULong.Nope
    } else {
      Assert.invariant(nBytesConsumed == bbRemainingBefore - data.remaining())
      val lengthInBits = stringLengthInbits(nBytesConsumed, nCharsTransferred)
      setBitPos0b(bitPos0bBefore + lengthInBits)
      MaybeULong(nCharsTransferred)
    }
  }

  def skipChars(nChars: Long): Boolean = {
    // threadCheck()
    Assert.usage(nChars <= skipCharBuf.capacity())
    skipCharBuf.clear
    skipCharBuf.limit(nChars.toInt)
    var maybeN: MaybeULong = MaybeULong(0L)
    var total = 0L
    while (maybeN.isDefined && total < nChars) {
      maybeN = fillCharBuffer(skipCharBuf)
      if (maybeN.isDefined) {
        total += maybeN.get.toLong
      }
    }
    if (total < nChars) false
    else true
  }

  private def needMoreData(bufPos: Int): Boolean = {
    val existingLimit = regexMatchBuffer.limit
    if (bufPos < existingLimit) {
      // we didn't fill the buffer with data last time
      // we can just try again and fillCharBuffer may
      // deliver more.
      return false // we are not done.
    }
    val existingCapacity = regexMatchBuffer.capacity

    val isMatchDone =
      if (existingLimit < existingCapacity) {
        // can enlarge it in place by increasing the limit.
        regexMatchBuffer.position(existingLimit)
        st.adaptedRegexMatchBufferLimit = math.min(st.adaptedRegexMatchBufferLimit * 2, existingCapacity)
        // println("regex match buffer size increased to " + st.adaptedRegexMatchBufferLimit)
        regexMatchBuffer.limit(st.adaptedRegexMatchBufferLimit)
        false // not done, try the match again
      } else {
        Assert.invariant(existingLimit == existingCapacity)
        // need more but we can't enlarge it any more
        // match is done. It might be successful or might have failed to match
        // but either way we have a decision now.
        true // we are done
      }
    isMatchDone
  }

  def lookingAt(matcher: java.util.regex.Matcher, initialRegexMatchLimitInChars: Long = limits.defaultInitialRegexMatchLimitInChars): Boolean = {
    // threadCheck()
    val initialBitPos0b = bitPos0b
    var isMatchDone = false
    var isAMatch: Boolean = false
    st.adaptedRegexMatchBufferLimit = math.max(initialRegexMatchLimitInChars.toInt, st.adaptedRegexMatchBufferLimit)
    regexMatchBuffer.clear
    regexMatchBuffer.limit(st.adaptedRegexMatchBufferLimit)
    var positionAfterLastFill = regexMatchBuffer.position()
    var limitAfterLastFill = regexMatchBuffer.limit()
    while (!isMatchDone) {
      //
      // a new or resized buffer means we have to reset the matcher
      //
      // Unfortunately, there appears to be no way to restart a matcher
      // part way through a match of a regex. The match succeeds or fails.
      // Resuming matchers is for find() where you then proceed to find
      // another match, not for resuming part way through a match.
      //
      // This means the matcher will have to start from position 0 of the buffer again
      // however, we don't have to fillCharBuffer again starting from
      // position 0, we can add more characters to the enlarged buffer.
      // if we choose the initial capacity large enough, this won't happen often.
      //
      regexMatchBuffer.position(positionAfterLastFill)
      regexMatchBuffer.limit(math.max(limitAfterLastFill, regexMatchBuffer.limit))
      val ml = fillCharBuffer(regexMatchBuffer)
      val bufPosAfter = regexMatchBuffer.position()
      val limitAfter = regexMatchBuffer.limit()
      positionAfterLastFill = bufPosAfter
      limitAfterLastFill = limitAfter
      // back up so matcher can read from the start through what was added.
      regexMatchBuffer.position(0)
      regexMatchBuffer.limit(bufPosAfter)
      if (!ml.isDefined) {
        // no more data can be had.
        // so this iteration gets the final answer.
        isMatchDone = true // it will be done after this iteration.
      } else {
        matcher.reset(regexMatchBuffer)
        val isMatch = matcher.lookingAt()
        isAMatch = isMatch // redundant val here is so we can see it in debugger of eclipse
        val hitEnd = matcher.hitEnd()
        val requireEnd = matcher.requireEnd()
        isMatchDone =
          (isMatch, hitEnd, requireEnd) match {
            case (_, true, _) if !isMatchDone => needMoreData(bufPosAfter) // need more data to know if it will match or not
            case (true, _, true) if !isMatchDone => needMoreData(bufPosAfter) // match underway but might be invalidated by more data
            case (false, false, _) => true // there is no match, but more data can't help, so we are done
            case (true, false, false) => true // match, but not at end, so more data can't invalidate it.
            case _ => {
              //
              // Proof that this is an impossible case
              //           TTT case 1
              //           TTF case 1
              //           TFT case 2
              //           TFF case 4
              //           FTT case 1
              //           FTF case 1
              //           FFT case 3
              //           FFF case 3
              //
              Assert.impossibleCase()
            }
          }
      }
    }
    if (!isAMatch) {
      setBitPos0b(initialBitPos0b) // no match, then we don't move the position
    } else {
      //
      // now we have to figure out how long in bytes the match is.
      //
      val nCharsInMatch = matcher.group().length
      if (nCharsInMatch < st.adaptedRegexMatchBufferLimit) {
        val newLimit = math.max(nCharsInMatch, limits.defaultInitialRegexMatchLimitInChars.toInt)
        //        if (newLimit > limits.defaultInitialRegexMatchLimitInChars.toInt)
        //          println("regex match buffer size decreased to " + newLimit)
        st.adaptedRegexMatchBufferLimit = newLimit // set the prior value as our adapted length
      }
      val nBitsConsumed: Long =
        if (this.isFixedWidthEncoding) {
          // that means the characters are fixed width
          Assert.notYetImplemented(if (st.maybeUTF16Width.isDefined) st.maybeUTF16Width.get == UTF16Width.Variable else false)
          // TODO: utf16 width variable just changes the way we count chars slightly. Have to scan the string
          // for surrogate pairs, and subtract 1 from the length in chars for each pair found.
          // not bothering for now.
          val bitsPerChar = st.maybeCharWidthInBits.get
          nCharsInMatch * bitsPerChar
        } else {
          //
          // variable width encoding (e.g, like utf-8, shift-JIS)
          // We measure the length by re-executing the fillCharBuffer
          // but this time we know how many characters to stop it at,
          // (length of the match), and then we can compare bitPos
          // to find out how long that was in bits.
          //
          // We use a different char buffer for this, because the regexMatchBuffer
          // is currently holding things like the groups of the match, we don't want
          // to mess with its position.
          //
          lengthDeterminationBuffer.position(0)
          lengthDeterminationBuffer.limit(nCharsInMatch) // exactly this many chars
          setBitPos0b(initialBitPos0b)
          //
          // looping call to fill this lengthDeterminationBuffer is
          // needed because fillCharBuffer stops on decode errors even if it
          // will just replace them with the unicode replacement character on the next
          // call. That's an efficiency hack to enable matching to not scan ahead the full
          // size of the regex match buffer while just crashing into and substituting for tons
          // of what really is binary data. If the format forces it to do that it will,
          // but it first returns shy of having substituted unicode replacement characters
          // for decode errors so as to give things like lookingAt a chance to match against the data up to that
          // point, which might be successful, and thereby eliminate the need to
          // push forward into filling the buffer with unicode replacement characters corresponding
          // to decode errors that are only encountered because the regex match buffer is big.
          //
          // In other words, if hitEnd is false after an attempted match, then we don't need to
          // keep pushing ahead decoding (and substituting for errors) into the data.
          //
          val isFilled = fillCharBufferLoop(lengthDeterminationBuffer)
          Assert.invariant(isFilled)
          val nbits = bitPos0b - initialBitPos0b
          nbits
        }
      setBitPos0b(initialBitPos0b + nBitsConsumed)
    }
    isAMatch // if true, then the matcher contains details about the match.
  }

  private val charIterator = new BBDISCharIterator(st, this)
  def asIteratorChar: DataInputStream.CharIterator = charIterator

  def pastData(nBytesRequested: Int): ByteBuffer = {
    // threadCheck()
    if (!areDebugging)
      throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    val bb = data.duplicate()
    val posOffset = math.min(bytePos0b, nBytesRequested).toInt
    bb.limit(bb.position)
    bb.position(bytePos0b.toInt - posOffset)
    bb.asReadOnlyBuffer()
  }

  def futureData(nBytesRequested: Int): ByteBuffer = {
    // threadCheck()
    if (!areDebugging)
      throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    if (nBytesRequested == 0) return ByteBuffer.allocate(0)
    val bb = data.duplicate()
    if (bytePos0b + nBytesRequested < bb.limit()) {
      bb.limit(bb.position() + nBytesRequested)
    } else {
      // ok. The position and limit bb already has are good.
    }
    bb.asReadOnlyBuffer()
  }

}

private[io] class CharIteratorState {
  var cb = CharBuffer.allocate(2) // allow for 2 in case of surrogate pair
  var deltaBits: Int = 0
  var isFetched = false
  var bitPos0bAtLastFetch = 0L
  // any members added here must be added to assignFrom below.

  def assignFrom(other: CharIteratorState) {
    // threadCheck()
    this.cb = other.cb
    this.deltaBits = other.deltaBits
    this.isFetched = other.isFetched
    this.bitPos0bAtLastFetch = other.bitPos0bAtLastFetch
  }

  def clear() {
    // threadCheck()
    isFetched = false
    cb.clear()
    deltaBits = 0
  }
}

class BBDISCharIterator(st: MarkState, dis: ByteBufferDataInputStream)
  extends DataInputStream.CharIterator
  with ThreadCheckMixin {

  def reset() {
    st.charIteratorState.clear()
    st.decoder.reset()
  }

  /**
   * returns false if unable to fetch a character
   * returns true if it is able.
   * Stores number of bits consumed by the character
   * in deltaBits
   */
  private def fetch(): Boolean = {
    // threadCheck()
    val ist = st.charIteratorState
    val dataBitPosBefore0b = dis.bitPos0b
    if (!dis.align(st.encodingMandatoryAlignmentInBits)) return false
    ist.bitPos0bAtLastFetch = dis.bitPos0b // keep track of where we start trying to fetch a character
    ist.cb.clear()
    ist.cb.limit(1)
    st.decoder.reset()
    val maybeNumChars = dis.fillCharBuffer(ist.cb) // throws if a decode error and encodingErrorPolicy="error"
    if (maybeNumChars.isEmpty) {
      // Couldn't get one character
      ist.deltaBits = 0
      Assert.invariant(dis.bitPos0b =#= dataBitPosBefore0b)
      return false
    }
    // got 1 character
    val dataBitPosAfter0b = dis.bitPos0b
    ist.deltaBits = (dataBitPosAfter0b - dataBitPosBefore0b).toInt
    dis.setBitPos0b(dataBitPosBefore0b) // restore data position so we aren't advancing if called by hasNext
    true
  }

  def peek(): Char = {
    // threadCheck()
    if (!hasNext) return -1.toChar
    Assert.invariant(st.charIteratorState.isFetched)
    val c = st.charIteratorState.cb.get(0)
    c
  }

  def peek2(): Char = {
    // threadCheck()
    if (!hasNext) return -1.toChar
    val m = dis.markPos
    next()
    val c2 =
      if (!hasNext)
        -1.toChar
      else
        next()
    dis.resetPos(m)
    c2
  }

  override def hasNext: Boolean = {
    // threadCheck()
    val ist = st.charIteratorState
    if (ist.bitPos0bAtLastFetch != dis.bitPos0b) {
      // something moved the position between the last
      // call to hasNext, and this call to next.
      // (or between two calls to hasNext()
      // so we have to invalidate any character
      // cached by hasNext.
      ist.isFetched = false
    }
    if (!ist.isFetched) ist.isFetched = fetch()

    Assert.invariant(dis.bitPos0b <= dis.bitLimit0b.get)
    ist.isFetched
  }

  def next(): Char = {
    // threadCheck()
    val ist = st.charIteratorState
    if (ist.bitPos0bAtLastFetch != dis.bitPos0b) {
      // something moved the position between the last
      // call to hasNext, and this call to next.
      // (or between two calls to hasNext()
      // so we have to invalidate any character
      // cached by hasNext.
      ist.isFetched = false
    }
    if (!ist.isFetched) ist.isFetched = fetch()
    if (!ist.isFetched) throw new NoSuchElementException()
    // val dataBitPosBefore0b = dis.bitPos0b
    val c = ist.cb.get(0)
    ist.isFetched = false
    val newBitPos0b = dis.bitPos0b + ist.deltaBits
    dis.setBitPos0b(newBitPos0b)
    Assert.invariant(dis.bitPos0b <= dis.bitLimit0b.get)
    c
  }
}
