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

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.LongBuffer

import passera.unsigned.ULong

import org.apache.daffodil.equality._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.util.Bits
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.util.Pool

/**
 * Factory for creating this type of DataInputStream
 *
 * Provides only core input sources to avoid making any assumptions about the
 * incoming data (i.e. should a File be mapped to a ByteBuffer or be streamed
 * as an InputStream). The user knows better than us, so have them make the
 * decision.
 */
object InputSourceDataInputStream {

  def apply(byteArray: Array[Byte]): InputSourceDataInputStream = {
    apply(ByteBuffer.wrap(byteArray))
  }

  def apply(byteBuffer: ByteBuffer): InputSourceDataInputStream = {
    new InputSourceDataInputStream(new ByteBufferInputSource(byteBuffer))
  }

  def apply(in: InputStream): InputSourceDataInputStream = {
    new InputSourceDataInputStream(new BucketingInputSource(in))
  }
}

/**
 * The state that must be saved and restored by mark/reset calls
 */
final class MarkState()
  extends DataStreamCommonState with DataInputStream.Mark {

  override def equals(other: Any) = other match {
    case ar: AnyRef => this eq ar // only if the same object
    case _ => false
  }

  // any members added here must be added to assignFrom below.
  var bitPos0b: Long = 0
  var bitLimit0b: MaybeULong = MaybeULong.Nope
  val charIteratorState = new InputSourceDataInputStreamCharIteratorState

  @inline
  def bytePos0b: Long = bitPos0b >> 3

  def assignFrom(other: MarkState): Unit = {
    super.assignFrom(other)
    this.bitPos0b = other.bitPos0b
    this.bitLimit0b = other.bitLimit0b
    this.charIteratorState.assignFrom(other.charIteratorState)
  }

}

private[io] class MarkPool() extends Pool[MarkState] {
  override def allocate = new MarkState()
}

/**
 * Realization of the DataInputStream API
 *
 * Underlying representation is an InputSource containing all input data.
 */
final class InputSourceDataInputStream private (val inputSource: InputSource)
  extends DataInputStreamImplMixin {
  import DataInputStream._

  override def toString = {
    val bp0b = bitPos0b
    val bl0b = bitLimit0b
    val bl0b1 = if (bl0b.isDefined) bl0b.get.toString else "none"
    val str = "DataInputStream(bitPos=" + bp0b +
      ", bitLimit=" + bl0b1 + ")"
    str
  }

  override final val cst = new MarkState
  val markStack = new MStackOf[MarkState]
  val markPool = new MarkPool()

  @inline
  override final def bitPos0b: Long = cst.bitPos0b

  @inline
  override final def bitLimit0b: MaybeULong = cst.bitLimit0b

  def setBitPos0b(newBitPos0b: Long) {
    // threadCheck()
    Assert.invariant(newBitPos0b >= 0)
    Assert.invariant(bitLimit0b.isEmpty || newBitPos0b <= bitLimit0b.get)

    inputSource.position(newBitPos0b >> 3)
    cst.bitPos0b = newBitPos0b
  }

  override def setBitLimit0b(newBitLimit0b: MaybeULong): Boolean = {
    // threadCheck()
    Assert.invariant(newBitLimit0b.isEmpty || newBitLimit0b.get >= 0)
    if (bitLimit0b.isEmpty || newBitLimit0b.isEmpty || newBitLimit0b.get <= bitLimit0b.get) {
      cst.bitLimit0b = newBitLimit0b
      true
    } else {
      false
    }
  }

  def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit = {
    cst.bitLimit0b = savedBitLimit0b
  }

  def getByteArray(bitLengthFrom1: Int, finfo: FormatInfo): Array[Byte] = {
    // threadCheck()
    if (!isDefinedForLength(bitLengthFrom1)) throw DataInputStream.NotEnoughDataException(bitLengthFrom1)

    val arraySize = (bitLengthFrom1 + 7) / 8
    val array = new Array[Byte](arraySize)
    fillByteArray(array, bitLengthFrom1, finfo)

    setBitPos0b(bitPos0b + bitLengthFrom1)

    array
  }

  /**
   * Accepts a preallocated array and a bitLength. Reads the specified number
   * of bits and stores them in the array in big endian byte order and most
   * significant bit first bit order. The most significant byte is stored in
   * the zero'th index in the array. This means that if the array is larger
   * than the number of bytes needed for the specified number of bits, the
   * trailing bytes will be untouched and should be ignored by the caller.
   */
  private def fillByteArray(array: Array[Byte], bitLengthFrom1: Int, finfo: FormatInfo): Unit = {
    if (isAligned(8) && bitLengthFrom1 % 8 == 0) {
      fillByteArrayAlignedNoFragment(array, bitLengthFrom1, finfo)
    } else {
      fillByteArrayUnalignedOrFragment(array, bitLengthFrom1, finfo)
    }
  }

  private def fillByteArrayAlignedNoFragment(array: Array[Byte], bitLengthFrom1: Int, finfo: FormatInfo): Unit = {
    // threadCheck()
    Assert.usage(isAligned(8))
    Assert.usage(bitLengthFrom1 % 8 == 0)

    val bytesToFill = bitLengthFrom1 / 8
    Assert.invariant(array.size >= bytesToFill)

    if (bytesToFill == 1 || // 1 byte is super common case. We don't want to retrieve byteOrder nor bitOrder in this case
      (finfo.byteOrder == ByteOrder.BigEndian && finfo.bitOrder == BitOrder.MostSignificantBitFirst)) {
      // bits & bytes are already in order, read them straight into the array
      inputSource.get(array, 0, bytesToFill)
    } else {
      // we are either LittleEndian & MSBF or BigEndian & LSBF. In either case,
      // we just need to flip the bytes to make it BigEndian MSBF. The bits are
      // in the correct order.
      var i = bytesToFill - 1
      while (i >= 0) {
        array(i) = inputSource.get().toByte
        i -= 1
      }
    }
  }

  private def fillByteArrayUnalignedOrFragment(array: Array[Byte], bitLengthFrom1: Int, finfo: FormatInfo): Unit = {
    // threadCheck()
    Assert.usage(!isAligned(8) || bitLengthFrom1 % 8 != 0)

    val bytesToFill = (bitLengthFrom1 + 7) / 8
    val bitOffset0b = bitPos0b % 8
    Assert.invariant(array.size >= bytesToFill)
    val nFragmentBits = bitLengthFrom1 % 8

    var priorByte = Bits.asUnsignedByte(inputSource.get())
    var i = 0

    @inline // see comment below as to why this giant method is marked inline.
    def addFragmentByte() = {
      // This function is used at either the beginning or end of this function
      // to read a fragement byte and store it in the correct location in the
      // output array. It modifies variables outside of the function, such as
      // the array, array index (i), and potentialy the priorByte, thus making
      // it a closure. Note that it is a nested function so that it can access
      // those local variables. Normally, this would would lead to an
      // allocation which is bad in this cricial code. However, marking it as
      // @inline prevents that. So this allows for consolidating duplicate code
      // keeping the code somewhat clean, and allows access to local variables
      // without the allocaiton overhead of closures.
      val bitsLeftInPriorByte = 8 - bitOffset0b
      val fragmentByte =
        if (nFragmentBits <= bitsLeftInPriorByte) {
          // all nFragmentBits can come from from the priror byte
          val composedByte =
            finfo.bitOrder match {
              case BitOrder.MostSignificantBitFirst => ((priorByte << bitOffset0b) & 0xFF) >>> (8 - nFragmentBits)
              case BitOrder.LeastSignificantBitFirst => ((priorByte << (bitsLeftInPriorByte - nFragmentBits)) & 0xFF) >>> (8 - nFragmentBits)
            }
          composedByte
        } else {
          // we need all the fragment bits from prior plus some bits from cur
          val bitsToGetFromCur = nFragmentBits - bitsLeftInPriorByte
          val curByte = Bits.asUnsignedByte(inputSource.get)
          val composedByte =
            finfo.bitOrder match {
              case BitOrder.MostSignificantBitFirst => {
                val priorContribution = ((priorByte << bitOffset0b) & 0xFF) >>> (bitOffset0b - bitsToGetFromCur)
                val curContribution = curByte >>> (8 - bitsToGetFromCur)
                priorContribution | curContribution
              }
              case BitOrder.LeastSignificantBitFirst => {
                val priorContribution = (priorByte >>> bitOffset0b)
                val curContribution = ((curByte << (8 - bitsToGetFromCur)) & 0xFF) >>> (8 - (bitsToGetFromCur + bitsLeftInPriorByte))
                priorContribution | curContribution
              }
            }
          priorByte = curByte
          composedByte
        }
      if (finfo.byteOrder =:= ByteOrder.LittleEndian) {
        array(bytesToFill - 1 - i) = Bits.asSignedByte(fragmentByte)
      } else {
        array(i) = Bits.asSignedByte(fragmentByte)
      }
      i += 1
    }

    val newBitOffset0b =
      if (finfo.byteOrder =:= ByteOrder.BigEndian && nFragmentBits > 0) {
        addFragmentByte()
        (bitOffset0b + nFragmentBits) % 8
      } else {
        bitOffset0b
      }

    if (newBitOffset0b == 0) {
      finfo.byteOrder match {
        case ByteOrder.BigEndian => {
          // we just parsed a bigEndian fragment byte and it put us back on a byte
          // boundary, so we just need to read the rest of the full aligned bytes
          inputSource.get(array, 1, bytesToFill - 1)
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
            priorByte = Bits.asUnsignedByte(inputSource.get())
            i += 1
          }
        }
      }
    } else {
      val priorShift = newBitOffset0b
      val curShift = 8 - priorShift

      // If the bitOrder is BE, then we have already consumed a fragment byte
      // above, so we want to fill in the rest of the array, so fill in the
      // whole array (i.e. stop at the array size). If the byteOrder is LE,
      // then we only want to fill the number of full bytes, and then we'll
      // consume that last fragment byte afterwards
      val stopBytePosition =
        if (finfo.byteOrder =:= ByteOrder.BigEndian) bytesToFill
        else (bitLengthFrom1 / 8)

      // consume full bytes
      while (i < stopBytePosition) {
        val curByte = Bits.asUnsignedByte(inputSource.get())
        val composedByte =
          finfo.bitOrder match {
            case BitOrder.MostSignificantBitFirst => ((priorByte << priorShift) & 0xFF) | ((curByte >>> curShift) & 0xFF)
            case BitOrder.LeastSignificantBitFirst => ((priorByte >>> priorShift) & 0xFF) | ((curByte << curShift) & 0xFF)
          }
        if (finfo.byteOrder =:= ByteOrder.LittleEndian) {
          array(bytesToFill - 1 - i) = Bits.asSignedByte(composedByte)
        } else {
          array(i) = Bits.asSignedByte(composedByte)
        }
        priorByte = curByte
        i += 1
      }
    }

    if (finfo.byteOrder =:= ByteOrder.LittleEndian && nFragmentBits > 0) {
      addFragmentByte()
    }
  }

  def getBinaryDouble(finfo: FormatInfo): Double = {
    val l = getSignedLong(64, finfo)
    val d = java.lang.Double.longBitsToDouble(l)
    d
  }

  def getBinaryFloat(finfo: FormatInfo): Float = {
    val i = getSignedLong(32, finfo).toInt
    val f = java.lang.Float.intBitsToFloat(i)
    f
  }

  def getSignedLong(bitLengthFrom1To64: Int, finfo: FormatInfo): Long = {
    // threadCheck()
    Assert.usage(bitLengthFrom1To64 >= 1)
    Assert.usage(bitLengthFrom1To64 <= 64)

    val res = getUnsignedLong(bitLengthFrom1To64, finfo)
    Bits.signExtend(res.longValue, bitLengthFrom1To64)
  }

  // used by the below function to get up to 8 bytes of data
  private val longArray = new Array[Byte](8)

  def getUnsignedLong(bitLengthFrom1To64: Int, finfo: FormatInfo): ULong = {
    Assert.usage(bitLengthFrom1To64 >= 1)
    Assert.usage(bitLengthFrom1To64 <= 64)

    if (!isDefinedForLength(bitLengthFrom1To64)) throw DataInputStream.NotEnoughDataException(bitLengthFrom1To64)

    val numBytes = (bitLengthFrom1To64 + 7) / 8

    // will result in the first numBytes the long array filled in
    fillByteArray(longArray, bitLengthFrom1To64, finfo)

    var res = Bits.asUnsignedByte(longArray(0)).toLong
    var i = 1
    while (i < numBytes) {
      res = res << 8
      res = res | Bits.asUnsignedByte(longArray(i))
      i += 1
    }

    setBitPos0b(bitPos0b + bitLengthFrom1To64)

    ULong(res)
  }

  def getSignedBigInt(bitLengthFrom1: Int, finfo: FormatInfo): BigInt = {
    // threadCheck()
    Assert.usage(bitLengthFrom1 >= 1)

    if (bitLengthFrom1 <= 64) {
      BigInt(getSignedLong(bitLengthFrom1, finfo))
    } else {
      val allBytes = getByteArray(bitLengthFrom1, finfo)
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

  def getUnsignedBigInt(bitLengthFrom1: Int, finfo: FormatInfo): BigInt = {
    Assert.usage(bitLengthFrom1 >= 1)
    val bytes = getByteArray(bitLengthFrom1, finfo)
    BigInt(1, bytes)
  }

  /**
   * Determines whether the input stream has this much more data.
   *
   * Does not advance the position
   */
  final def isDefinedForLength(nBits: Long): Boolean = {
    val newBitPos0b = bitPos0b + nBits
    if (bitLimit0b.isDefined && newBitPos0b > bitLimit0b.get) false
    else {
      val newEndingBytePos0b = Bits.roundUpBitToBytePosition(newBitPos0b)
      val moreBytesNeeded = newEndingBytePos0b - inputSource.position()
      inputSource.areBytesAvailable(moreBytesNeeded)
    }
  }

  def skip(nBits: Long, finfo: FormatInfo): Boolean = {
    // threadCheck()
    Assert.usage(nBits <= Int.MaxValue)
    if (!this.isDefinedForLength(nBits)) return false
    setBitPos0b(bitPos0b + nBits)
    true
  }

  def mark(requestorID: String): DataInputStream.Mark = {
    val m = markPool.getFromPool(requestorID)
    m.assignFrom(cst)
    markStack.push(m)
    inputSource.lockPosition(m.bytePos0b)
    m
  }

  private def releaseUntilMark(mark: DataInputStream.Mark) = {
    // threadCheck()
    Assert.usage(!markStack.isEmpty)
    Assert.usage(mark != null)
    var current = markStack.pop
    while (!(markStack.isEmpty) && (current ne mark)) {
      inputSource.releasePosition(current.bytePos0b)
      markPool.returnToPool(current)
      current = markStack.pop
    }
    Assert.invariant(current eq mark) // holds unless markStack was empty
    current
  }

  def reset(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    cst.assignFrom(current)
    setBitPos0b(cst.bitPos0b)
    inputSource.releasePosition(current.bytePos0b)
    markPool.returnToPool(current)
  }

  def discard(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    inputSource.releasePosition(current.bytePos0b)
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

  final def getString(nChars: Long, finfo: FormatInfo): Maybe[String] = {
    val startingBitPos = bitPos0b
    val aligned = align(finfo.encodingMandatoryAlignmentInBits, finfo)
    if (!aligned) {
      Maybe.Nope
    } else {
      withLocalCharBuffer { lcb =>
        val cb = lcb.getBuf(nChars)
        val numDecoded = finfo.decoder.decode(this, finfo, cb)
        if (numDecoded == nChars) {
          Maybe(cb.flip.toString)
        } else {
          setBitPos0b(startingBitPos)
          Maybe.Nope
        }
      }
    }
  }

  final def getSomeString(nChars: Long, finfo: FormatInfo): Maybe[String] = {
    val startingBitPos = bitPos0b
    val aligned = align(finfo.encodingMandatoryAlignmentInBits, finfo)
    if (!aligned) {
      Maybe.Nope
    } else {
      withLocalCharBuffer { lcb =>
        val cb = lcb.getBuf(nChars)
        val numDecoded = finfo.decoder.decode(this, finfo, cb)
        if (numDecoded > 0) {
          Maybe(cb.flip.toString)
        } else {
          setBitPos0b(startingBitPos)
          Maybe.Nope
        }
      }
    }
  }

  lazy val skipCharBuffer = CharBuffer.allocate(32)

  def skipChars(nChars: Long, finfo: FormatInfo): Boolean = {
    // threadCheck()
    val startingBitPos = bitPos0b

    val aligned = align(finfo.encodingMandatoryAlignmentInBits, finfo)
    if (!aligned) {
      false
    } else {
      var remainingCharsToSkip = nChars
      var keepGoing = true

      while (keepGoing && remainingCharsToSkip > 0) {
        val charsToSkip = Math.min(remainingCharsToSkip, skipCharBuffer.capacity)
        skipCharBuffer.position(0)
        skipCharBuffer.limit(charsToSkip.toInt)

        val numDecoded = finfo.decoder.decode(this, finfo, skipCharBuffer)
        remainingCharsToSkip -= numDecoded

        if (numDecoded == 0) {
          keepGoing = false
        }
      }

      val skippedAllNChars = remainingCharsToSkip == 0

      if (!skippedAllNChars) {
        // failed to skip all the necessary chars, reset the bit position back to
        // where we started
        setBitPos0b(startingBitPos)
      }

      skippedAllNChars
    }
  }

  def lookingAt(matcher: java.util.regex.Matcher, finfo: FormatInfo): Boolean = {
    val aligned = align(finfo.encodingMandatoryAlignmentInBits, finfo)
    if (!aligned) {
      false
    } else {
      var regexMatchBufferLimit = finfo.tunable.initialRegexMatchLimitInCharacters
      val regexMatchBuffer = finfo.regexMatchBuffer
      val regexMatchBitPositionBuffer = finfo.regexMatchBitPositionBuffer

      regexMatchBuffer.position(0)
      regexMatchBuffer.limit(0)
      regexMatchBitPositionBuffer.position(0)
      regexMatchBitPositionBuffer.limit(0)

      val startingBitPos = bitPos0b
      var keepMatching = true
      var isMatch = false

      while (keepMatching) {
        // set the position to the last place data stopped decoding and increase
        // the limit so we can fill more data
        regexMatchBuffer.position(regexMatchBuffer.limit)
        regexMatchBuffer.limit(regexMatchBufferLimit)
        regexMatchBitPositionBuffer.position(regexMatchBitPositionBuffer.limit)
        regexMatchBitPositionBuffer.limit(regexMatchBufferLimit)

        val numDecoded = finfo.decoder.decode(this, finfo, regexMatchBuffer, regexMatchBitPositionBuffer)
        val filledToLimit = regexMatchBuffer.position == regexMatchBuffer.limit
        regexMatchBuffer.flip
        regexMatchBitPositionBuffer.flip

        if (numDecoded > 0) {
          // we decoded at least one extra characer than we had before, so the
          // match results could have changed. Try again.
          matcher.reset(regexMatchBuffer)
          isMatch = matcher.lookingAt()
          val hitEnd = matcher.hitEnd
          val requireEnd = matcher.requireEnd

          if (filledToLimit && ((isMatch && requireEnd) || (!isMatch && hitEnd))) {
            // We filled the CharBuffer to its limit, so it's possible there
            // is more data available. AND either 1) we got a match but more
            // data could negate it or 2) we hit the end without getting a
            // match but more data might result in a match. In either case,
            // let's increase the match limit if possible, try to decode more
            // data, and try the match again if we got more data.
            if (regexMatchBufferLimit == regexMatchBuffer.capacity) {
              // consumed too much data, just give up
              keepMatching = false
            } else {
              regexMatchBufferLimit = Math.min(regexMatchBufferLimit * 2, regexMatchBuffer.capacity)
            }
          } else {
            // no more data could affect the match result, so exit the loop and
            // figure out the match result
            keepMatching = false
          }
        } else {
          // We failed to decode any data, that means that there is no more
          // data to match against. Either we've already done a match or more
          // data could have changed the outcome, or there was never any data
          // to match in the first place. In either case, the state of isMatch
          // is correct--we are done.
          keepMatching = false
        }
      }

      if (isMatch && matcher.end != 0) {
        // got a non-zero length match, set the bit position to the end of the
        // match, note that the end match position is the index *after* the
        // last match, so we need the ending bitPosition of the previous
        // character
        val endingBitPos = regexMatchBitPositionBuffer.get(matcher.end - 1)
        setBitPos0b(endingBitPos)
      } else {
        // failed to match, set the bit position back to where we started
        setBitPos0b(startingBitPos)
      }

      isMatch
    }
  }


  private val charIterator = new InputSourceDataInputStreamCharIterator(this)
  def asIteratorChar: CharIterator = {
    val ci = charIterator
    ci.reset()
    ci
  }

  override def setDebugging(setting: Boolean) {
    super.setDebugging(setting)
    inputSource.setDebugging(setting)
  }

  def pastData(nBytesRequested: Int): ByteBuffer = {
    // threadCheck()
    if (!areDebugging)
      throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    if (nBytesRequested == 0) return ByteBuffer.allocate(0).asReadOnlyBuffer()

    val savedBytePosition = inputSource.position
    val bytesToRead = Math.min(savedBytePosition, nBytesRequested).toInt
    val newBytePosition = savedBytePosition - bytesToRead
    inputSource.position(newBytePosition)

    val array = new Array[Byte](bytesToRead)
    inputSource.get(array, 0, bytesToRead)

    inputSource.position(savedBytePosition)
    ByteBuffer.wrap(array).asReadOnlyBuffer()
  }

  def futureData(nBytesRequested: Int): ByteBuffer = {
    // threadCheck()
    if (!areDebugging)
       throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)

    if (nBytesRequested == 0) return ByteBuffer.allocate(0).asReadOnlyBuffer()

    val savedBytePosition = inputSource.position
    // need to call areBytesAvailable first to ensure at least length bytes are
    // buffered if they exist
    val available = inputSource.areBytesAvailable(nBytesRequested)
    val bytesToRead = if (available) nBytesRequested else inputSource.bytesAvailable.toInt
    val array = new Array[Byte](bytesToRead)
    inputSource.get(array, 0, bytesToRead)

    inputSource.position(savedBytePosition)
    ByteBuffer.wrap(array).asReadOnlyBuffer()
  }

}

class InputSourceDataInputStreamCharIteratorState  {
  var bitPositionAtLastFetch0b = 0L

  // CharBuffer and LongBuffer for some amount of cache lookahead
  var decodedChars = CharBuffer.allocate(8)
  var bitPositions = LongBuffer.allocate(8)

  var moreDataAvailable: Boolean = true

  def clear(): Unit = {
    // Set the Buffers for reading with zero data. The iterator will see there
    // is no cachd decoded chars to read and perform the appropriate actions to
    // fill it in
    decodedChars.position(0).limit(0)
    bitPositions.position(0).limit(0)
    moreDataAvailable = true
    bitPositionAtLastFetch0b = 0L
  }

  def assignFrom(other: InputSourceDataInputStreamCharIteratorState) {
    // We are intentionally not saving/restoring any state here. This is
    // because saving state requires duplicating the long and charbuffers,
    // which is fairly expensive and can use up alot of memory, especially when
    // there are lots of points of uncertainties. Instead, the
    // checkNeedsRefetch method will determine if something changed the
    // bitPosition (e.g. resetting a mark), and just clear all the internal
    // state, leading to a redecode of data. This does mean that if we reset
    // back to a mark we will repeat work that has already been done. But that
    // should be relatively fast, avoids memory usage, and only takes a penalty
    // when backtracking rather than every time there is a PoU.
  }
}

class InputSourceDataInputStreamCharIterator(dis: InputSourceDataInputStream) extends DataInputStream.CharIterator {

  private var finfo : FormatInfo = null

  override def reset(): Unit = {
    dis.cst.charIteratorState.clear()
    if (finfo != null) {
      finfo.decoder.reset()
      finfo = null
    }
  }

  override def setFormatInfo(_finfo: FormatInfo): Unit = {
    finfo = _finfo
  }

  @inline
  private def checkNeedsRefetch(): Unit = {
    val cis = dis.cst.charIteratorState
    if (cis.bitPositionAtLastFetch0b != dis.bitPos0b) {
      // Something outside of the char iterator  moved the bit position since
      // the last time we fetched data (e.g. backtracking to a previous mark,
      // in which we do not save/restore this iterator state data). In this
      // case, all of our cached decoded data is wrong. So lets clear the
      // iterator state and reset the last fetch state so that more data will
      // be fetched
      dis.cst.charIteratorState.clear()
      dis.cst.charIteratorState.bitPositionAtLastFetch0b = dis.bitPos0b
    }
  }

  private def fetch(): Unit = {
    val cis = dis.cst.charIteratorState
    if (cis.moreDataAvailable) {
      val startingBitPos = dis.bitPos0b

      if (!dis.align(finfo.encodingMandatoryAlignmentInBits, finfo)) {
        // failed to align, which means there must be no more data
        cis.moreDataAvailable = false
      } else {
        // Need more data--the previous call to deocde only stopped
        // because we ran out of space in our CharBuffer, so there should
        // be more data to get and we need to make space. There might
        // still be some data in the Buffers that haven't been read even
        // though we need more data (e.g. because of peek2), so compact() the
        // buffers, which makes more space and flips for putting, then call
        // decode.
        val decodedChars = cis.decodedChars
        val bitPositions = cis.bitPositions

        // we need to pick up decoding where we last left off if there are any
        // characters still cached. This location is found in the last element
        // of the bitPositions buffer
        if (bitPositions.limit > 0) {
          val lastFetchBitPositionEnd = bitPositions.get(bitPositions.limit - 1)
          dis.setBitPos0b(lastFetchBitPositionEnd)
        }

        decodedChars.compact()
        bitPositions.compact()

        val numDecoded = finfo.decoder.decode(dis, finfo, decodedChars, bitPositions)

        // flip for reading
        decodedChars.flip()
        bitPositions.flip()

        if (numDecoded == 0) {
          // ran out of data. If we try to consume more than is currently
          // available in decodedChars, it's an error
          cis.moreDataAvailable = false
        }

        // we have aligned and decoded characters which moves the bit position
        // ahead, so reset the bit position to before we started the decode.
        // We'll only update the bit position when characters are actually read
        // when next() is called
        dis.setBitPos0b(startingBitPos)
      }
    } else {
      // do nothing, no more data to get
    }
  }

  def peek(): Char = {
    checkNeedsRefetch()

    val decodedChars = dis.cst.charIteratorState.decodedChars
    if (decodedChars.remaining >= 1) {
      decodedChars.get(decodedChars.position)
    } else if (dis.cst.charIteratorState.moreDataAvailable) {
      fetch()
      peek()
    } else {
      -1.toChar
    }
  }

  def peek2(): Char = {
    checkNeedsRefetch()

    val decodedChars = dis.cst.charIteratorState.decodedChars
    if (decodedChars.remaining >= 2) {
      decodedChars.get(decodedChars.position + 1)
    } else if (dis.cst.charIteratorState.moreDataAvailable) {
      fetch()
      peek2()
    } else {
      -1.toChar
    }
  }

  override def hasNext: Boolean = {
    peek() != -1.toChar
  }

  def next(): Char = {
    Assert.invariant(hasNext)

    val cis = dis.cst.charIteratorState
    val char = cis.decodedChars.get()
    val pos = cis.bitPositions.get()
    dis.setBitPos0b(pos)
    cis.bitPositionAtLastFetch0b = pos
    char
  }
}
