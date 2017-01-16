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

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
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
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.util.LogLevel

sealed trait DOSState
private[io] case object Active extends DOSState
private[io] case object Finished extends DOSState
private[io] case object Uninitialized extends DOSState

trait DataOutputStreamImplMixin extends DataStreamCommonState
  with DataOutputStream
  with DataStreamCommonImplMixin
  with LocalBufferMixin {

  def defaultCodingErrorAction: CodingErrorAction = CodingErrorAction.REPLACE

  private val initialEncoder = {
    val enc = StandardCharsets.UTF_8.newEncoder
    enc.onMalformedInput(defaultCodingErrorAction)
    enc.onUnmappableCharacter(defaultCodingErrorAction)
    enc
  }

  private var fillByte_ : Int = 0
  def fillByte = fillByte_

  /**
   * The fillLong is a long that has been pre-filled with the fill byte.
   *
   * If the fillByte changes, then the fillLong is updated to track it.
   */
  private var fillLong_ : Long = 0L
  def fillLong = fillLong_

  def setFillByte(newFillByte: Int) {
    Assert.usage(isWritable)
    Assert.usage(newFillByte <= 255 && newFillByte >= 0)
    slamFillByte(newFillByte)
  }

  /**
   * slamFillByte assigns the fill byte but doesn't do the invariant checking.
   * It is for use from contexts where you need to set the fill byte, but the
   * usual invariants aren't expected to hold.
   */
  def slamFillByte(newFillByte: Int): Unit = {
    fillByte_ = newFillByte
    fillLong_ = {
      if (fillByte_ == 0) 0L
      else {
        var fl: Long = 0L
        var i = 0
        while (i < 8) {
          fl = fl << 8
          fl = fl | fillByte_
          i += 1
        }
        fl
      }
    }
  }

  protected final var encoder_ : CharsetEncoder = initialEncoder

  protected final var codingErrorAction: CodingErrorAction = defaultCodingErrorAction

  /**
   * Relative bit position zero based.
   *
   * Relative to the start of the current buffer.
   */
  private var relBitPos0b_ : ULong = ULong(0)

  /**
   * Once we determine what it is, this will hold the absolute bit pos
   * of the first bit of this buffer.
   *
   * It should always be the case that the absbitPos0b is the sum of
   * this value and the relBitPos0b.
   */

  private var maybeAbsStartingBitPos0b_ : MaybeULong = MaybeULong.Nope

  private def checkInvariants() {
    // nothing right now
  }

  /**
   * Absolute bit position zero based.
   * Absolute as in relative the the start of the output data stream.
   *
   * This is a Maybe type because we might not know this value, but still need
   * to do unparsing into a buffer.
   */
  def maybeAbsBitPos0b: MaybeULong = {
    val mas = maybeAbsStartingBitPos0b
    if (mas.isDefined) {
      val st = mas.get
      val rel = this.relBitPos0b.longValue
      val abs = st + rel
      MaybeULong(abs)
    } else
      MaybeULong.Nope
  }

  /**
   * the starting bit pos is undefined to start.
   * then it is in many cases set to some value n, because we
   * know in advance how long the prior data is.
   * Then when absorbed into the direct stream, the
   * stream technically starts at 0, but we need to keep track
   * of the former starting bit pos so as to be able to
   * convert relative positions to absolute positions correctly.
   */
  protected final def maybeAbsStartingBitPos0b = {
    if (this.maybeAbsolutizedRelativeStartingBitPosInBits_.isDefined)
      maybeAbsolutizedRelativeStartingBitPosInBits_
    else
      maybeAbsStartingBitPos0b_
  }

  final def toAbsolute(relBitPos0b: ULong) = {
    Assert.usage(maybeAbsStartingBitPos0b.isDefined)
    maybeAbsStartingBitPos0b.getULong + relBitPos0b
  }
  def resetAllBitPos() {
    this.maybeAbsolutizedRelativeStartingBitPosInBits_ = MaybeULong.Nope
    maybeAbsStartingBitPos0b_ = MaybeULong.Nope
    relBitPos0b_ = ULong(0)
  }

  def setAbsStartingBitPos0b(newStartingBitPos0b: ULong) {
    checkInvariants()
    val mv = MaybeULong(newStartingBitPos0b.longValue)
    //
    // there are 3 states
    //
    if (this.maybeAbsStartingBitPos0b_.isEmpty &&
      this.maybeAbsolutizedRelativeStartingBitPosInBits_.isEmpty) {
      this.maybeAbsStartingBitPos0b_ = mv
    } else if (this.maybeAbsStartingBitPos0b_.isDefined) {
      this.maybeAbsolutizedRelativeStartingBitPosInBits_ =
        this.maybeAbsStartingBitPos0b_
      this.maybeAbsStartingBitPos0b_ = mv
    } else {
      // both are defined
      Assert.usageError("You cannot set the abs starting bit pos again.")
    }
    checkInvariants()
  }

  protected final def setRelBitPos0b(newRelBitPos0b: ULong) {
    Assert.usage(isWritable)
    checkInvariants()
    relBitPos0b_ = newRelBitPos0b
    checkInvariants()
  }

  def relBitPos0b = {
    relBitPos0b_
  }

  /**
   * Once a stream has been made direct, this contains the
   * absolute bit pos corresponding to the starting of this
   * stream when it was buffering. That is, it is the
   * starting position (which was zero), converted to the
   * absolute starting position.
   *
   * This differs from maybeAbsStartingPos0b, because that
   * is modified once a stream becomes direct. This
   * doesn't change.
   *
   * This value allows stored relative offsets (stored as the start/ends
   * of content and value lenght regions) to still be meaningful even though
   * we have collapsed the stream into a direct one.
   */
  private var maybeAbsolutizedRelativeStartingBitPosInBits_ = MaybeULong.Nope

  /**
   * Absolute bit limit zero based
   *
   * If defined it is the position 1 bit past the last bit location that can be written.
   * So if we at starting at bitPos0b of 0, and we allow only 100 bits, then the bit positions are
   * 0 to 99, and the bit limit is 100.
   */
  var maybeAbsBitLimit0b: MaybeULong = MaybeULong.Nope

  /**
   * Relative bit limit zero based
   */
  private var maybeRelBitLimit0b_ : MaybeULong = MaybeULong.Nope
  def maybeRelBitLimit0b = maybeRelBitLimit0b_

  /**
   * sets, but also maintains the absolute bit limit, if that is defined.
   *
   * Returns false if the set was unsuccessful, meaning one is setting a limit that
   * extends past a pre-existing limit.
   */
  protected def setMaybeRelBitLimit0b(newMaybeRelBitLimit0b: MaybeULong, reset: Boolean = false): Boolean = {
    Assert.invariant((maybeAbsBitLimit0b.isDefined && maybeRelBitLimit0b.isDefined) || maybeAbsBitLimit0b.isEmpty)
    if (newMaybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = MaybeULong.Nope
      maybeAbsBitLimit0b = MaybeULong.Nope
    } else if (maybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = newMaybeRelBitLimit0b
      // absolute limit doesn't getchanged (it must be undefined)
      Assert.invariant(maybeAbsBitLimit0b.isEmpty)
    } else {
      val delta = maybeRelBitLimit0b.get - newMaybeRelBitLimit0b.get // this is how much the relative value is changing.
      if (!reset && delta < 0) {
        // we're trying to lengthen past an existing limit. This isn't allowed, unless we are forcing a reset
        return false
      }
      maybeRelBitLimit0b_ = MaybeULong(maybeRelBitLimit0b.get - delta)
      if (maybeAbsBitLimit0b.isDefined) {
        // adjust absolute value by the same amount.
        maybeAbsBitLimit0b = MaybeULong(maybeAbsBitLimit0b.get - delta)
      }
    }
    true // in all other cases, we return true because we're successful.
  }

  /**
   * Offset within the first byte to the first bit. This is the number of bits to skip.
   * Value is from 0 to 7.
   */
  var bitStartOffset0b: Int = 0

  final def remainingBits: MaybeULong = {
    if (maybeRelBitLimit0b.isEmpty) MaybeULong.Nope else MaybeULong(maybeRelBitLimit0b.get - relBitPos0b.toLong)
  }

  private var byteOrder_ : ByteOrder = ByteOrder.BigEndian

  def setByteOrder(byteOrder: ByteOrder): Unit = {
    Assert.usage(isWritable)
    byteOrder_ = byteOrder
  }

  def byteOrder: ByteOrder = {
    Assert.usage(isReadable)
    byteOrder_
  }

  var debugOutputStream: Maybe[ByteArrayOutputStream] = Nope

  /**
   * When there is a partial byte on the end that is also to be
   * considered part of the output, but as a partial byte only it
   * can't be written to the underlying java.io.OutputStream, that
   * partial byte is held here.
   */
  private var fragmentLastByte_ : Int = 0
  def fragmentLastByte = fragmentLastByte_

  /**
   * Within the fragmentLastByte, how many bits are in use.
   *
   * Always between 0 and 7 inclusive.
   */
  private var fragmentLastByteLimit_ : Int = 0
  def fragmentLastByteLimit = fragmentLastByteLimit_

  def setFragmentLastByte(newFragmentByte: Int, nBitsInUse: Int) {
    Assert.usage(nBitsInUse >= 0 && nBitsInUse <= 7)
    Assert.usage(newFragmentByte >= 0 && newFragmentByte <= 255) // no bits above first byte are in use.
    if (nBitsInUse == 0) {
      Assert.usage(newFragmentByte == 0)
    }
    fragmentLastByte_ = newFragmentByte
    fragmentLastByteLimit_ = nBitsInUse
  }

  def isEndOnByteBoundary = fragmentLastByteLimit == 0

  private var _dosState: DOSState = Active // active when new.

  @inline final def dosState = _dosState

  @inline protected final def setDOSState(newState: DOSState) { _dosState = newState }

  private[io] def isBuffering: Boolean

  @inline private[io] final def isDirect = !isBuffering

  @inline private[io] final def isDead = { _dosState =:= Uninitialized }
  @inline override final def isFinished = { _dosState =:= Finished }
  @inline override def setFinished() { _dosState = Finished }
  @inline private[io] final def isActive = { _dosState =:= Active }
  @inline private[io] final def isReadOnly = { isFinished && isBuffering }
  @inline private[io] final def isWritable = {
    isActive ||
      // This can happen if merging streams A and B and C, where A, the direct stream) is being setFinished.
      // if B is finished, then the result of merging A into B is a finished stream (now direct), but we still
      // want to keep merging B into C, and that involves calling this putBytes call to merge C's data into B's stream.
      // Hence, the stream could be writable or finished.
      (isFinished && isDirect)
  }
  @inline private[io] final def isReadable = { !isDead }

  final def encoder = {
    Assert.usage(isReadable)
    encoder_
  }

  final def setEncoder(encoder: CharsetEncoder): Unit = {
    Assert.usage(isWritable)
    if (this.encoder_ == encoder) return
    encoder_ = encoder
    encoder.onMalformedInput(codingErrorAction)
    encoder.onUnmappableCharacter(codingErrorAction)
    val cs = encoder.charset()

    if (cs == StandardCharsets.UTF_16 || cs == StandardCharsets.UTF_16BE || cs == StandardCharsets.UTF_16LE) {
      if (maybeUTF16Width.isDefined && maybeUTF16Width.get == UTF16Width.Fixed) {
        maybeCharWidthInBits = MaybeInt(16)
        encodingMandatoryAlignmentInBits = 8
      } else {
        maybeCharWidthInBits = MaybeInt.Nope
        encodingMandatoryAlignmentInBits = 8
      }
    } else {
      cs match {
        case encoderWithBits: NonByteSizeCharset => {
          maybeCharWidthInBits = MaybeInt(encoderWithBits.bitWidthOfACodeUnit)
          encodingMandatoryAlignmentInBits = 1
        }
        case _ => {
          val maxBytes = encoder.maxBytesPerChar()
          if (maxBytes == encoder.averageBytesPerChar()) {
            maybeCharWidthInBits = MaybeInt((maxBytes * 8).toInt)
            encodingMandatoryAlignmentInBits = 8
          } else {
            maybeCharWidthInBits = MaybeInt.Nope
            encodingMandatoryAlignmentInBits = 8
          }
        }
      }
    }
  }

  final def encodingErrorPolicy = {
    Assert.usage(isReadable)
    codingErrorAction match {
      case CodingErrorAction.REPLACE => EncodingErrorPolicy.Replace
      case CodingErrorAction.REPORT => EncodingErrorPolicy.Error
    }
  }

  final def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = {
    Assert.usage(isWritable)
    codingErrorAction = eep match {
      case EncodingErrorPolicy.Replace => CodingErrorAction.REPLACE
      case EncodingErrorPolicy.Error => CodingErrorAction.REPORT
    }
    encoder.onMalformedInput(codingErrorAction)
    encoder.onUnmappableCharacter(codingErrorAction)
    ()
  }

  protected def setJavaOutputStream(newOutputStream: java.io.OutputStream): Unit

  protected def getJavaOutputStream(): java.io.OutputStream

  final protected def cst = this

  protected def assignFrom(other: DataOutputStreamImplMixin) {
    Assert.usage(isWritable)
    super.assignFrom(other)
    this.fillByte_ = other.fillByte_
    this.fillLong_ = other.fillLong_
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.encoder = other.encoder
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.codingErrorAction = other.codingErrorAction
    this.maybeAbsStartingBitPos0b_ = other.maybeAbsStartingBitPos0b_
    this.maybeAbsolutizedRelativeStartingBitPosInBits_ = other.maybeAbsolutizedRelativeStartingBitPosInBits_
    this.relBitPos0b_ = other.relBitPos0b_
    this.maybeAbsBitLimit0b = other.maybeAbsBitLimit0b
    this.maybeRelBitLimit0b_ = other.maybeRelBitLimit0b_
    this.byteOrder_ = other.byteOrder_
    this.debugOutputStream = other.debugOutputStream
    //this.setFragmentLastByte(other.fragmentLastByte, other.fragmentLastByteLimit)
    this.setEncoder(other.encoder)
    this.setEncodingErrorPolicy(other.encodingErrorPolicy)
  }

  /**
   * just a synonym
   */
  protected final def realStream = getJavaOutputStream()

  final override def setDebugging(setting: Boolean) {
    Assert.usage(isWritable)
    if (setting) {
      Assert.usage(!areDebugging)
      val dataCopyStream = new ByteArrayOutputStream()
      debugOutputStream = One(dataCopyStream)
      val teeStream = new TeeOutputStream(realStream, dataCopyStream)
      setJavaOutputStream(teeStream)
      this.cst.debugging = true
    } else {
      // turn debugging off
      this.cst.debugging = false
      debugOutputStream = Nope
      setJavaOutputStream(new ByteArrayOutputStream())
    }
  }

  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean = {
    Assert.usage(isWritable)
    if (bitLengthFrom1 <= 64) {
      // output as a long
      val longInt = bigInt.toLong
      putLong(longInt, bitLengthFrom1)
    } else {
      ???
      //
      // TODO: DELETE THIS COMMENT ONCE THIS IS IMPLEMENTED
      //
      // The code below is "a good try", but far from correct.
      // For now, just leaving it here with the ??? above.
      //
      // The API is actually wrong. We need to distinguish signed from unsigned here
      // because BigInts are stored sign and magnitude, so there's really no way for us
      // to acertain whether to add another bit for the sign bit, or not.
      //
      val numWholeBytes = bitLengthFrom1 / 8
      val numBitsLastByte = bitLengthFrom1 % 8

      val bytes = {
        //
        // We have to pad the bigInt out to the number of bytes required for bitLengthFrom1
        // because a bigInt in principle extends infinitely to the left (most significant bits of 0, or extended sign 1 bits)
        //
        val byteCapacity = numWholeBytes + (if (numBitsLastByte > 0) 1 else 0)
        val numBytes = bigInt.toByteArray
        Assert.notYetImplemented(numBytes.length > byteCapacity)
        val bytes = new Array[Byte](byteCapacity)
        numBytes.copyToArray(bytes, byteCapacity - numBytes.length, numBytes.length)
        bytes
      }
      //
      // TODO: we have to truncate off bits if the bitLengthFrom1 does not take all the bits
      //
      if (byteOrder eq ByteOrder.LittleEndian) {
        Bits.reverseBytes(bytes)
        if (bitOrder eq BitOrder.LeastSignificantBitFirst) {
          val nBits = putBits(bytes, 0, bitLengthFrom1)
          Assert.invariant(nBits == bitLengthFrom1)
        } else {
          val lastByte = bytes(bytes.length - 1)
          val numBitsLastByte = bitLengthFrom1 % 8
          val lastByteShiftLeft = 8 - numBitsLastByte
          val newLastByte = (lastByte << lastByteShiftLeft).toByte
          bytes(bytes.length - 1) = newLastByte
          val nBits = putBits(bytes, 0, bitLengthFrom1)
          Assert.invariant(nBits == bitLengthFrom1)
        }
      } else {
        // BigEndian case
        Assert.invariant(bitOrder eq BitOrder.MostSignificantBitFirst)
        val wholeNumShiftLeft = 8 - numBitsLastByte
        val bigIntShifted = bigInt << wholeNumShiftLeft // this can be a byte bigger
        val bytes = {
          //
          // We have to pad the bigInt out to the number of bytes required for bitLengthFrom1
          // because a bigInt in principle extends infinitely to the left (most significant bits of 0, or extended sign 1 bits)
          //
          val numBytes = bigIntShifted.toByteArray
          val byteCapacity = numWholeBytes + (if (numBitsLastByte > 0) 1 else 0)
          Assert.notYetImplemented(numBytes.length > byteCapacity)
          val bytes = new Array[Byte](byteCapacity)
          numBytes.copyToArray(bytes, byteCapacity - numBytes.length, numBytes.length)
          bytes
        }
        val nBits = putBits(bytes, 0, bitLengthFrom1)
        Assert.invariant(nBits == bitLengthFrom1)
      }
      //
      // TODO: PERFORMANCE: algorithm idea. To avoid heap allocated things, you could do this
      // by recursively calling, putting each 64-bit chunk of the bigInt into
      // a local val chunk: Long = ...least significant 64 bit chunk ...
      // If little endian, then the chunk is output before we recurse (on the way down)
      // If big endian, then the chunk is output after we recurse (on the way back up)
      //
      true
    }
  }

  private def exceedsBitLimit(lengthInBytes: Long): Boolean = {
    val relEndBitPos0b = relBitPos0b + ULong(lengthInBytes * 8)
    if (maybeRelBitLimit0b.isDefined &&
      (relEndBitPos0b > maybeRelBitLimit0b.getULong)) true
    else false
  }

  def putBits(ba: Array[Byte], byteStartOffset0b: Int, lengthInBits: Long): Long = {
    Assert.usage((ba.length - byteStartOffset0b) * 8 >= lengthInBits)
    val nWholeBytes = (lengthInBits / 8).toInt
    val nFragBits = (lengthInBits % 8).toInt
    val nBytesWritten = putBytes(ba, byteStartOffset0b, nWholeBytes)
    val nBitsWritten = nBytesWritten * 8
    if (nBytesWritten < nWholeBytes) {
      nBytesWritten * 8
    } else {
      val isFragWritten =
        if (nFragBits > 0) {
          var fragByte: Long = ba(byteStartOffset0b + nWholeBytes)
          if (bitOrder == BitOrder.MostSignificantBitFirst) {
            // we need to shift the bits. We want the most significant bits of the byte
            fragByte = fragByte >>> (8 - nFragBits)
          }
          putLong(fragByte, nFragBits)
        } else
          true
      if (isFragWritten) nBitsWritten + nFragBits
      else nBitsWritten
    }
  }

  def putBytes(ba: Array[Byte], byteStartOffset0b: Int, lengthInBytes: Int): Long = {
    Assert.usage(isWritable)
    if (isEndOnByteBoundary) {
      val nBytes =
        if (exceedsBitLimit(lengthInBytes)) {
          val n = (maybeRelBitLimit0b.getULong - relBitPos0b) / 8
          Assert.invariant(n >= 0)
          n
        } else {
          lengthInBytes
        }
      realStream.write(ba, byteStartOffset0b, nBytes.toInt)
      setRelBitPos0b(relBitPos0b + ULong(nBytes * 8))
      nBytes
    } else {
      // the data currently ends with some bits in the fragment byte.
      //
      // rather than duplicate all this shifting logic here, we're going to output
      // each byte separately as an 8-bit chunk using putLong
      //
      var i = 0
      var continue = true
      while ((i < lengthInBytes) && continue) {
        continue = putLong(Bits.asUnsignedByte(ba(i)), 8) // returns false if we hit the limit.
        if (continue) i += 1
      }
      i
    }
  }

  def putBytes(ba: Array[Byte]): Long = putBytes(ba, 0, ba.length)

  def putBitBuffer(bb: java.nio.ByteBuffer, lengthInBits: Long): Long = {
    Assert.usage(bb.remaining() * 8 >= lengthInBits)
    val nWholeBytes = (lengthInBits / 8).toInt
    val nFragBits = (lengthInBits % 8).toInt
    //
    // The position and limit of the buffer must be consistent with lengthInBits
    //
    val numBytesForLengthInBits = nWholeBytes + (if (nFragBits > 0) 1 else 0)

    Assert.usage(bb.remaining() == numBytesForLengthInBits)

    if (nFragBits > 0) bb.limit(bb.limit - 1) // last byte is the frag byte
    val nBytesWritten = putByteBuffer(bb) // output all but the frag byte if there is one.
    val nBitsWritten = nBytesWritten * 8
    if (nBytesWritten < nWholeBytes) {
      nBytesWritten * 8
    } else {
      val isFragWritten =
        if (nFragBits > 0) {
          bb.limit(bb.limit + 1)
          var fragByte: Long = Bits.asUnsignedByte(bb.get(bb.limit - 1))
          if (bitOrder == BitOrder.MostSignificantBitFirst) {
            // we need to shift the bits. We want the most significant bits of the byte
            fragByte = fragByte >>> (8 - nFragBits)
          }
          putLong(fragByte, nFragBits)
        } else
          true
      if (isFragWritten) nBitsWritten + nFragBits
      else nBitsWritten
    }
  }

  private def putByteBuffer(bb: java.nio.ByteBuffer): Long = {
    Assert.usage(isWritable)
    val nTransferred =
      if (bb.hasArray) {
        putBytes(bb.array, bb.arrayOffset + bb.position, bb.remaining())
      } else {
        if (isEndOnByteBoundary) {
          val lengthInBytes = bb.remaining
          if (exceedsBitLimit(lengthInBytes)) return 0
          val chan = Channels.newChannel(realStream) // supposedly, this will not allocate if the outStream is a FileOutputStream
          setRelBitPos0b(relBitPos0b + ULong(lengthInBytes * 8))
          chan.write(bb)
        } else {
          // not on a byte boundary
          //
          // rather than duplicate all this shifting logic here, we're going to output
          // each byte separately as an 8-bit chunk using putLong
          //
          var i = 0
          var continue = true
          val limit = bb.remaining()
          while ((i < limit) && continue) {
            continue = putLong(Bits.asUnsignedByte(bb.get(i)), 8) // returns false if we hit the limit.
            if (continue) i += 1
          }
          i
        }
      }
    nTransferred
  }

  final def putString(str: String): Long = {
    Assert.usage(isWritable)
    // must respect bitLimit0b if defined
    // must not get encoding errors until a char is being written to the output
    // otherwise we can't get precise encodingErrorPolicy='error' behavior.
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(str.length)
      cb.append(str) // one copying hop
      cb.flip
      putCharBuffer(cb)
    }
  }

  def putCharBuffer(cb: java.nio.CharBuffer): Long = {
    Assert.usage(isWritable)

    val debugCBString = if (areLogging(LogLevel.Debug)) cb.toString() else ""

    val nToTransfer = cb.remaining()
    //
    // TODO: restore mandatory alignment functionality.
    // It can't sipmly be here, as we might be writing to a buffering
    // stream, so unless we know the absoluteStartPos, we have to
    // suspend until it is known
    //
    // if (!align(encodingMandatoryAlignmentInBits)) return 0

    // must respect bitLimit0b if defined
    // must not get encoding errors until a char is being written to the output
    // otherwise we can't get precise encodingErrorPolicy='error' behavior.
    // must handle non-byte-sized characters (e.g., 7-bit), so that bit-granularity positioning
    // works.
    val res = withLocalByteBuffer { lbb =>
      //
      // TODO: data size limit. For utf-8, this will limit the max number of chars
      // in cb to below 1/4 the max number of bytes in a bytebuffer, because the maxBytesPerChar
      // is 4, even though it is going to be on average much closer to 1 or 2.
      //
      val nBytes = math.ceil(cb.remaining * encoder.maxBytesPerChar()).toLong
      val bb = lbb.getBuf(nBytes)
      // Note that encode reads the charbuffer and encodes the character into a
      // local byte buffer. The contents of the local byte buffer are then
      // copied to the DOS byte buffer using putBitBuffer, which handles
      // bitPosiition/bitLimit/fragment bytes. So because we encode to a
      // temporary output buffer, the encode loop does not need to be aware of
      // bit position/limit.
      val cr = encoder.encode(cb, bb, true)
      cr match {
        case CoderResult.UNDERFLOW => //ok. Normal termination
        case CoderResult.OVERFLOW => Assert.invariantFailed("byte buffer wasn't big enough to accomodate the string")
        case _ if cr.isMalformed() => cr.throwException()
        case _ if cr.isUnmappable() => cr.throwException()
      }
      bb.flip

      val bitsToWrite = encoder match {
        case encoderWithBits: NonByteSizeCharsetEncoder => encoderWithBits.bitWidthOfACodeUnit * nToTransfer
        case _ => bb.remaining * 8
      }
      if (putBitBuffer(bb, bitsToWrite) == 0) 0 else nToTransfer
    }
    log(LogLevel.Debug, "Wrote string '%s' to %s", debugCBString, this)
    res
  }

  def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int): Boolean = putLong(unsignedLong.longValue, bitLengthFrom1To64)

  def putLong(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    Assert.usage(bitLengthFrom1To64 >= 1 && bitLengthFrom1To64 <= 64)
    Assert.usage(isWritable)
    //
    // do we have enough room for ALL the bits?
    // if not return false.
    //
    if (maybeRelBitLimit0b.isDefined) {
      if (maybeRelBitLimit0b.get < relBitPos0b + bitLengthFrom1To64) return false
    }
    //
    // based on bit and byte order, dispatch to specific code
    //
    val res =
      if (byteOrder eq ByteOrder.BigEndian) {
        //
        // You would think this invariant would hold
        //
        // Assert.invariant(bitOrder eq BitOrder.MostSignificantBitFirst)
        //
        // However, when collapsing DOS forward into each other as a part of
        // evaluating suspensions, we encounter situations where the bitOrder
        // in the DOS has LSBF, and that was just never modified though the
        // byte order was modified. We could hammer the bitOrder to MSBF
        // any time the byte order is set, but that would mask errors where the
        // user really had the properties conflicting.
        //
        putLong_BE_MSBFirst(signedLong, bitLengthFrom1To64)
      } else {
        Assert.invariant(byteOrder eq ByteOrder.LittleEndian)
        if (bitOrder eq BitOrder.MostSignificantBitFirst) {
          putLong_LE_MSBFirst(signedLong, bitLengthFrom1To64)
        } else {
          Assert.invariant(bitOrder eq BitOrder.LeastSignificantBitFirst)
          putLong_LE_LSBFirst(signedLong, bitLengthFrom1To64)
        }
      }

    if (res) {
      setRelBitPos0b(relBitPos0b + ULong(bitLengthFrom1To64))
    }
    res
  }

  protected def putLong_BE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean
  protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean
  protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean

  /*
   * These members are like a C language union. We write as float, we get back same storage
   * as int. Ditto double to long.
   */
  protected val unionByteBuffer = ByteBuffer.allocate(8)
  private val unionDoubleBuffer = unionByteBuffer.asDoubleBuffer()
  private val unionFloatBuffer = unionByteBuffer.asFloatBuffer()
  private val unionIntBuffer = unionByteBuffer.asIntBuffer()
  protected val unionLongBuffer = unionByteBuffer.asLongBuffer()

  def putBinaryFloat(v: Float): Boolean = {
    unionFloatBuffer.put(0, v)
    val i = unionIntBuffer.get(0)
    putLong(i, 32)
  }

  def putBinaryDouble(v: Double): Boolean = {
    unionDoubleBuffer.put(0, v)
    val l = unionLongBuffer.get(0)
    putLong(l, 64)
  }

  def skip(nBits: Long): Boolean = {
    Assert.usage(isWritable)
    if (maybeRelBitLimit0b.isDefined) {
      val lim = maybeRelBitLimit0b.getULong
      if (relBitPos0b + ULong(nBits) > lim) return false
    }
    if (nBits <= 64) {
      putLong(fillLong, nBits.toInt)
    } else {
      // more than 64 bits to skip
      //
      // break into 3 skips
      //
      // first is the small skip that fills in any remaining bits of the fragment byte
      //
      // second is a skip of whole bytes done by writing whole fillbytes
      //
      // third is a skip that re-creates the fragment byte to hold whatever part of that fragment
      // is part of the skipped bits.
      //
      // often there will be no fragment byte at the beginning or at the end

      var nBitsRemaining = nBits

      if (fragmentLastByteLimit > 0) {
        // there is a fragment byte.
        val numRemainingFragmentBits = 8 - fragmentLastByteLimit
        val isInitialFragDone = putLong(fillByte, numRemainingFragmentBits)
        Assert.invariant(isInitialFragDone)
        nBitsRemaining -= numRemainingFragmentBits
        Assert.invariant(fragmentLastByteLimit == 0) // no longer is a fragment byte on the end
      }
      //
      // now the whole bytes
      //
      var nBytes = nBitsRemaining / 8 // computes floor
      while (nBytes > 0) {
        nBytes -= 1
        setRelBitPos0b(relBitPos0b + ULong(8))
        realStream.write(fillByte)
        nBitsRemaining -= 8
      }
      //
      // now any final fragment
      //
      Assert.invariant(nBitsRemaining < 8)
      if (nBitsRemaining > 0) {
        val isFinalFragDone = putLong(fillByte, nBitsRemaining.toInt)
        Assert.invariant(isFinalFragDone)
      }
      true
    }
  }

  def futureData(nBytesRequested: Int): ByteBuffer = {
    Assert.usage(isReadable)
    ByteBuffer.allocate(0)
  }

  def pastData(nBytesRequested: Int): ByteBuffer = {
    Assert.usage(isReadable)
    if (!areDebugging) throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    if (debugOutputStream == Nope) {
      ByteBuffer.allocate(0)
    } else {
      val arr = debugOutputStream.get.toByteArray()
      val bb = ByteBuffer.wrap(arr)
      val sz = math.max(0, arr.length - nBytesRequested)
      bb.limit(arr.length)
      bb.position(sz)
      bb
    }
  }

  override final def resetMaybeRelBitLimit0b(savedBitLimit0b: MaybeULong): Unit = {
    Assert.usage(isWritable)
    setMaybeRelBitLimit0b(savedBitLimit0b, true)
  }

  def validateFinalStreamState {
    // nothing to validate
  }

  final override def isAligned(bitAlignment1b: Int): Boolean = {
    Assert.usage(bitAlignment1b >= 1)
    if (bitAlignment1b =#= 1) true
    else {
      Assert.usage(maybeAbsBitPos0b.isDefined)
      val alignment = maybeAbsBitPos0b.get % bitAlignment1b
      val res = alignment == 0
      res
    }
  }

  final override def align(bitAlignment1b: Int): Boolean = {
    if (isAligned(bitAlignment1b)) true
    else {
      val deltaBits = bitAlignment1b - (maybeAbsBitPos0b.get % bitAlignment1b)
      skip(deltaBits)
    }
  }

}
