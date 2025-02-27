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

import java.io.ByteArrayOutputStream
import java.math.{ BigInteger => JBigInt }
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.charset.CoderResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption

import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSizeEncoder
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.util.Bits
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.MaybeULong

import org.apache.commons.io.output.TeeOutputStream
import passera.unsigned.ULong

sealed trait DOSState
private[io] case object Active extends DOSState
private[io] case object Finished extends DOSState
private[io] case object Uninitialized extends DOSState

trait DataOutputStreamImplMixin
  extends DataStreamCommonState
  with DataOutputStream
  with DataStreamCommonImplMixin
  with LocalBufferMixin {

  /**
   * Relative bit position zero based.
   *
   * Relative to the start of the current buffer.
   */
  private var relBitPos0b_ : ULong = ULong(0)

  private var zlStatus_ : ZeroLengthStatus = ZeroLengthStatus.Unknown

  /**
   * If we merge forward into this buffering stream, converting it into a direct stream,
   * we must set this var to the size of the stream, so that comparing this to
   * the relBitPos0b tells us if there have been any actual writes to this stream.
   */
  private var maybeStartingRelBitPos0bAfterMerging_ : MaybeULong = MaybeULong.Nope

  /**
   * Determines, as soon as possible, whether the stream has zero length, or non-zero length.
   *
   * Only produces Zero length if the stream is finished, so we know nothing will be added to
   * make it non-zero.
   *
   * Starts as Unknown status, transitions to either Zero or NonZero.
   *
   * Works even if the streams have been collapsing together as a result of earlier streams
   * having been setFinished.
   */
  final override def zeroLengthStatus = {
    import ZeroLengthStatus._
    zlStatus_ match {
      case Zero => // ok
      case NonZero => // ok
      case Unknown => {
        if (this.isFinished || this.isDead) {
          // this DOS was finished or this DOS is dead (i.e. was finished,
          // became direct and then the following DOS become direct). In either
          // case, we know that nothing has written to this DOS or the
          // zlStatus_ would have been set by setNonZeroLength() call, which is
          // required to be called by everything that writes.
          //
          // So regardless of whether this stream has been merged/collapsed/dead, etc.
          // we know that no actual writes occurred to this DOS, so it is zero length.
          // And now that it is finsihed, that can never change.
          zlStatus_ = Zero
        } else {
          // do nothing. It stays what it is, Unknown.
        }
      }
    }
    zlStatus_
  }

  /**
   * Must be called by anything that writes to the DOS
   * if the amount written was 1 bit or more.
   */
  final protected def setNonZeroLength(): Unit = {
    zlStatus_ = ZeroLengthStatus.NonZero
  }

  /**
   * Once we determine what it is, this will hold the absolute bit pos
   * of the first bit of this buffer.
   *
   * It should always be the case that the absbitPos0b is the sum of
   * this value and the relBitPos0b.
   */

  private var maybeAbsStartingBitPos0b_ : MaybeULong = MaybeULong.Nope

  private def checkInvariants(): Unit = {
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
  final def maybeAbsStartingBitPos0b: MaybeULong = {
    if (this.maybeAbsolutizedRelativeStartingBitPosInBits_.isDefined)
      maybeAbsolutizedRelativeStartingBitPosInBits_
    else
      maybeAbsStartingBitPos0b_
  }

  final def toAbsolute(relBitPos0b: ULong) = {
    Assert.usage(maybeAbsStartingBitPos0b.isDefined)
    maybeAbsStartingBitPos0b.getULong + relBitPos0b
  }
  def resetAllBitPos(): Unit = {
    this.maybeAbsolutizedRelativeStartingBitPosInBits_ = MaybeULong.Nope
    maybeAbsStartingBitPos0b_ = MaybeULong.Nope
    relBitPos0b_ = ULong(0)
    zlStatus_ = ZeroLengthStatus.Unknown
  }

  def setAbsStartingBitPos0b(newStartingBitPos0b: ULong): Unit = {
    checkInvariants()
    val mv = MaybeULong(newStartingBitPos0b.longValue)
    //
    // there are 3 states
    //
    if (
      this.maybeAbsStartingBitPos0b_.isEmpty &&
      this.maybeAbsolutizedRelativeStartingBitPosInBits_.isEmpty
    ) {
      this.maybeAbsStartingBitPos0b_ = mv
    } else if (this.maybeAbsStartingBitPos0b_.isDefined) {
      this.maybeAbsolutizedRelativeStartingBitPosInBits_ = this.maybeAbsStartingBitPos0b_
      this.maybeAbsStartingBitPos0b_ = mv
    } else {
      // both are defined
      Assert.usageError("You cannot set the abs starting bit pos again.")
    }
    checkInvariants()

    // We now know the absolute starting position of this DOS. If we also know its
    // length, then we can set the absolute starting position of the next DOS as well
    // (if one exists and if it doesn't already have an absolute starting
    // position). This can potentially propagate to other DOSs, greatly improving
    // the chances of suspensions not deadlocking, since starting absolute positions
    // are often needed for evaluation (e.g. alignment, length calculations).
    if (this.maybeLengthInBits_.isDefined && this.maybeNextInChain.isDefined) {
      val nic = this.maybeNextInChain.get.asInstanceOf[DirectOrBufferedDataOutputStream]
      if (nic.maybeAbsBitPos0b.isEmpty) {
        val nextAbsPos =
          this.maybeAbsStartingBitPos0b.getULong + this.maybeLengthInBits_.getULong
        nic.setAbsStartingBitPos0b(nextAbsPos)
      }
    }
  }

  protected final def setRelBitPos0b(newRelBitPos0b: ULong): Unit = {
    Assert.usage(isWritable)
    checkInvariants()
    relBitPos0b_ = newRelBitPos0b
    checkInvariants()
  }

  def setLengthInBits(newLengthInBits: ULong): Unit = {
    Assert.invariant(maybeLengthInBits_.isEmpty)
    Assert.usage(isWritable)
    checkInvariants()
    this.maybeLengthInBits_ = MaybeULong(newLengthInBits.longValue)
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
   * of content and value length regions) to still be meaningful even though
   * we have collapsed the stream into a direct one.
   */
  private var maybeAbsolutizedRelativeStartingBitPosInBits_ = MaybeULong.Nope

  /**
   * When we split a stream, we do so because some suspension needs to write to
   * the end of this DOS. In some cases, we know the number of bits that
   * suspension will write before it does it, and so we can set the starting
   * absolute bit position of the new buffered stream that we split into.
   * However, we can only do that if we know the absolute starting position of
   * the original DOS when the split occurs. If we do not know that, we can
   * still calculate the final length of this DOS (i.e. the current relative
   * position + the length of the suspension) and store that here. Once we learn
   * the starting absolute position of this DOS, we can then set the absolute
   * starting position of the next DOS using this length.
   */
  private var maybeLengthInBits_ = MaybeULong.Nope

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
  def setMaybeRelBitLimit0b(
    newMaybeRelBitLimit0b: MaybeULong,
    reset: Boolean = false
  ): Boolean = {
    Assert.invariant(
      (maybeAbsBitLimit0b.isDefined && maybeRelBitLimit0b.isDefined) || maybeAbsBitLimit0b.isEmpty
    )
    if (newMaybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = MaybeULong.Nope
      maybeAbsBitLimit0b = MaybeULong.Nope
    } else if (maybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = newMaybeRelBitLimit0b
      // absolute limit doesn't getchanged (it must be undefined)
      Assert.invariant(maybeAbsBitLimit0b.isEmpty)
    } else {
      val delta =
        maybeRelBitLimit0b.get - newMaybeRelBitLimit0b.get // this is how much the relative value is changing.
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
  def fragmentLastByteLimit: Int = fragmentLastByteLimit_

  def setFragmentLastByte(newFragmentByte: Int, nBitsInUse: Int): Unit = {
    Assert.usage(nBitsInUse >= 0 && nBitsInUse <= 7)
    Assert.usage(
      newFragmentByte >= 0 && newFragmentByte <= 255
    ) // no bits above first byte are in use.
    if (nBitsInUse == 0) {
      Assert.usage(newFragmentByte == 0)
    }
    fragmentLastByte_ = newFragmentByte
    fragmentLastByteLimit_ = nBitsInUse
  }

  def isEndOnByteBoundary = fragmentLastByteLimit == 0

  private var _dosState: DOSState = Active // active when new.

  @inline final def dosState = _dosState

  @inline protected final def setDOSState(newState: DOSState): Unit = { _dosState = newState }

  private[io] def isBuffering: Boolean

  @inline private[io] final def isDirect = !isBuffering

  @inline private[io] final def isDead = { _dosState =:= Uninitialized }
  @inline override final def isFinished = { _dosState =:= Finished }
  // @inline override def setFinished(finfo: FormatInfo) { _dosState = Finished }
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

  protected def setJavaOutputStream(newOutputStream: java.io.OutputStream): Unit

  protected def getJavaOutputStream(): java.io.OutputStream

  final def cst: DataOutputStreamImplMixin = this

  protected def assignFrom(other: DataOutputStreamImplMixin): Unit = {
    Assert.usage(isWritable)
    super.assignFrom(other)
    this.maybeAbsStartingBitPos0b_ = other.maybeAbsStartingBitPos0b_
    this.maybeAbsolutizedRelativeStartingBitPosInBits_ =
      other.maybeAbsolutizedRelativeStartingBitPosInBits_
    this.relBitPos0b_ = other.relBitPos0b_
    this.maybeAbsBitLimit0b = other.maybeAbsBitLimit0b
    this.maybeRelBitLimit0b_ = other.maybeRelBitLimit0b_
    this.debugOutputStream = other.debugOutputStream
    this.zlStatus_ = other.zlStatus_
  }

  /**
   * just a synonym
   */
  protected final def realStream = getJavaOutputStream()

  final override def setDebugging(setting: Boolean): Unit = {
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
    }
  }

  final override def putBigInt(
    bigInt: JBigInt,
    bitLengthFrom1: Int,
    signed: Boolean,
    finfo: FormatInfo
  ): Boolean = {
    Assert.usage(isWritable)
    Assert.usage(bitLengthFrom1 > 0)
    Assert.usage(signed || (!signed && bigInt.signum() >= 0))

    val res = {
      if (bitLengthFrom1 <= 64) {
        // output as a long
        val longInt = bigInt.longValue()
        putLongChecked(longInt, bitLengthFrom1, finfo)
      } else {
        // Note that we do not need to distinguish between signed and unsigned
        // here. Checks should have been performend earlier to ensure that the
        // bigInt value matches the signed/unsignedness. So we just need to
        // convert this to a byte array, ensure that the number of bits in that
        // bit array fit the bit length, and put the array.
        val array = bigInt.toByteArray
        val numWholeBytesNeeded = (bitLengthFrom1 + 7) / 8

        val maybeArrayToPut =
          if (array.size > numWholeBytesNeeded) {
            // This is the only case where we care about signedness. If the
            // BigInt is a positive value and the most significant bit is a 1,
            // then toByteArray will create one extra byte that is all zeros so
            // that the two's complement isn't negative. In this case, the array
            // size is 1 greater than numWholeBytesNeeded and the most
            // significant byte is zero. For a signed type (e.g. xs:integer),
            // having this extra byte, regardless of its value, means the BigInt
            // value was too big to fit into bitLengthFrom1 bits. However, if
            // this is a unsigned type (e.g. xs:nonNegativeInteger), this extra
            // byte can be zero and still fit into bitLengthFrom1. So if this is
            // the case, then just chop off that byte and write the array.
            // Otherwise, this results in Nope which ends up returning false.
            if (!signed && (array.size == numWholeBytesNeeded + 1) && array(0) == 0) {
              One(array.tail)
            } else {
              Nope
            }
          } else if (array.size < numWholeBytesNeeded) {
            // This bigInt value can definitely fit in the number of bits,
            // however, we need to pad it up to numWholeBytesNeed. We may need to
            // sign extend with the most significant bit of most significant byte
            // of the array
            val paddedArray = new Array[Byte](numWholeBytesNeeded)
            val numPaddingBytes = numWholeBytesNeeded - array.size

            array.copyToArray(paddedArray, numPaddingBytes)
            val sign = 0x80 & array(0)
            if (sign > 0) {
              // The most significant bit of the most significant byte was 1. That
              // means this was a negative number and these padding bytes must be
              // sign extended
              Assert.invariant(bigInt.signum() < 0)
              var i = 0
              while (i < numPaddingBytes) {
                paddedArray(i) = 0xff.toByte
                i += 1
              }
            }
            One(paddedArray)
          } else {
            // We got the right amount of bytes, however, it is possible that
            // more significant bits were set that can fit in bitLengthFrom1.
            // Determine if that is the case.
            val fragBits = bitLengthFrom1 % 8
            if (fragBits == 0) {
              // no frag bits, so the most significant bit is fine and no sign
              // extending is needed to be checked
              One(array)
            } else {
              val shifted =
                array(
                  0
                ) >> (fragBits - 1) // shift off the bits we don't care about (with sign extend shift)
              val signBit = shifted & 0x1 // get the most significant bit
              val signExtendedBits = shifted >> 1 // shift off the sign bit

              // At this point, signExtendedBits should be all 1's (i.e. -1) if
              // the sign bit was 1, or all zeros (i.e. 0) if the sign bit was 0.
              // If this isn't the case, then there were non-signed extended bits
              // above our most significant bit, and so this BigInt was too big
              // for the number of bits. If this is the case, result in a Nope
              // which ends up returning false.
              if (
                (signBit == 1 && signExtendedBits != -1) || (signBit == 0 && signExtendedBits != 0)
              ) {
                // error
                Nope
              } else {
                // The most significant byte is properly sign extended for the
                // for the number of bits, so the array is good
                One(array)
              }
            }
          }

        if (maybeArrayToPut.isDefined) {
          // at this point, we have an array that is of the right size and properly
          // sign extended. It is also BE MSBF, so we can put it just like we would
          // put a hexBinary array
          putByteArray(maybeArrayToPut.get, bitLengthFrom1, finfo)
        } else {
          false
        }
      }
    }
    res
  }

  final override def putByteArray(
    array: Array[Byte],
    bitLengthFrom1: Long,
    finfo: FormatInfo,
    ignoreByteOrder: Boolean = false
  ): Boolean = {
    // this is to be used for an array generated by getByteArray. Thus, this
    // array is expected by to BigEndian MSBF. It must be transformed into an
    // array that the other putBytes/Bits/etc functions can accept. The length
    // in bits must be positive and must be <= the max size of an array in
    // bits, which is IntMax * 8
    Assert.usage(bitLengthFrom1 >= 1 && bitLengthFrom1 <= Int.MaxValue.toLong * 8)
    Assert.usage(isWritable)

    val res =
      if (
        maybeRelBitLimit0b.isDefined && maybeRelBitLimit0b.get < (relBitPos0b + bitLengthFrom1)
      ) {
        false
      } else {
        // Cannot rely on array.length since it is possible the array is bigger
        // than the number of bits we should put. This value is guaranteed to
        // fit in an integer due to the assertion at the top, so toInt is safe
        val bytesToPutFromArray = ((bitLengthFrom1 + 7) / 8).toInt

        // First thing we need to do is reverse the array if it's littleEndian,
        // but only bother checking if the bilength is more than a byte--single
        // byte sizes are common and it avoids the byteOrder check and
        // potential array allocation
        val bytes =
          if (
            bitLengthFrom1 > 8 &&
            finfo.byteOrder =:= ByteOrder.LittleEndian &&
            !ignoreByteOrder
          ) {
            // We need to reverse this array. However, we cannot modify this
            // array since it might come straight from the infoset, which could
            // potentiall affect expressions that reference this data. So
            // allocate a new array and populate it with the reverse of the old
            // array.
            val reversedArray = new Array[Byte](bytesToPutFromArray)
            var index = 0
            while (index < bytesToPutFromArray) {
              reversedArray(index) = array(bytesToPutFromArray - 1 - index)
              index += 1
            }
            reversedArray
          } else {
            // do not need to reverse, and the below code does not modify the
            // array, so we can just use this for outputting data
            array
          }

        // Determine the length of the new fragment after everything is added
        // from the array taking into account the current fragment limit, and
        // define a place to store its eventual value
        val newFragmentByteLimit = ((fragmentLastByteLimit + bitLengthFrom1) % 8).toInt
        var newFragmentByte: Int = 0

        if (fragmentLastByteLimit == 0) {
          // There is no current fragment byte, so we can just write all the
          // full bytes. Due to the assertion at the top, we know
          // bitLengthFrom1 / 8 must be <= MaxInt, so toInt is safe
          val fullBytes = (bitLengthFrom1 / 8).toInt
          realStream.write(bytes, 0, fullBytes)

          // There still maybe be a fragment byte. If so it will become our new
          // fragment byte. Note that this new byte contains too much data that
          // needs to be masked out, but we'll do that at the end.
          if (newFragmentByteLimit > 0) {
            newFragmentByte = Bits.asUnsignedByte(bytes(fullBytes))
          }
        } else {
          // There is an existing fragment byte. That means we must combine
          // all the bytes with the existing fragment byte to create
          // new bytes and a new fragment byte, taking bitOrder into account

          val isMSBF = finfo.bitOrder == BitOrder.MostSignificantBitFirst

          // Determine masks and shifts needed to combine the fragment byte
          // with array bytes
          val fragByteMask =
            if (isMSBF)
              Bits.maskR(fragmentLastByteLimit)
            else
              Bits.maskL(fragmentLastByteLimit)
          val curByteMask = ~fragByteMask & 0xff
          val fragShift = 8 - fragmentLastByteLimit
          val curShift = fragmentLastByteLimit

          // When we loop through the bytes we combine bytes with the existing
          // fragment byte, updating the new fragment byte as we go along. This
          // byte array might also have some fragment bits if the bitLength is
          // not a multiple of 8. So when we combine the fragment byte with the
          // last fragment byte, it may or may not create a complete new byte.
          // The below pre-determines where the last index in the byte array is,
          // and if that byte combined with a fragment byte will create a whole
          // byte or have some leftover.
          val lastIndexCreatesFullByte = {
            val fragmentBitsInArray = bitLengthFrom1 % 8
            val bitsAvailableInLastByte =
              if (fragmentBitsInArray == 0) 8 else fragmentBitsInArray
            (fragmentLastByteLimit + bitsAvailableInLastByte) >= 8
          }
          val lastIndex = bytesToPutFromArray - 1

          // For each byte, combine it with the previous fragment byte
          // according to bitOrder to create a new byte and a new fragment
          // byte. Potentially write the new byte to the stream if we had
          // enough bits for a full byte.
          newFragmentByte = fragmentLastByte
          var index = 0
          while (index < bytesToPutFromArray) {
            val curByte = Bits.asUnsignedByte(bytes(index))
            val newByte =
              if (isMSBF)
                ((curByte & curByteMask) >> curShift) | newFragmentByte
              else
                ((curByte & curByteMask) << curShift) | newFragmentByte

            if (index != lastIndex || lastIndexCreatesFullByte) {
              // If this is *not* the last byte, then we definitely got 8 bits
              // from this index and can write a full byte and update the
              // fragment with the remaining byte. If this *is* the last byte,
              // but what is left in the last byte combined with the fragment
              // byte creates a full byte, then write the full byte and update
              // the fragment byte with what is left over.
              realStream.write(Bits.asSignedByte(newByte))
              if (isMSBF)
                newFragmentByte = (curByte & fragByteMask) << fragShift
              else
                newFragmentByte = (curByte & fragByteMask) >> fragShift
            } else {
              // We are at the last byte, and the remaining bits combined with
              // the frag byte will not create a whole byte. The new byte we
              // calculated is actually the new frag byte.
              newFragmentByte = newByte
            }
            index += 1
          }
        }

        if (newFragmentByteLimit > 0) {
          // We have written all the data that can be written and now have
          // calculated a new fragment. However, since this is a fragment byte,
          // some of the bits need to be masked out.
          val mask =
            if (finfo.bitOrder == BitOrder.MostSignificantBitFirst)
              Bits.maskL(newFragmentByteLimit)
            else
              Bits.maskR(newFragmentByteLimit)
          setFragmentLastByte(newFragmentByte & mask, newFragmentByteLimit)
        } else {
          // After writing all the bytes, we ended on a byte boundary, there is
          // no fragment byte. Erase the existing one if it existed.
          setFragmentLastByte(0, 0)
        }

        setRelBitPos0b(relBitPos0b + ULong(bitLengthFrom1))
        setNonZeroLength()
        true
      }
    res
  }

  private def exceedsBitLimit(lengthInBytes: Long): Boolean = {
    val relEndBitPos0b = relBitPos0b + ULong(lengthInBytes * 8)
    if (
      maybeRelBitLimit0b.isDefined &&
      (relEndBitPos0b > maybeRelBitLimit0b.getULong)
    ) true
    else false
  }

  /**
   * Returns number of bytes transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  final def putBytes(
    ba: Array[Byte],
    byteStartOffset0b: Int,
    lengthInBytes: Int,
    finfo: FormatInfo
  ): Long = {
    Assert.usage(isWritable)
    if (isEndOnByteBoundary) {
      val nBytes: Long =
        if (exceedsBitLimit(lengthInBytes)) {
          val n = (maybeRelBitLimit0b.getULong - relBitPos0b) / 8
          Assert.invariant(n >= 0)
          n
        } else {
          lengthInBytes
        }
      realStream.write(ba, byteStartOffset0b, nBytes.toInt)
      setRelBitPos0b(relBitPos0b + ULong(nBytes * 8))
      if (lengthInBytes > 0)
        setNonZeroLength()
      nBytes
    } else {
      // the data currently ends with some bits in the fragment byte.
      //
      // rather than duplicate all this shifting logic here, we're going to output
      // each byte separately as an 8-bit chunk using putLongUnchecked
      //
      var i = 0
      var continue = true
      while ((i < lengthInBytes) && continue) {
        continue = putLongUnchecked(
          Bits.asUnsignedByte(ba(i)),
          8,
          finfo
        ) // returns false if we hit the limit.
        if (continue) i += 1
      }
      i
    }
  }

  final def putFile(
    path: Path,
    lengthInBits: Long,
    chunkSizeInBytes: Int,
    finfo: FormatInfo
  ): Long = {

    val fileStream =
      try {
        Files.newInputStream(path, StandardOpenOption.READ)
      } catch {
        case e: Exception =>
          throw new FileIOException(
            "Unable to open file %s for reading: %s".format(path.toString, e.getMessage())
          )
      }

    val array = new Array[Byte](chunkSizeInBytes)

    val chunkSizeInBits = chunkSizeInBytes * 8

    var remainingBitsToPut = lengthInBits
    var fileHasData = true
    var bitsWritten: Long = 0
    while (remainingBitsToPut > 0 && fileHasData) {
      val bitsToRead = Math.min(remainingBitsToPut, chunkSizeInBits)
      val bytesToRead = (bitsToRead + 7) / 8
      val bytesRead = fileStream.read(array, 0, bytesToRead.toInt)
      if (bytesRead == -1) {
        fileHasData = false
      } else {
        val bitsToPut = Math.min(bytesRead * 8, bitsToRead)
        val ret = putByteArray(array, bitsToPut.toInt, finfo, ignoreByteOrder = true)
        if (!ret) {
          fileStream.close()
          throw new FileIOException("Failed to write file data: %s".format(path.toString))
        }
        bitsWritten += bitsToPut
        remainingBitsToPut -= bitsToPut
      }
    }

    fileStream.close()

    // calculate the skip bits
    val nFillBits = remainingBitsToPut
    if (nFillBits > 0) {
      val ret = skip(nFillBits, finfo)
      Assert.invariant(ret)
      bitsWritten += nFillBits
    }
    bitsWritten
  }

  /**
   * Returns number of bytes transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  private[io] def putBytes(ba: Array[Byte], finfo: FormatInfo): Long =
    putBytes(ba, 0, ba.length, finfo)

  private[io] def putBitBuffer(
    bb: java.nio.ByteBuffer,
    lengthInBits: Long,
    finfo: FormatInfo
  ): Long = {
    Assert.usage(bb.remaining() * 8 >= lengthInBits)
    val nWholeBytes = (lengthInBits / 8).toInt
    val nFragBits = (lengthInBits % 8).toInt
    //
    // The position and limit of the buffer must be consistent with lengthInBits
    //
    val numBytesForLengthInBits = nWholeBytes + (if (nFragBits > 0) 1 else 0)

    Assert.usage(bb.remaining() == numBytesForLengthInBits)

    if (nFragBits > 0) bb.limit(bb.limit() - 1) // last byte is the frag byte
    val nBytesWritten =
      putByteBuffer(bb, finfo) // output all but the frag byte if there is one.
    val nBitsWritten = nBytesWritten * 8
    val res = {
      if (nBytesWritten < nWholeBytes) {
        nBytesWritten * 8
      } else {
        val isFragWritten =
          if (nFragBits > 0) {
            bb.limit(bb.limit() + 1)
            var fragByte: Long = Bits.asUnsignedByte(bb.get(bb.limit() - 1))
            if (finfo.bitOrder eq BitOrder.MostSignificantBitFirst) {
              // we need to shift the bits. We want the most significant bits of the byte
              fragByte = fragByte >>> (8 - nFragBits)
            }
            putLongChecked(fragByte, nFragBits, finfo)
          } else
            true
        if (isFragWritten) nBitsWritten + nFragBits
        else nBitsWritten
      }
    }
    if (res > 0) setNonZeroLength()
    res
  }

  private def putByteBuffer(bb: java.nio.ByteBuffer, finfo: FormatInfo): Long = {
    Assert.usage(isWritable)
    val nTransferred: Long =
      if (bb.hasArray) {
        putBytes(bb.array, bb.arrayOffset + bb.position(), bb.remaining(), finfo)
      } else {
        if (isEndOnByteBoundary) {
          val lengthInBytes = bb.remaining
          if (exceedsBitLimit(lengthInBytes)) return 0
          val chan = Channels.newChannel(
            realStream
          ) // supposedly, this will not allocate if the outStream is a FileOutputStream
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
            continue = putLongChecked(
              Bits.asUnsignedByte(bb.get(i)),
              8,
              finfo
            ) // returns false if we hit the limit.
            if (continue) i += 1
          }
          i
        }
      }
    if (nTransferred > 0) setNonZeroLength()
    nTransferred
  }

  final def putString(str: String, finfo: FormatInfo): Long = {
    Assert.usage(isWritable)
    // must respect bitLimit0b if defined
    // must not get encoding errors until a char is being written to the output
    // otherwise we can't get precise encodingErrorPolicy='error' behavior.
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(str.length)
      cb.append(str) // one copying hop
      cb.flip
      putCharBuffer(cb, finfo)
    }
  }

  final def putCharBuffer(cb: java.nio.CharBuffer, finfo: FormatInfo): Long = {
    Assert.usage(isWritable)

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
      val nBytes = math.ceil(cb.remaining * finfo.encoder.maxBytesPerChar()).toLong
      val bb = lbb.getBuf(nBytes)
      // Note that encode reads the charbuffer and encodes the character into a
      // local byte buffer. The contents of the local byte buffer are then
      // copied to the DOS byte buffer using putBitBuffer, which handles
      // bitPosiition/bitLimit/fragment bytes. So because we encode to a
      // temporary output buffer, the encode loop does not need to be aware of
      // bit position/limit.
      val cr = finfo.encoder.encode(cb, bb, true)
      cr match {
        case CoderResult.UNDERFLOW => // ok. Normal termination
        case CoderResult.OVERFLOW =>
          Assert.invariantFailed("byte buffer wasn't big enough to accomodate the string")
        case _ if cr.isMalformed() => cr.throwException()
        case _ if cr.isUnmappable() => cr.throwException()
      }
      bb.flip

      val bitsToWrite = finfo.encoder match {
        case encoderWithBits: BitsCharsetNonByteSizeEncoder =>
          encoderWithBits.bitsCharset.bitWidthOfACodeUnit * nToTransfer
        case _ => bb.remaining * 8
      }
      if (putBitBuffer(bb, bitsToWrite, finfo) == 0) 0 else nToTransfer
    }
    if (res > 0)
      setNonZeroLength()
    res
  }

  protected final def putLongChecked(
    signedLong: Long,
    bitLengthFrom1To64: Int,
    finfo: FormatInfo
  ): Boolean = {
    Assert.usage(bitLengthFrom1To64 >= 1 && bitLengthFrom1To64 <= 64)
    Assert.usage(isWritable)
    //
    // do we have enough room for ALL the bits?
    // if not return false.
    //
    if (maybeRelBitLimit0b.isDefined) {
      if (maybeRelBitLimit0b.get < relBitPos0b + bitLengthFrom1To64) return false
    }

    putLongUnchecked(signedLong, bitLengthFrom1To64, finfo)
  }

  /**
   * Use when calling from a put/write operation that has already checked the
   * length limit.
   *
   * Assumed to be called from inner loops, so should be fast as possible.
   */
  protected final def putLongUnchecked(
    signedLong: Long,
    bitLengthFrom1To64: Int,
    finfo: FormatInfo,
    ignoreByteOrder: Boolean = false
  ): Boolean = {
    // The order we check bitOrder vs byteOrder here is actually very
    // important, primarily due to how suspensions end up resolved and data
    // output streams (DOS) collapsed. To explain why:
    //
    // When we create new a buffered DOS, no byteOrder is really associated
    // with the DOS, because a DOS is just a series of bytes. To figure out
    // what bytes to write to the DOS, we use the FormatInfo associated with an
    // element, call the appropriate function below to convert that element to
    // bytes, and then write those bytes to the DOS. This works as expected
    // most of the time.
    //
    // However, a buffered DOS maintains a fragment byte, and at some point we
    // collapse that fragment byte into a direct DOS using this function. But
    // we just said that there not really a byteOrder associated with a DOS,
    // only with elements. So if we do not know the byteOrder of the fragment
    // byte of a DOS, how do we know which function below to call when
    // collapsing a fragment byte?
    //
    // Well, we do keep track of the bitOrder, since we need to know if we
    // change bitOrder not on a byte boundary, and we need to know the bitOrder
    // when collapsing a fragment byte. And fortunately, byteOrder doesn't
    // matter because a fragment byte is always less than 8 bits--byteOrder
    // only makes a difference for lengths greater than 8 bits. So, as long as
    // we call *some* function below that uses the right bitOrder, then it
    // doesn't matter what the byteOrder is. So we need to make sure we check
    // bitOrder and byteOrder below in the correct order so that that if
    // bitOrer is MSBF we call one of the MBSF functions, and vice versa for
    // LSBF. Note that for this reason, even if bitOrder is LSBF and byteOrder
    // is BE, we can still call putLong_LE_LSBFirst function. That's because
    // the only time that combination can legally occurs is in this edge case
    // of collapsing a fragment byte, and then byte order doesn't matter.
    val res =
      if ((finfo.bitOrder eq BitOrder.MostSignificantBitFirst) && !ignoreByteOrder) {
        if (finfo.byteOrder eq ByteOrder.BigEndian) {
          putLong_BE_MSBFirst(signedLong, bitLengthFrom1To64)
        } else {
          putLong_LE_MSBFirst(signedLong, bitLengthFrom1To64)
        }
      } else {
        putLong_LE_LSBFirst(signedLong, bitLengthFrom1To64)
      }

    if (res) {
      setRelBitPos0b(relBitPos0b + ULong(bitLengthFrom1To64))
      setNonZeroLength()
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

  final override def putBinaryFloat(v: Float, finfo: FormatInfo): Boolean = {
    unionFloatBuffer.put(0, v)
    val i = unionIntBuffer.get(0)
    val res = putLongChecked(i, 32, finfo)
    res
  }

  final override def putBinaryDouble(v: Double, finfo: FormatInfo): Boolean = {
    unionDoubleBuffer.put(0, v)
    val l = unionLongBuffer.get(0)
    val res = putLongChecked(l, 64, finfo)
    res
  }

  /**
   * Used when we have to fill in things that are larger or smaller than a byte
   */
  private def fillLong(fillByte: Byte) = {
    var fl: Long = 0L
    val fb = fillByte.toInt & 0xff
    fl = (fl << 8) + fb
    fl = (fl << 8) + fb
    fl = (fl << 8) + fb
    fl = (fl << 8) + fb
    fl = (fl << 8) + fb
    fl = (fl << 8) + fb
    fl = (fl << 8) + fb
    fl = (fl << 8) + fb
    fl
  }

  final override def skip(nBits: Long, finfo: FormatInfo): Boolean = {
    Assert.usage(isWritable)
    if (maybeRelBitLimit0b.isDefined) {
      val lim = maybeRelBitLimit0b.getULong
      if (relBitPos0b + ULong(nBits) > lim) return false
    }
    if (nBits <= 64) {
      val fb = finfo.fillByte
      val lng = fillLong(fb)
      putLongUnchecked(lng, nBits.toInt, finfo)
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
        val isInitialFragDone =
          putLongUnchecked(finfo.fillByte, numRemainingFragmentBits, finfo)
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
        realStream.write(finfo.fillByte)
        nBitsRemaining -= 8
      }
      //
      // now any final fragment
      //
      Assert.invariant(nBitsRemaining < 8)
      if (nBitsRemaining > 0) {
        val isFinalFragDone = putLongUnchecked(finfo.fillByte, nBitsRemaining.toInt, finfo)
        Assert.invariant(isFinalFragDone)
      }
      setNonZeroLength()
      true
    }
  }

  final override def futureData(nBytesRequested: Int): ByteBuffer = {
    Assert.usage(isReadable)
    ByteBuffer.allocate(0)
  }

  final override def pastData(nBytesRequested: Int): ByteBuffer = {
    Assert.usage(
      isReadable ||
        // when unparsing trace/debug wants to access pastData from this DOS
        // even after it has been closed. This is just a consequence of the
        // creation and completion of DOS interacting with the DOS being
        // created on the fly to implement layering, where we allocate a new
        // DOS for the layer, and then later just drop it when the layer exits.
        // At that point the layer is closed, but trace/debug still wants to print
        // pastData from it as part of what it displays.
        areDebugging
    )
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

  final override def align(bitAlignment1b: Int, finfo: FormatInfo): Boolean = {
    if (isAligned(bitAlignment1b)) true
    else {
      val deltaBits = bitAlignment1b - (maybeAbsBitPos0b.get % bitAlignment1b)
      skip(deltaBits, finfo)
    }
  }

}
