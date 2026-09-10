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

import java.io.File
import java.math.BigInteger as JBigInt
import java.nio.CharBuffer
import java.nio.file.Path

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeULong

import passera.unsigned.ULong

sealed abstract class ZeroLengthStatus
object ZeroLengthStatus {
  object Zero extends ZeroLengthStatus
  object NonZero extends ZeroLengthStatus
  object Unknown extends ZeroLengthStatus
}

/**
 * Callback for code outside daffodil-io that wants to know, without
 * polling for it, the moment some fact about a specific DataOutputStream
 * settles into its final, permanent value: currently its absolute bit
 * position (some suspensions only need this, not the DOS to be fully
 * finished, to become resolvable: e.g. alignment fill, or a length
 * calculation where the other endpoint is already absolute) or its
 * zeroLengthStatus (e.g. deciding whether to suppress a separator).
 * Unlike the finished notification below, this one's only implementer
 * (Suspension) isn't a SuspensionWaiter; each
 * suspension may be watching a different DOS from its own current
 * writing context, so it registers itself directly. That's why this
 * stays a generic listener trait instead of also being collapsed onto
 * SuspensionWaiter. A single notifyKnown covers every such fact (rather
 * than a differently-named method per fact) since every implementer to
 * date reacts the same way regardless of which one changed: worth a
 * real attempt again.
 */
trait DataOutputStreamEventListener {
  def notifyKnown(dos: DataOutputStream): Unit
}

/**
 * Shared bookkeeping for a registry of listeners waiting on some
 * DataOutputStream fact that settles once and never changes again:
 * register, remove, and clear-then-notify. notify is supplied
 * per instance rather than via subclassing, so a use site just
 * constructs one directly with its own dos and notify callback (e.g.
 * `new DataOutputStreamListenerRegistry[SuspensionWaiter](this, (w, _) =>
 * w.notifySuspensions())`) instead of a registry subclass per event.
 */
private[io] final class DataOutputStreamListenerRegistry[L](
  dos: DataOutputStream,
  notify: (L, DataOutputStream) => Unit
) {
  private var listeners: Set[L] = Set.empty

  def register(l: L): Unit = { listeners = listeners + l }

  def remove(l: L): Unit = { listeners = listeners - l }

  // Drops every registered listener without notifying them, for a DOS
  // being reset for reuse rather than reaching the fact they're waiting on.
  def clear(): Unit = { listeners = Set.empty }

  // Resets the field to empty BEFORE running any callback, not after:
  // toNotify aliases the old (immutable, so this is free) Set, so a
  // re-entrant registration during notification lands in a fresh
  // `listeners` and survives instead of being wiped out afterward.
  def clearAndNotifyAll(): Unit = {
    val toNotify = listeners
    listeners = Set.empty
    toNotify.foreach(notify(_, dos))
  }
}

/**
 * There is an asymmetry between DataInputStream and DataOutputStream with respect to the
 * positions and limits in the bit stream.
 *
 * For the DataInputStream, we have this concept of the current bitPos0b,
 * and optionally there may be abound called bitLimit0b. There are 1b
 * variants of these.
 *
 * For parsing, these are always absolute values, that is they contain bit
 * position relative the ultimate start of the input stream where parsing
 * began.
 *
 * For DataOutputStream, we have slightly different concepts.
 *
 * There are absolute and relative variants. The absolute bitPosOb or
 * absBitPos0b is symmetric to the parser's bitPos0b. It's the position
 * relative to the ultimate start of the output stream.
 *
 * However, we often do not know this value. So the UState and
 * DataOutputStream have a maybeAbsBitPos0b which can be MaybeULong.Nope if
 * the value isn't known.
 *
 * In addition we have the relative or relBitPos0b. This is relative to the
 * start of whatever buffer we are doing unparsing into.
 *
 * When unparsing, we often have to unparse into a buffer where the
 * ultimate actual absolute position isn't yet known, but we have to do the
 * unparsing anyway, for example so that we can measure exactly how long
 * something is.
 *
 * Conversely, sometimes we simply must have the absolute output bit
 * position, for example, when computing the number of bits to insert to
 * achieve the required alignment.
 *
 * Hence we have relBitPos0b - always known and is a value >= 0, and we
 * have maybeAbsBitPos0b which is a MaybeULong. If known it is >=0.
 *
 * Corresponding to bit position we have bit limit, which is measured in
 * the same 0b or 1b units, but is *always* a maybe type, because even in
 * the case where we know the absolute position, we still may or may not
 * have any limit in place. Hence the UState and DataOutputStream have a
 *
 * maybeRelBitLimit0b
 *
 * and
 *
 * maybeAbsBitLimit0b.
 *
 * One invariant is this: when the absolute bit pos is known, then it is
 * the same as the relative bit pos. Similarly when the absolute bit limit
 * is known, then the relative bit limit is known and is equal.
 *
 */
trait DataOutputStream extends DataStreamCommon {

  def maybeNextInChain: Maybe[DataOutputStream]

  def id: String

  def relBitPos0b: ULong

  final def relBitPos1b: ULong = ULong(relBitPos0b + 1L)

  def maybeAbsBitPos0b: MaybeULong

  /**
   * These values are used for output streams that could change from
   * ByteArray's to File based output streams.
   */
  def chunkSizeInBytes: Int
  def maxBufferSizeInBytes: Long
  def tempDirPath: File

  /**
   * maybeExistingFile is used in the case of blob files, where we already have an
   * existing file containing the data. This is the path to said file.
   */
  def maybeExistingFile: Maybe[Path]

  /**
   * Besides setting the relBitPos, it also maintains the value of
   * the absolute bit pos, if it is known.
   */
  protected def setRelBitPos0b(newRelBitPos0b: ULong): Unit

  /**
   * Absolute bit limit zero based
   *
   * If defined it is the position 1 bit past the last bit location that can be written.
   * So if we at starting at bitPos0b of 0, and we allow only 100 bits, then the bit positions are
   * 0 to 99, and the bit limit is 100.
   */
  def maybeAbsBitLimit0b: MaybeULong

  /**
   * Relative bit limit zero based
   */
  def maybeRelBitLimit0b: MaybeULong

  /**
   * sets, but also maintains the absolute bit limit, if that is defined.
   */
  protected def setMaybeRelBitLimit0b(
    newMaybeRelBitLimit0b: MaybeULong,
    reset: Boolean = false
  ): Boolean

  def resetMaybeRelBitLimit0b(savedBitLimit0b: MaybeULong): Unit

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putLong(signedLong: Long, bitLengthFrom1To64: Int, finfo: FormatInfo): Boolean

  /**
   * If bitLengthFrom1To64 bits are available to be written before bitLimit0b (if defined) is encountered,
   * then this writes the bitLengthFrom1To64 least significant bits of the long using the
   * current bit order and byte order, and returns true.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * It is a usage error if bitLengthFrom1To64 is not in the range 1 to 64 inclusive.
   */
  def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int, finfo: FormatInfo): Boolean

  /**
   * If bitLengthFrom1 bits are available to be written before bitLimit0b (if
   * defined) is encountered, then this writes the bitLengthFrom1 least
   * significant bits of the bigInt using the current bit order and byte order,
   * and returns true. The signed flag determines whether or not the output
   * should be output as a signed or unsigned type.
   *
   * If not enough bits are available or the big integer cannot fit into
   * bitLengthFrom1 bits, this writes nothing and returns false.
   *
   * It is a usage error if signed is false and bigInt is a negative BigInteger.
   *
   * It is a usage error if bitLengthFrom1 is not greater than or equal to 1.
   *
   */
  def putBigInt(
    bigInt: JBigInt,
    bitLengthFrom1: Int,
    signed: Boolean,
    finfo: FormatInfo
  ): Boolean

  /**
   * If bitLengthFrom1 bits are available to be written before bitLimit0b (if
   * defined) is encountered, then this writes the bitLengthFrom1 bits of the
   * ba using the current bit order and byte order, and returns true. The array
   * is assumed to be have bigEndian byte order and most significant bit first
   * bit order.
   *
   * If not enough bits are available, this writes nothing and returns false.
   *
   * ignoreByteOrder is used when we are working with a FileOutputStream. Given
   * that the bytes are already converted to BigEndian when they are written out
   * to a file, we no longer need to convert little endian arrays so byte order
   * can be ignored.
   *
   * It is a usage error if bitLengthFrom1 is not greater than or equal to 1.
   *
   */
  def putByteArray(
    ba: Array[Byte],
    bitLengthFrom1: Long,
    finfo: FormatInfo,
    ignoreByteOrder: Boolean = false
  ): Boolean

  /**
   * Float and Double
   * <p>
   * These are unparsed per the currently set BinaryFloatRep, byteOrder, and bitOrder
   * <p>
   * Returns false if there are not 32 bits or 64 bits (respectively) available.
   */
  def putBinaryFloat(v: Float, finfo: FormatInfo): Boolean
  def putBinaryDouble(v: Double, finfo: FormatInfo): Boolean

  /**
   * Returns number of characters transferred. Stops when the bitLimit is
   * encountered if one is defined.
   */
  def putString(str: String, finfo: FormatInfo): Long
  def putCharBuffer(cb: CharBuffer, finfo: FormatInfo): Long

  /**
   * close-out this output stream. No more writing to this after.
   */
  def setFinished(finfo: FormatInfo): Unit
  def isFinished: Boolean

  /**
   * True once this DOS's own position/content can never change again:
   * either finished, or direct and merged/flushed away without ever
   * passing through the finished state. Callers that only care whether
   * it's safe to treat this DOS's data as permanent should use this.
   */
  def isFinishedOrDead: Boolean

  /**
   * Registers/deregisters a callback for one of this DOS's facts
   * settling into its final value; see DataOutputStreamEventListener
   * for what a listener does with that notification.
   */
  def registerListener(l: DataOutputStreamEventListener): Unit
  def removeListener(l: DataOutputStreamEventListener): Unit

  /**
   * This function deletes any temnporary files that have been generated
   */
  def cleanUp(): Unit

  def zeroLengthStatus: ZeroLengthStatus
}
