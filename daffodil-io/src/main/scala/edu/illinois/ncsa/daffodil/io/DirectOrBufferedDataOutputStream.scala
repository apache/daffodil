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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import passera.unsigned.ULong
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.util.LogLevel

/**
 * This simple extension just gives us a public method for access to the underlying byte array.
 * That way we don't have to make a copy just to access the bytes.
 */
private[io] class ByteArrayOutputStreamWithGetBuf() extends java.io.ByteArrayOutputStream {
  def getBuf() = buf
  def getCount() = count

  def toDebugContent = {
    val content = toString("iso-8859-1")
    val s = Misc.remapControlsAndLineEndingsToVisibleGlyphs(content)
    s
  }
}

/**
 * To support dfdl:outputValueCalc, we must suspend output. This is done by
 * taking the current "direct" output, and splitting it into a still direct part, and
 * a following buffered output.
 *
 * The direct part waits for the OVC calculation to complete, when that is written,
 * it is finished and collapses into the following, which was buffered, but becomes direct
 * as a result of this collapsing.
 *
 * Hence, most output will be to direct data output streams, with some, while an OVC
 * is pending, will be buffered, but this is eliminated as soon as possible.
 *
 * A Buffered DOS can be finished or not. Not finished means that it might still be
 * appended to. Not concurrently, but by other code invoked from this thread of
 * control (which might traverse different co-routine "stack" threads, but it's still
 * one thread of control).
 *
 * Finished means that the Buffered DOS can never be appended to again.
 *
 * Has two modes of operation, buffering or direct. When buffering, all output goes into a
 * buffer. When direct, all output goes into a "real" DataOutputStream.
 *
 */
final class DirectOrBufferedDataOutputStream private[io] (var splitFrom: DirectOrBufferedDataOutputStream)
  extends DataOutputStreamImplMixin {
  type ThisType = DirectOrBufferedDataOutputStream

  /**
   * Must be val, as split-from will get reset to null as streams
   * are morphed into direct streams.
   */
  val id: Int = if (splitFrom == null) 0 else splitFrom.id + 1

  /**
   * Two of these are equal if they are eq.
   * This matters because we compare them to see if we are making forward progress
   */
  override def equals(other: Any) = AnyRef.equals(other)

  override def hashCode() = AnyRef.hashCode()

  override def toString = {
    lazy val buf = bufferingJOS.getBuf()
    lazy val max16ByteArray = buf.slice(0, 16)
    lazy val upTo16BytesInHex = Misc.bytes2Hex(max16ByteArray)
    val toDisplay = "DOS(id=" + id + ", " + dosState +
      (if (isBuffering) ", Buffered" else ", Direct") +
      (if (maybeAbsBitPos0b.isDefined) {
        val srt = if (isDirect) 0 else maybeAbsStartingBitPos0b.get
        val end = maybeAbsBitPos0b.get
        val len = ULong(end - srt).longValue
        " Absolute from %d to %d (length %d)".format(srt, end, len)
      } else {
        if (splitFrom ne null)
          " at rel bit pos %d".format(relBitPos0b.longValue)
        else
          " at rel bit pos %d".format(relBitPos0b.longValue)
      }) +
      (if (maybeAbsBitLimit0b.isDefined) {
        " limit %d.".format(maybeAbsBitLimit0b.get)
      } else if (maybeRelBitLimit0b.isDefined) {
        " length limit %d.".format(maybeRelBitLimit0b.get)
      } else "") +
      (if (isBuffering) ", data=" + upTo16BytesInHex else "") +
      (if (_following.isEmpty) " no following" else "") +
      ")"
    toDisplay
  }

  def findFirstBlocking: DirectOrBufferedDataOutputStream = {
    if (maybeAbsBitPos0b.isEmpty || !isFinished) this
    else {
      Assert.invariant(this.maybeAbsBitPos0b.isEmpty)
      Assert.invariant(this.splitFrom ne null)
      splitFrom.findFirstBlocking
    }
  }
  /**
   * When in buffering mode, this is the buffering device.
   *
   * If reused, this must be reset.
   */
  private val bufferingJOS = new ByteArrayOutputStreamWithGetBuf()

  /**
   * Returns a byte buffer containing all the whole bytes that have been buffered.
   *  Does not contain any bits of the fragment byte (if there is one).
   */
  def getByteBuffer = {
    Assert.usage(isBuffering)
    val bb = ByteBuffer.wrap(bufferingJOS.getBuf())
    bb.limit(bufferingJOS.getCount)
    bb
  }

  /**
   * Switched to point a either the buffering or direct java output stream in order
   * to change modes from buffering to direct (and back if these objects get reused.)
   */
  private var _javaOutputStream: java.io.OutputStream = bufferingJOS

  private[io] final def isBuffering: Boolean = {
    val res = getJavaOutputStream() _eq_ bufferingJOS
    res
  }

  override def setJavaOutputStream(newOutputStream: java.io.OutputStream) {
    Assert.usage(newOutputStream ne null)
    _javaOutputStream = newOutputStream
    Assert.usage(newOutputStream ne bufferingJOS) // these are born buffering, and evolve into direct.
  }

  override def getJavaOutputStream() = {
    Assert.usage(_javaOutputStream ne null)
    _javaOutputStream
  }

  /**
   * Refers to the next DOS the contents of which will follow the contents of this DOS in the output.
   *
   * Note that an alignment region may be inserted first if the next DOS has an alignment requirement.
   */
  private var _following: Maybe[DirectOrBufferedDataOutputStream] = Nope

  /**
   * Provides a new buffered data output stream. Note that this must
   * be completely configured (byteOrder, encoding, bitOrder, etc.)
   */
  def addBuffered: DirectOrBufferedDataOutputStream = {
    Assert.usage(_following.isEmpty)
    val newBufStr = new DirectOrBufferedDataOutputStream(this)
    _following = One(newBufStr)
    //
    // TODO: PERFORMANCE: This is very pessimistic. It's making a complete clone of the state
    // just in case after an outputValueCalc element we go off for a long time and lots of things
    // change about these format settings.
    //
    // Really the expected case is that an OVC element and an IVC element form pairs. Often they'll
    // be adjacent elements even, and it's very unlikely that any of the format properties vary as we
    // go from the OVC element to the most distant element the OVC expression references
    //
    // So algorithmically, we'd like to share the DataOutputStream state, and UState, and split so they
    // can differ only if we need to.
    //
    // Seems we need one more indirection to the state, so that we can share it, but on any write operation, we
    // can split it by copying, and then change our indirection pointer to the copy, and then modify that.
    //
    newBufStr.assignFrom(this)
    newBufStr.resetAllBitPos()
    val savedBP = relBitPos0b.toLong
    if (maybeRelBitLimit0b.isDefined) newBufStr.setMaybeRelBitLimit0b(MaybeULong(maybeRelBitLimit0b.get - savedBP))
    newBufStr
  }

  /**
   * A buffering stream, when preceded by a direct stream, can become a
   * direct stream when the preceding direct stream is finished.
   */
  private def convertToDirect(oldDirectDOS: ThisType) {
    Assert.usage(isBuffering)
    Assert.usage(oldDirectDOS.isDirect)

    setJavaOutputStream(oldDirectDOS.getJavaOutputStream)
    Assert.invariant(isDirect)
    this.setAbsStartingBitPos0b(ULong(0))

    Assert.invariant(oldDirectDOS.maybeAbsStartingBitPos0b.isDefined)

    // Preserve the bit limit
    val mabl = oldDirectDOS.maybeAbsBitLimit0b
    val absLargerLimit = math.max(mabl.getOrElse(0L),
      maybeAbsBitLimit0b.getOrElse(0L))
    if (mabl.isDefined || maybeAbsBitLimit0b.isDefined) {
      val newRelLimit = absLargerLimit - this.maybeAbsStartingBitPos0b.get
      this.setMaybeRelBitLimit0b(MaybeULong(newRelLimit))
    }

    // after the old bufferedDOS has been completely written to the
    // oldDirectDOS, there may have been a fragment byte left over. We must
    // copy that fragment byte to the new directDOS
    this.setFragmentLastByte(oldDirectDOS.fragmentLastByte, oldDirectDOS.fragmentLastByteLimit)

    // lastly, as the direct stream, we no longer have a splitFrom that we look back at.
    this.splitFrom = null

    Assert.invariant(isDirect)
  }

  override def setFinished() {
    Assert.usage(!isFinished)
    // if we are direct, and there's a buffer following this one
    //
    // we know it isn't finished (because of flush() above)
    //
    // It must take over being the direct one.
    //
    if (isDirect) {
      var directStream = this
      var keepMerging = true
      while (directStream._following.isDefined && keepMerging) {
        val first = directStream._following.get
        keepMerging = first.isFinished // continue until AFTER we merge forward into the first non-finished successor
        Assert.invariant(first.isBuffering)

        log(LogLevel.Debug, "merging direct DOS %s into DOS %s", directStream, first)
        val dabp = directStream.maybeAbsBitPos0b.getULong
        if (first.maybeAbsStartingBitPos0b.isEmpty) {
          first.setAbsStartingBitPos0b(dabp)
        }

        DirectOrBufferedDataOutputStream.deliverBufferContent(directStream, first) // from first, into direct stream's buffers
        // so now the first one is an EMPTY not necessarily a finished buffered DOS
        //
        first.convertToDirect(directStream) // first is now the direct stream
        directStream.setDOSState(Uninitialized) // old direct stream is now dead
        directStream = first // long live the new direct stream!
        log(LogLevel.Debug, "New direct DOS %s", directStream)

      }
      if (directStream._following.isDefined) {
        Assert.invariant(!keepMerging) // we stopped because we merged forward into an active stream.
        // that active stream isn't finished
        Assert.invariant(directStream.isActive)
        // we still have a following stream, but it might be finished or might still be active.
        Assert.invariant(directStream._following.get.isActive ||
          directStream._following.get.isFinished)
      } else {
        // nothing following, so we're setting finished at the very end of everything.
        // However, the last thing we merged forward into may or may not be finished.
        // So you can setFinished() on a stream, that stream becomes dead (state uninitialized),
        // and the stream it merges forward into remains active. Funny, but no stream ends up in state "finished".
        if (keepMerging) {
          // the last stream we merged into was finished. So we're completely done.
          // flush the final frag byte if there is one.
          if (directStream.cst.fragmentLastByteLimit > 0) {
            // must not omit the fragment byte on the end.
            directStream.getJavaOutputStream().write(directStream.cst.fragmentLastByte)
            directStream.cst.setFragmentLastByte(0, 0) // zero out so we don't end up thinking it is still there.
          }
          directStream.setDOSState(Uninitialized) // not just finished. We're dead now.
        } else {
          // the last stream we merged forward into was not finished.
          Assert.invariant(directStream.isActive)
        }
      }
      // that ends everything for a direct stream being set finished.
    } else {
      Assert.invariant(isBuffering)
      //
      // setFinished() on a unfinished buffered DOS
      // we want to become read-only. So that after the
      // setFinished, any bugs if someone still tries to
      // operate on this, are caught.
      //
      // However, we don't merge forward, because that involves copying the bytes
      // and we want to do that exactly once, which is when the direct DOS "catches up"
      // and merges itself forward into all the buffered streams.
      //
      // But, we do need to propagate information about the absolute position
      // of buffers.
      //
      setDOSState(Finished)

      if (_following.isDefined) {
        val f = _following.get
        f.maybeAbsBitPos0b // requesting this pulls the absolute position info forward.
      }
    }
  }

  /**
   * This override implements a critical behavior, which is that when we ask for
   * an absolute bit position, if we have it great. if we don't, we look at the
   * prior DOS to see if it is finished and has an absolute bit position. If so
   * that bit position becomes this DOS abs starting bit position, and then our
   * absolute bit position is known.
   *
   * Without this behavior, it's possible for the unparse to hang, with every
   * DOS chained together, but they all get finished in just the wrong order,
   * and so the content or value length of something late in the data can't be
   * determined that is needed to determine something early in the schema.
   * Unless this absolute position information is propagated forward, everything
   * can hang.
   *
   * Recursively this reaches backward until it finds a non-finished DOS or one
   * that doesn't have absolute positioning information.
   *
   * I guess worst case this is a bad algorithm in that this could recurse
   * deeply, going all the way back to the very start, over and over again.
   * A better algorithm would depend on forward push of the absolute positioning
   * information when setFinished occurs, which is, after all, the time when we
   * can push such info forward.
   *
   * However, see setFinished comment that nothing is doing this push forward.
   */
  override def maybeAbsBitPos0b: MaybeULong = {
    val mSuper = super.maybeAbsBitPos0b
    if (mSuper.isDefined)
      mSuper
    else if (splitFrom eq null) MaybeULong.Nope
    else {
      val prior = this.splitFrom
      Assert.invariant(prior ne null)
      Assert.invariant(prior._following.isDefined)
      Assert.invariant(prior._following.get eq this)
      if (prior.isFinished) {
        // The prior is a finished DOS. If it (recursively) has a maybeAbsBitPos0b,
        // then since it is finished, we can compute ours and save it.
        val pmabp = prior.maybeAbsBitPos0b
        if (pmabp.isDefined) {
          val pabp = pmabp.getULong
          this.setAbsStartingBitPos0b(pabp)
          log(LogLevel.Debug, "for %s propagated absolute starting bit pos %s\n", this, pabp.toString)
          super.maybeAbsBitPos0b // will get the right value this time.
        } else {
          // prior doesn't have an abs bit pos.
          MaybeULong.Nope
        }
      } else {
        // prior is not finished, so we don't know where we start yet
        // and so can't compute an absolute bit pos yet.
        MaybeULong.Nope
      }
    }
  }

  final override protected def putLong_BE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    // Note: we don't have to check for bit limit. That check was already done.
    //
    // steps are
    // add bits to the fragmentByte (if there is one)
    // if the fragmentByte is full, write it.
    // so now there is no fragment byte
    // if we have more bits still to write, then
    // do we have a multiple of 8 bits left (all whole bytes) or are we going to have a final fragment byte?
    // shift long until MSB is first bit to be output
    // for all whole bytes, take most-significant byte of the long, and write it out. shift << 8 bits
    // set the fragment byte to the remaining most significant byte.
    var nBitsRemaining = bitLengthFrom1To64
    val mask = if (bitLengthFrom1To64 == 64) -1.toLong else (1.toLong << bitLengthFrom1To64) - 1
    var bits = signedLong & mask

    if (fragmentLastByteLimit > 0) {
      //
      // there is a frag byte, to which we are writing first.
      // We will write at least 1 bit to the frag.
      //
      val nFragBitsAvailableToWrite = 8 - fragmentLastByteLimit
      val nBitsOfFragToBeFilled =
        if (bitLengthFrom1To64 >= nFragBitsAvailableToWrite) nFragBitsAvailableToWrite
        else bitLengthFrom1To64
      val nFragBitsAfter = fragmentLastByteLimit + nBitsOfFragToBeFilled // this can be 8 if we're going to fill all of the frag.

      val bitsToGoIntoFrag = bits >> (bitLengthFrom1To64 - nBitsOfFragToBeFilled)
      val bitsToGoIntoFragInPosition = bitsToGoIntoFrag << (8 - nFragBitsAfter)

      val newFragByte = Bits.asUnsignedByte(fragmentLastByte | bitsToGoIntoFragInPosition)
      Assert.invariant(newFragByte <= 255 && newFragByte >= 0)

      val shift1 = 64 - (bitLengthFrom1To64 - nBitsOfFragToBeFilled)
      bits = (bits << shift1) >>> shift1
      nBitsRemaining = bitLengthFrom1To64 - nBitsOfFragToBeFilled

      if (nFragBitsAfter == 8) {
        // we filled the entire frag byte. Write it out, then zero it
        realStream.write(newFragByte.toByte)
        setFragmentLastByte(0, 0)
      } else {
        // we did not fill up the frag byte. We added bits to it (at least 1), but
        // it's not filled up yet.
        setFragmentLastByte(newFragByte.toInt, nFragBitsAfter)
      }

    }
    // at this point we have bits and nBitsRemaining

    Assert.invariant(nBitsRemaining >= 0)
    if (nBitsRemaining == 0)
      true // we are done
    else {
      // we have more bits to write. Could be as many as 64 still.
      Assert.invariant(fragmentLastByteLimit == 0) // there is no frag byte.
      val nWholeBytes = nBitsRemaining / 8
      val nFragBits = nBitsRemaining % 8

      // we want to shift the bits so that the 1st byte is in 0xFF00000000000000 position.
      val shift = 64 - nBitsRemaining
      var shiftedBits = bits << shift

      var i = 0
      while (i < nWholeBytes) {
        val byt = shiftedBits >>> 56
        Assert.invariant(byt <= 255)
        realStream.write(byt.toByte)
        shiftedBits = shiftedBits << 8
        i += 1
      }
      if (nFragBits > 0) {
        val newFragByte = shiftedBits >>> 56
        setFragmentLastByte(newFragByte.toInt, nFragBits)
      }
      true
    }
  }

  final override protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    // Note: we don't have to check for bit limit. That check was already done.
    //
    // LE_MSBF is most complicated of all.
    // Frag byte contents must be shifted to MSB position
    // But we take MSBs of the least-significant byte of the signedLong to put into that FragByte.

    var bits = signedLong
    //
    // The long we're writing has a last byte (from byteOrder LittleEndian perspective).
    // If this last byte is partial, we have to shift left to put the bits in the MSBs of
    // that byte, since we're storing data MSBF.
    //
    val nWholeBytesAtStart = bitLengthFrom1To64 / 8
    val nUsedBitsLastByte = (bitLengthFrom1To64 % 8)
    val nUnusedBitsLastByte = if (nUsedBitsLastByte == 0) 0 else 8 - nUsedBitsLastByte
    val indexOfLastByteLE = nWholeBytesAtStart - (if (nUnusedBitsLastByte > 0) 0 else 1)

    unionLongBuffer.put(0, bits)
    Bits.reverseBytes(unionByteBuffer)

    // bytes are now in unionByteBuffer in LE order

    val lastByte = unionByteBuffer.get(indexOfLastByteLE) // last byte is the most significant byte
    val newLastByte = ((lastByte << nUnusedBitsLastByte) & 0xFF).toByte
    unionByteBuffer.put(indexOfLastByteLE, newLastByte)

    //
    // bytes of the number are now in LE order, but with bits MSBF
    //
    var nBitsOfFragToBeFilled = 0

    if (fragmentLastByteLimit > 0) {
      //
      // there is a frag byte, to which we are writing first.
      // We will write at least 1 bit to the frag.
      //
      val nFragBitsAvailableToWrite = 8 - fragmentLastByteLimit

      // the bits we're writing might not fill the frag, so the number
      // we will fill is the lesser of the size of available space in the frag, and the bitLength argument.
      nBitsOfFragToBeFilled =
        if (bitLengthFrom1To64 >= nFragBitsAvailableToWrite) nFragBitsAvailableToWrite
        else bitLengthFrom1To64

      val nFragBitsAfter = fragmentLastByteLimit + nBitsOfFragToBeFilled // this can be 8 if we're going to fill all of the frag.

      // Now get the bits that will go into the frag, from the least significant (first) byte.
      val newFragBitsMask = 0x80.toByte >> (nBitsOfFragToBeFilled - 1)
      val LSByte = unionByteBuffer.get(0)
      val bitsToGoIntoFragInPosition = ((LSByte & newFragBitsMask) >>> fragmentLastByteLimit).toInt

      val newFragByte = Bits.asUnsignedByte((fragmentLastByte | bitsToGoIntoFragInPosition).toByte)
      Assert.invariant(newFragByte <= 255 && newFragByte >= 0)

      if (nFragBitsAfter == 8) {
        // we filled the entire frag byte. Write it out, then zero it
        realStream.write(newFragByte.toByte)
        setFragmentLastByte(0, 0)
      } else {
        // we did not fill up the frag byte. We added bits to it (at least 1), but
        // it's not filled up yet.
        setFragmentLastByte(newFragByte, nFragBitsAfter)
      }

      //
      // Now we have to remove the bits that went into the
      // current frag byte
      //
      // This is a strange operation. Were creating a long from the littleEndian bytes.
      // The value of this will be very strange, but shifting left moves bits from more significant
      // bytes into less significant bytes,
      bits = unionLongBuffer.get(0)
      bits = bits << nBitsOfFragToBeFilled
      unionLongBuffer.put(0, bits)

    }
    //
    // now we have the unionByteBuffer containing the correct LE bytes, in LE order.
    //
    val bitLengthRemaining = bitLengthFrom1To64 - nBitsOfFragToBeFilled
    Assert.invariant(bitLengthRemaining >= 0)

    if (bitLengthRemaining > 0) {
      val nWholeBytesNow = bitLengthRemaining / 8
      val nBitsInFinalFrag = bitLengthRemaining % 8
      val indexOfFinalFragByte = nWholeBytesNow

      var i = 0
      while (i < nWholeBytesNow) {
        realStream.write(unionByteBuffer.get(i))
        i += 1
      }
      if (nBitsInFinalFrag > 0) {
        val finalFragByte = Bits.asUnsignedByte(unionByteBuffer.get(indexOfFinalFragByte))
        setFragmentLastByte(finalFragByte, nBitsInFinalFrag)
      }
    }
    true
  }

  final override protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    // Note: we don't have to check for bit limit. That check was already done.
    //
    // Interestingly, LE_LSBF is slightly simpler than BE_MSBF as we don't have to shift bytes to get the
    // bits into MSBF position.
    //
    // steps are
    // add bits to the fragmentByte (if there is one)
    // if the fragmentByte is full, write it.
    // so now there is no fragment byte
    // if we have more bits still to write, then
    // do we have a multiple of 8 bits left (all whole bytes) or are we going to have a final fragment byte?
    // for all whole bytes, take least-significant byte of the long, and write it out. shift >> 8 bits
    // set the fragment byte to the remaining most significant byte.
    var nBitsRemaining = bitLengthFrom1To64
    var bits = signedLong

    if (fragmentLastByteLimit > 0) {
      //
      // there is a frag byte, to which we are writing first.
      // We will write at least 1 bit to the frag.
      //
      val nFragBitsAvailableToWrite = 8 - fragmentLastByteLimit
      val nBitsOfFragToBeFilled =
        if (bitLengthFrom1To64 >= nFragBitsAvailableToWrite) nFragBitsAvailableToWrite
        else bitLengthFrom1To64
      val nFragBitsAfter = fragmentLastByteLimit + nBitsOfFragToBeFilled // this can be 8 if we're going to fill all of the frag.

      val fragLastByteMask = 0xFF >> (8 - nFragBitsAfter)
      val bitsToGoIntoFragInPosition = ((bits << fragmentLastByteLimit) & fragLastByteMask).toInt

      val newFragByte = fragmentLastByte | bitsToGoIntoFragInPosition
      Assert.invariant(newFragByte <= 255 && newFragByte >= 0)

      bits = bits >>> nBitsOfFragToBeFilled
      nBitsRemaining = bitLengthFrom1To64 - nBitsOfFragToBeFilled

      if (nFragBitsAfter == 8) {
        // we filled the entire frag byte. Write it out, then zero it
        realStream.write(newFragByte.toByte)
        setFragmentLastByte(0, 0)
      } else {
        // we did not fill up the frag byte. We added bits to it (at least 1), but
        // it's not filled up yet.
        setFragmentLastByte(newFragByte, nFragBitsAfter)
      }

    }
    // at this point we have bits and nBitsRemaining

    Assert.invariant(nBitsRemaining >= 0)
    if (nBitsRemaining == 0)
      true // we are done
    else {
      // we have more bits to write. Could be as many as 64 still.
      Assert.invariant(fragmentLastByteLimit == 0) // there is no frag byte.
      val nWholeBytes = nBitsRemaining / 8
      val nFragBits = nBitsRemaining % 8
      val fragUsedBitsMask = ((1 << nFragBits) - 1)

      var shiftedBits = bits

      var i = 0
      while (i < nWholeBytes) {
        val byt = shiftedBits & 0xFF
        realStream.write(byt.toByte)
        shiftedBits = shiftedBits >>> 8
        i += 1
      }
      if (nFragBits > 0) {
        val newFragByte = Bits.asUnsignedByte((shiftedBits & fragUsedBitsMask).toByte)
        setFragmentLastByte(newFragByte, nFragBits)
      }
      true
    }
  }
}

/**
 * Throw to indicate that bitOrder changed, but not on a byte boundary.
 *
 * Must be caught at higher level and turned into a RuntimeSDE.
 */
class BitOrderChangeException(directDOS: DirectOrBufferedDataOutputStream,
  bufDOS: DirectOrBufferedDataOutputStream) extends Exception with ThinThrowable

object DirectOrBufferedDataOutputStream {

  /**
   * This is over here to be sure it isn't operating on other members
   * of the object. This operates on the arguments only.
   *
   * Delivers the bits of bufDOS into directDOS's output stream. Deals with the possibility that
   * the directDOS ends with a fragment byte, or the bufDOS does, or both.
   */
  private def deliverBufferContent(directDOS: DirectOrBufferedDataOutputStream, bufDOS: DirectOrBufferedDataOutputStream) {
    Assert.invariant(bufDOS.isBuffering)
    Assert.invariant(!directDOS.isBuffering)

    val ba = bufDOS.bufferingJOS.getBuf
    val bufferNBits = bufDOS.relBitPos0b // don't have to subtract a starting offset. It's always zero in buffered case.

    if (bufDOS.cst.bitOrder ne directDOS.cst.bitOrder) {
      if (!directDOS.isEndOnByteBoundary) {
        //
        // If the bit order changes, it has to be on a byte boundary
        // It's simply not meaningful for it to change otherwise.
        //
        throw new BitOrderChangeException(directDOS, bufDOS)
      }
    }

    // cases
    // no fragment bytes anywhere - just take the bytes
    // fragment byte on directDOS, fragment byte on bufDOS, or both.

    directDOS.withBitLengthLimit(bufferNBits.toLong) {

      if (directDOS.isEndOnByteBoundary && bufDOS.isEndOnByteBoundary) {

        val nBytes = (bufferNBits / 8).toInt
        val nBytesPut = directDOS.putBytes(ba, 0, nBytes)
        Assert.invariant(nBytesPut == nBytes)

      } else {
        if (bufDOS.cst.fragmentLastByteLimit > 0) {
          val bufDOSStream = bufDOS.getJavaOutputStream()
          bufDOSStream.write(bufDOS.cst.fragmentLastByte.toByte)
        }

        val nBitsPut = directDOS.putBitBuffer(bufDOS.getByteBuffer, bufferNBits.toLong)
        Assert.invariant(nBitsPut == bufferNBits.toLong)
      }
    }
  }

  /**
   * Factory for creating new ones
   */
  def apply(jos: java.io.OutputStream, creator: DirectOrBufferedDataOutputStream) = {
    val dbdos = new DirectOrBufferedDataOutputStream(creator)
    dbdos.setJavaOutputStream(jos)

    if (creator eq null) {
      dbdos.setAbsStartingBitPos0b(ULong(0))
      dbdos.setAbsStartingBitPos0b(ULong(0)) // yes. We do want to call this twice.
      Assert.invariant(dbdos.isDirect)
    }
    dbdos
  }

}
