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
final class DirectOrBufferedDataOutputStream private[io] ()
  extends DataOutputStreamImplMixin {
  type ThisType = DirectOrBufferedDataOutputStream

  override def toString = {
    lazy val buf = bufferingJOS.getBuf()
    lazy val max16ByteArray = buf.slice(0, 16)
    lazy val upTo16BytesInHex = Misc.bytes2Hex(max16ByteArray)
    lazy val bitPos0b = if (isBuffering) relBitPos0b.toLong else {
      Assert.invariant(st.maybeAbsBitPos0b.isDefined)
      st.maybeAbsBitPos0b.get
    }
    val toDisplay = "DOS(" + dosState +
      (if (isBuffering) ", Buffered" else ", Direct") +
      ", bitPos0b=" + bitPos0b +
      (if (st.maybeAbsBitLimit0b.isDefined) ", bitLimit0b=" + st.maybeAbsBitLimit0b.get
      else if (st.maybeRelBitLimit0b.isDefined) ", bitLimit0b=" + st.maybeRelBitLimit0b.get
      else "") +
      (if (isBuffering) ", data=" + upTo16BytesInHex else "") +
      (if (_following.isDefined) " with Following DOS" else "") +
      ")"
    toDisplay
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
    bb
  }

  /**
   * Switched to point a either the buffering or direct java output stream in order
   * to change modes from buffering to direct (and back if these objects get reused.)
   */
  private var _javaOutputStream: java.io.OutputStream = bufferingJOS

  @inline private[io] final def isBuffering: Boolean = {
    val res = getJavaOutputStream() _eq_ bufferingJOS
    res
  }

  @inline private[io] final def isDirect: Boolean = !isBuffering

  override def setJavaOutputStream(newOutputStream: java.io.OutputStream) {
    Assert.usage(newOutputStream ne null)
    _javaOutputStream = newOutputStream
    Assert.usage(newOutputStream ne bufferingJOS) // these are born buffering, and evolve into direct.
    st.maybeAbsBitPos0b = MaybeULong(0)
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
    val newBufStr = new DirectOrBufferedDataOutputStream()
    _following = One(newBufStr)
    //
    // PERFORMANCE: This is very pessimistic. It's making a complete clone of the state
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
    val savedBP = relBitPos0b.toLong
    newBufStr.setRelBitPos0b(ULong(0))
    if (maybeRelBitLimit0b.isDefined) newBufStr.st.setMaybeRelBitLimit0b(MaybeULong(maybeRelBitLimit0b.get - savedBP))
    newBufStr
  }

  /**
   * A buffering stream, when preceded by a direct stream, can become a
   * direct stream when the preceding direct stream is finished.
   */
  private def convertToDirect(oldDirectDOS: ThisType) {
    Assert.usage(isBuffering)
    setJavaOutputStream(oldDirectDOS.getJavaOutputStream)
    this.st.maybeAbsBitPos0b = oldDirectDOS.maybeAbsBitPos0b // preserve the absolute position.
    Assert.invariant(isDirect)
  }

  override def flush() {
    Assert.usage(!isFinished)

    if (isBuffering) return
    //
    // output any finished buffers - walk forward through any
    // finished buffering streams following this direct one.
    // Grab their content, and output it "for real"
    //
    while (_following.isDefined && _following.get.isFinished) {
      val following = _following.get
      DirectOrBufferedDataOutputStream.deliverBufferContent(this, following)
      following.setDOSState(Uninitialized)
      _following = following._following
    }
  }

  override def setFinished() {
    Assert.usage(!isFinished)
    flush() //
    // if we are direct, and there's a buffer following this one
    //
    // we know it isn't finished (because of flush() above)
    //
    // It must take over being the direct one.
    //
    if (isDirect) {
      if (_following.isDefined) {
        Assert.invariant(!_following.get.isFinished)
        val first = _following.get
        DirectOrBufferedDataOutputStream.deliverBufferContent(this, first)
        // so now the first one is an EMPTY not finished buffered DOS
        //
        first.convertToDirect(this)
        first.flush()
        setDOSState(Uninitialized)
      } else {
        // nothing following, so we're setting finished at the very end of everything.
        if (cst.fragmentLastByteLimit > 0)
          // must not omit the fragment byte on the end.
          this.getJavaOutputStream().write(cst.fragmentLastByte)
        setDOSState(Uninitialized) // not just finished. We're dead now.
      }
    } else if (isBuffering) {
      //
      // setFinished() on a unfinished buffered DOS
      // we want to become read-only. So that after the
      // setFinished, any bugs if someone still tries to
      // operate on this, are caught.
      setDOSState(Finished)
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
    var bits = signedLong

    if (st.fragmentLastByteLimit > 0) {
      //
      // there is a frag byte, to which we are writing first.
      // We will write at least 1 bit to the frag.
      //
      val nFragBitsAvailableToWrite = 8 - st.fragmentLastByteLimit
      val nBitsOfFragToBeFilled =
        if (bitLengthFrom1To64 >= nFragBitsAvailableToWrite) nFragBitsAvailableToWrite
        else bitLengthFrom1To64
      val nFragBitsAfter = st.fragmentLastByteLimit + nBitsOfFragToBeFilled // this can be 8 if we're going to fill all of the frag.

      val bitsToGoIntoFrag = bits >> (bitLengthFrom1To64 - nBitsOfFragToBeFilled)
      val bitsToGoIntoFragInPosition = bitsToGoIntoFrag << (8 - nFragBitsAfter)

      val newFragByte = st.fragmentLastByte | bitsToGoIntoFragInPosition.toInt
      Assert.invariant(newFragByte < 255 && newFragByte > -128)

      val shift1 = 64 - (bitLengthFrom1To64 + nBitsOfFragToBeFilled)
      bits = (bits << shift1) >>> shift1
      nBitsRemaining = bitLengthFrom1To64 - nBitsOfFragToBeFilled

      if (nFragBitsAfter == 8) {
        // we filled the entire frag byte. Write it out, then zero it
        realStream.write(newFragByte.toByte)
        st.setFragmentLastByte(0, 0)
      } else {
        // we did not fill up the frag byte. We added bits to it (at least 1), but
        // it's not filled up yet.
        st.setFragmentLastByte(newFragByte, nFragBitsAfter)
      }

    }
    // at this point we have bits and nBitsRemaining

    Assert.invariant(nBitsRemaining >= 0)
    if (nBitsRemaining == 0)
      true // we are done
    else {
      // we have more bits to write. Could be as many as 64 still.
      Assert.invariant(st.fragmentLastByteLimit == 0) // there is no frag byte.
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
        val newFragByte = Bits.asUnsignedByte((shiftedBits >> 56).toByte)
        st.setFragmentLastByte(newFragByte, nFragBits)
      }
      true
    }
  }

  final override protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    ???
  }

  final override protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    ???
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
        // there is a fragment byte on directDOS, or on bufDOS or both.
        if (bufDOS.cst.fragmentLastByteLimit > 0) {
          val bufDosStream = bufDOS.getJavaOutputStream()
          bufDosStream.write(bufDOS.cst.fragmentLastByte.toByte) // add the fragment onto the end so it's all shifted together
          bufDosStream.write(0) // add another zero byte. Gives us room to shift into when we shift by the directDOS frag length.
        }
        val bb = bufDOS.getByteBuffer
        if (directDOS.cst.fragmentLastByteLimit > 0) {
          //
          // PERFORMANCE: consider writing a one-pass here vs this shifting thing.
          // (However, we should see if this matters anyway. It's only relevant if we have
          // frag bytes involved around splits of direct and buffered. May not matter.
          //
          Bits.shiftToHigherBitPosition(bufDOS.cst.bitOrder, bb, directDOS.cst.fragmentLastByteLimit)
          // at this point we've shifted the whole bytes right
          // merge the directDOS frag byte with the first byte of bb.
          val shiftedFirstByte = bb.get(0)
          val newFirstByte = shiftedFirstByte | directDOS.cst.fragmentLastByte // works for any bitOrder
          bb.put(0, newFirstByte.toByte)
        }
        // now we have to determine whether anything was shifted into the final byte, or
        // if that one can be dropped
        val numFragmentBitsBothDOS = directDOS.cst.fragmentLastByteLimit + bufDOS.cst.fragmentLastByteLimit
        val hasBitsInLastByte = numFragmentBitsBothDOS > 8
        if (!hasBitsInLastByte) {
          bb.limit(bb.limit - 1) // trims it off the end.
        }
        val numFinalFragBits =
          if (hasBitsInLastByte) numFragmentBitsBothDOS - 8
          else numFragmentBitsBothDOS
        val finalFrag = if (numFinalFragBits > 0) {
          val finalFrag = bb.get(bb.limit - 1)
          bb.limit(bb.limit - 1) // trim frag from end of bb
          finalFrag
        } else {
          0.toByte
        }

        //
        // At this point bb contains the whole bytes, finalFrag the final frag byte,
        // and numFinalFragBits contains the count of bits in use in finalFrag
        //
        val arr = bb.array()
        val startPos = bb.position
        val len = bb.limit - bb.position
        val os = directDOS.getJavaOutputStream()
        os.write(arr, startPos, len)
        if (numFinalFragBits == 8) {
          // special case - the frag bits from direct and frag from buffered result in
          // exactly a whole byte, so we just add this byte to the direct, and there is no frag.
          os.write(finalFrag)
          directDOS.cst.setFragmentLastByte(0, 0)
        } else {
          directDOS.cst.setFragmentLastByte(finalFrag, numFinalFragBits)
        }
      }
    }
  }

  def apply(realDOS: java.io.OutputStream) = {
    val dbdos = new DirectOrBufferedDataOutputStream()
    dbdos.setJavaOutputStream(realDOS)
    dbdos
  }
}
