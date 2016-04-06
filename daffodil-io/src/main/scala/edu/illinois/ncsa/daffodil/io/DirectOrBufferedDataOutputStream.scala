package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import passera.unsigned.ULong

/**
 * This simple extension just gives us a public method for access to the underlying byte array.
 * That way we don't have to make a copy just to access the bytes.
 */
private[io] class ByteArrayOutputStreamWithGetBuf() extends java.io.ByteArrayOutputStream {
  def getBuf() = buf

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
        // nothing following
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
    ???
  }

  final override protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    ???
  }

  final override protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    ???
  }
}

object DirectOrBufferedDataOutputStream {

  /**
   * This is over here to be sure it isn't operating on other members
   * of the object. This operates on the arguments only.
   */
  private def deliverBufferContent(newDirectDOS: DirectOrBufferedDataOutputStream, bufDOS: DirectOrBufferedDataOutputStream) = {
    val ba = bufDOS.bufferingJOS.getBuf
    val bufferNBits = bufDOS.relBitPos0b // don't have to subtract a starting offset. It's always zero in buffered case.
    val realBitPos0b = newDirectDOS.relBitPos0b

    newDirectDOS.withBitLengthLimit(bufferNBits.toLong) {
      //
      // TODO/FIXME : This must transfer any fractional byte as well in case
      // the last bit is not on a byte boundary.
      //
      // Temporary restrictions
      Assert.notYetImplemented(realBitPos0b % 8 != 0)
      Assert.notYetImplemented(bufferNBits % 8 != 0)

      val nBytes = (bufferNBits / 8).toInt
      newDirectDOS.putBytes(ba, 0, nBytes)
    }
  }

  def apply(realDOS: java.io.OutputStream) = {
    val dbdos = new DirectOrBufferedDataOutputStream()
    dbdos.setJavaOutputStream(realDOS)
    dbdos
  }
}
