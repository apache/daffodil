package delimsearch.io

import java.nio.CharBuffer
import java.io.FileInputStream
import java.io.UnsupportedEncodingException
import java.nio.charset.Charset
import sun.nio.cs.HistoricallyNamedCharset
import java.nio.channels.ReadableByteChannel
import java.nio.channels.Channels
import java.io.IOException
import java.nio.charset.IllegalCharsetNameException
import java.nio.channels.FileChannel
import java.nio.charset.CharsetDecoder
import daffodil.exceptions.Assert
import java.nio.charset.CodingErrorAction
import java.nio.ByteBuffer
import java.io.InputStream
import scala.util.control.Breaks._
import java.nio.charset.CoderResult

/**
 * The purpose of re-implementing this class is to gain control over
 * how the StreamDecoder handles malformed input.  In DFDL we want the
 * malformed input error to be treated as the end of data.  Java's
 * StreamDecoder only ignores, replaces or treats it as an error.
 */
object DFDLJavaIOStreamDecoder {
  private val MIN_BYTE_BUFFER_SIZE: Int = 32
  private val DEFAULT_BYTE_BUFFER_SIZE: Int = 8192

  // Factories for DFDLJavaIOInputStreamReader
  def forInputStreamReader(in: InputStream, lock: Object, charsetName: String): DFDLJavaIOStreamDecoder = {

    // We should throw if csn is nil. Tolerating this would just lead to bugs.
    Assert.usage(charsetName != Nil)

    // There is no notion of a default charset in DFDL.
    // So this can be val.
    val csn: String = charsetName

    try {
      if (Charset.isSupported(csn)) { return new DFDLJavaIOStreamDecoder(in, Charset.forName(csn)) }
    } catch {
      case e: IllegalCharsetNameException => // Nothing
    }
    throw new UnsupportedEncodingException(csn)
  }

  def forInputStreamReader(in: InputStream, lock: Object, cs: Charset): DFDLJavaIOStreamDecoder = {
    return new DFDLJavaIOStreamDecoder(in, cs)
  }

  def forInputStreamReader(in: InputStream, lock: Object, dec: CharsetDecoder): DFDLJavaIOStreamDecoder = {
    return new DFDLJavaIOStreamDecoder(in, dec)
  }

  private var channelsAvailable = true

  private def getChannel(in: FileInputStream): FileChannel = {
    if (!channelsAvailable) return null
    try {
      return in.getChannel
    } catch {
      case e: UnsatisfiedLinkError => {
        channelsAvailable = false
        return null
      }
    }
  }
}

/**
 * The purpose of re-implementing this class is to gain control over
 * how the StreamDecoder handles malformed input.  In DFDL we want the
 * malformed input error to be treated as the end of data.  Java's
 * StreamDecoder only ignores, replaces or treats it as an error.
 *
 * Forces the decoder to REPORT on malformed input.
 */
class DFDLJavaIOStreamDecoder(var cs: Charset, var decoder: CharsetDecoder, var bb: ByteBuffer,
                              var in: InputStream, var ch: ReadableByteChannel)
  extends java.io.Reader {

  def this(in: InputStream, dec: CharsetDecoder) = {
    //super(lock) // Java lock/synchronizing code

    // In order for this customized version of the StreamDecoder to work
    // as we expect it, we should force the decoder to REPORT on malformed input.
    this(dec.charset(), dec.onMalformedInput(CodingErrorAction.REPORT), {
      var myBB: ByteBuffer = null
      myBB = ByteBuffer.allocateDirect(DFDLJavaIOStreamDecoder.DEFAULT_BYTE_BUFFER_SIZE)
      myBB.flip()
      myBB
    }, in, {
      if (in.isInstanceOf[FileInputStream]) in.asInstanceOf[FileInputStream].getChannel()
      else Channels.newChannel(in)
    })
  }

  // TODO: Do we want to support this constructor? Might want to just Assert.NYI for this.
  def this(in: InputStream, cs: Charset) = {
    // In this case we always want to report the error in order for us to treat a malformed
    // input as end of data.
    //
    //this(in, cs.newDecoder().onMalformedInput(CodingErrorAction.REPLACE).onUnmappableCharacter(CodingErrorAction.REPLACE))
    this(in, cs.newDecoder().onMalformedInput(CodingErrorAction.REPORT).onUnmappableCharacter(CodingErrorAction.REPORT))
  }

  @volatile
  private var isOpen: Boolean = true
  def ensureOpen = { if (!isOpen) throw new java.io.IOException("Stream closed") }

  private var haveLeftoverChar: Boolean = false
  private var leftoverChar: Char = 0

  def getEncoding: String = {
    if (isOpen) { return encodingName }
    return null
  }

  override def read: Int = {
    return read0
  }

  // Leftover char is needed because of utf-16 and the surrogate pair stuff.
  // In that encoding, we need to read two codepoints, and if they are a surrogate pair
  // synthesize one character from them.
  //
  // Presumably this happens in the decoder, but in the case where it isn't a surrogate
  // pair, we save work by returning two decoded characters, not one since we've already
  // decoded the second one. (This little optimization seems very not worth it to me, but
  // is necessary if you can't peek forward 2 bytes into the underlying bytes without 
  // consuming them.)
  //
  // Issue is that UTF-16 is really a variable-width 2-byte or 4-byte character encoding. But 
  // In the early days of unicode and java, it was treated as a fixed width 2-byte.
  //
  // Hence, in DFDL we provide control.
  // This depends on dfdl:utf16Width="variable". If dfdl:utf16Width="fixed", then 
  // we do NOT process surrogate pairs, and return them each as a separate codepoint.
  //
  // So at some point our decoder must implement utf16Width="fixed" which means it must 
  // not do this surrogate pair processing. That means either we find a feature for that
  // in CharsetDecoder (it may be there as this is a common need), or we'll have to clone
  // and reimplement that class too.
  //
  // TBD: implement utf16Width="variable". For now we can SDE on utf16Width="variable".
  //
  // This code assumes implicitly that it is ok if the byte stream has been advanced 
  // further (by one character decoding) than it would have to produce one character only.
  // 
  // So long as we're never asking this stream of characters for a byte position we're ok.
  // 
  // 
  private def read0: Int = {
    //lock.synchronized // in java this was a synchronized method
    {
      // Return the leftover char, if there is one
      if (haveLeftoverChar) {
        haveLeftoverChar = false
        return leftoverChar
      }

      // Convert more bytes
      val cb: Array[Char] = new Array[Char](2)
      val n: Int = read(cb, 0, 2)
      n match {
        case -1 => return -1
        case 2 => {
          leftoverChar = cb(1)
          haveLeftoverChar = true
          return cb(0)
        }
        case 1 => return cb(0)
        case _ => {
          // TODO: assert false : n
          return -1
        }
      }
    }
  }

  // All lengths and offsets below are in character units.
  //
  def read(cbuf: Array[Char], offset: Int, length: Int): Int = {
    var off: Int = offset
    var len: Int = length

    // lock.synchronized // in java this was a synchronized method 
    {
      ensureOpen
      if ((off < 0) || (off > cbuf.length) || (len < 0)
        || ((off + len) > cbuf.length) || ((off + len) < 0)) {
        throw new IndexOutOfBoundsException()
      }

      if (len == 0) { return 0 }
      var n: Int = 0

      if (haveLeftoverChar) {
        // Copy the leftover char into the buffer
        cbuf(off) = leftoverChar
        off += 1
        len -= 1
        haveLeftoverChar = false
        n = 1
        if ((len == 0) || !implReady) {
          // Return now if this is all we can produce w/o blocking
          return n
        }
      }

      // If length is 1, then we recursively end up back here with length 2, 
      // and that's why it's not a stack overflow.
      if (len == 1) {
        // Treat single-character array reads just like read()
        val c: Int = read0
        if (c == -1) {
          return if (n == 0) -1 else n
        }
        cbuf(off) = c.asInstanceOf[Char]
        return n + 1
      }
      return n + implRead(cbuf, off, off + len)
    }

  }

  override def ready: Boolean = {
    lock.synchronized {
      ensureOpen
      return haveLeftoverChar || implReady
    }
  }

  def close: Unit = {
    lock.synchronized {
      if (!isOpen) { return }
      implClose
      isOpen = false
    }
  }

  private def readBytes: Int = {
    bb.compact()
    try {
      if (ch != null) {
        // Read from the channel
        val n: Int = ch.read(bb)
        if (n < 0) return n
      } else {
        // Read from the input stream, and hten update the buffer
        val lim: Int = bb.limit()
        val pos: Int = bb.position()
        assert(pos <= lim)
        val rem: Int = if (pos <= lim) lim - pos else 0
        assert(rem > 0)
        val n: Int = in.read(bb.array, bb.arrayOffset() + pos, rem)
        if (n < 0) return n
        if (n == 0) throw new IOException("Underlying input stream returned zero bytes")
        //assert(n <= rem) : "n = " + n + ", rem = " + rem
        assert(n <= rem)
        bb.position(pos + n)
      }
    } finally {
      // Flip even when an IOException is thrown,
      // otherwise the stream will stutter
      bb.flip()
    }
    val rem: Int = bb.remaining()
    // assert(rem != 0) : rem
    assert(rem != 0)
    return rem
  }

  /**
   * Contains change that allows us to treat malformed data as
   * end of data.  However, this requires that the decoder is set
   * to REPORT the issue rather than IGNORE or REPLACE it.
   */
  def implRead(cbuf: Array[Char], off: Int, end: Int): Int = {
    // In order to handle surrogate pairs, this method requires that
    // the invoker attempt to read at least two characters.  Saving the
    // extra character, if any, at a higher level is easier
    // to deal with it here.
    assert(end - off > 1)
    var cb: CharBuffer = CharBuffer.wrap(cbuf, off, end - off)
    if (cb.position != 0) {
      // Ensure that cb[0] == cbuf[off]
      cb = cb.slice
    }

    var eof: Boolean = false
    var continue: Boolean = false

    breakable {
      while (true) {
        val cr: CoderResult = decoder.decode(bb, cb, eof)
        if (cr.isUnderflow) {
          if (eof) { break }
          if (!cb.hasRemaining()) { break }
          if ((cb.position() > 0) && !inReady) { break }
          val n: Int = readBytes
          if (n < 0) {
            eof = true
            if ((cb.position() == 0) && (!bb.hasRemaining())) { break }
            decoder.reset()
          }
          continue = true // No continue exists in Scala
        }

        if (!continue) { // No continue exists in Scala, fabricated one
          if (cr.isOverflow) {
            assert(cb.position() > 0)
            break
          }

          // DFDL Implementors:
          // The whole reason we reimplemented this code in scala is
          // to change this behavior here.
          //
          // DFDL needs decode errors to behave as if end of data was reached.
          // So instead of throw, just set eof true. 
          // cr.throwException
          eof = true
          break
        }
        continue = false
      }
    }

    if (eof) {
      // ## Need to flush decoder
      decoder.reset()
    }

    if (cb.position() == 0) {
      if (eof) return -1
      assert(false)
    }

    return cb.position()
  }

  def encodingName: String = {
    return if (cs.isInstanceOf[HistoricallyNamedCharset]) cs.asInstanceOf[HistoricallyNamedCharset].historicalName else cs.name()
  }

  private def inReady: Boolean = {
    try {
      return (((in != null) && (in.available() > 0)) || (ch.isInstanceOf[FileChannel])) // ## RBC.available()?
    } catch {
      case e: IOException => return false
    }
  }

  def implReady: Boolean = { return bb.hasRemaining() || inReady }

  def implClose: Unit = {
    if (ch != null) ch.close()
    else in.close()
  }

}
