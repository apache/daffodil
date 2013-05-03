package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import java.nio.CharBuffer
import java.io.FileInputStream
import java.nio.charset.Charset
import sun.nio.cs.HistoricallyNamedCharset
import java.nio.channels.ReadableByteChannel
import java.nio.channels.Channels
import java.io.IOException
import java.nio.channels.FileChannel
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.CodingErrorAction
import java.nio.ByteBuffer
import java.io.InputStream
import scala.util.control.Breaks._
import java.nio.charset.CoderResult
import edu.illinois.ncsa.daffodil.processors.charset.USASCII7BitPackedCharset
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import edu.illinois.ncsa.daffodil.processors.charset.SupportsInitialBitOffset
import edu.illinois.ncsa.daffodil.compiler.Compiler
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.processors.charset.CharacterSetAlignmentError

/**
 * The purpose of re-implementing this class is to gain control over
 * how the StreamDecoder handles malformed input.  In DFDL we want the
 * malformed input error to be treated as the end of data.  Java's
 * StreamDecoder only ignores, replaces or treats it as an error.
 *
 * Mostly this class tries to remain true to the Java code from which it was derived
 * so as to preserve future inter-operation potential. However, some modifications
 * (for bit-level positioning) make that a lot less likely.
 */
object DFDLJavaIOStreamDecoder {

  private val DEFAULT_BYTE_BUFFER_SIZE: Int = Compiler.readerByteBufferSize.toInt

  def forInputStreamReader(in: InputStream, charset: Charset, bitOffset0to7: Int, bitLimit: Long): DFDLJavaIOStreamDecoder = {

    val myBB = ByteBuffer.allocateDirect(DFDLJavaIOStreamDecoder.DEFAULT_BYTE_BUFFER_SIZE)
    myBB.flip()

    val chan = in match {
      case fis: FileInputStream => fis.getChannel()
      case _ => Channels.newChannel(in)
    }

    new DFDLJavaIOStreamDecoder(bitOffset0to7, bitLimit, charset, myBB, in, chan)
  }

}

/**
 * The purpose of re-implementing this class is to gain control over
 * how the StreamDecoder handles malformed input.  In DFDL we want the
 * malformed input error to be treated as the end of data.  Java's
 * StreamDecoder only ignores, replaces or treats it as an error.
 *
 * Forces the decoder to REPORT on malformed input.
 *
 * We also have to implment the upper-bound aka bitLimit, and not
 * allow consumption of data past it.
 */
class DFDLJavaIOStreamDecoder private (bitOffsetWithinAByte: Int, val bitLimit: Long, var cs: Charset, var bb: ByteBuffer,
  var in: InputStream, var ch: ReadableByteChannel)
  extends java.io.Reader {

  Assert.usage(ch != null)
  Assert.usage(bitOffsetWithinAByte >= 0)
  Assert.usage(bitOffsetWithinAByte <= 7)

  val decoder = cs.newDecoder().onMalformedInput(CodingErrorAction.REPORT)

  //
  // Now we deal with bit positioning. If the decoder is capable of dealing with a starting bit offset
  // then we configure it that way. If not we insist we are byte aligned and fail if not. 
  //
  decoder match {
    case decoderWithBits: SupportsInitialBitOffset =>
      decoderWithBits.setInitialBitOffset(bitOffsetWithinAByte)
    case _ if (bitOffsetWithinAByte == 0) => // ok. Do nothing. We are luckily, aligned to a byte.
    // We're counting on the parser surrounding us to catch this error and turn it into 
    // a ParseError.
    case _ => throw new CharacterSetAlignmentError(cs.name, 8, bitOffsetWithinAByte)
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
  // Note: we're not sharing this object across threads. We're assuming one instance per, for
  // any sort of parallel execution; hence, we don't do lock.synchronized.
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
          DFDLCharCounter.incr(n)
          return n
        }
      }

      // If length is 1, then we recursively end up back here with length 2, 
      // and that's why it's not a stack overflow.
      if (len == 1) {
        // Treat single-character array reads just like read()
        val c: Int = read0
        if (c == -1) {
          return if (n == 0) -1 else {
            DFDLCharCounter.incr(n)
            n
          }
        }
        cbuf(off) = c.asInstanceOf[Char]
        return {
          DFDLCharCounter.incr(n + 1)
          n + 1
        }
      }
      val res = n + implRead(cbuf, off, off + len)
      if (res > 0) DFDLCharCounter.incr(res)
      return res
    }

  }

  override def ready: Boolean = {
    //    lock.synchronized {
    ensureOpen
    return haveLeftoverChar || implReady
    //    }
  }

  def close: Unit = {
    //    lock.synchronized {
    if (!isOpen) { return }
    implClose
    isOpen = false
    //    }
  }

  private def readBytes: Int = {
    bb.compact()
    try {
      //      if (ch != null) {
      // Read from the channel
      val n: Int = ch.read(bb)
      if (n < 0) return n
      //      } else {
      //        // Read from the input stream, and hten update the buffer
      //        val lim: Int = bb.limit()
      //        val pos: Int = bb.position()
      //        assert(pos <= lim)
      //        val rem: Int = if (pos <= lim) lim - pos else 0
      //        assert(rem > 0)
      //        val n: Int = in.read(bb.array, bb.arrayOffset() + pos, rem)
      //        if (n < 0) return n
      //        if (n == 0) throw new IOException("Underlying input stream returned zero bytes")
      //        //assert(n <= rem) : "n = " + n + ", rem = " + rem
      //        assert(n <= rem)
      //        bb.position(pos + n)
      //      }
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
        val cr: CoderResult = decoder.decode(bb, cb, eof) // decode bb to cb
        if (cr.isUnderflow) { // no more data, or not enough data to complete a character.
          if (eof) { break } // eof flag set last time around this loop, so if we get an underflow it just reinforces our eof.
          if (!cb.hasRemaining()) { break } // no more room in cb, so break out. 
          if ((cb.position() > 0) && !inReady) { break } // we've got at least one character, and we're not ready to read anymore. So break out.
          val n: Int = readBytes // try to add more bytes to the bb
          if (n < 0) { // got 0 more bytes
            eof = true // so we're at EOF.
            if ((cb.position() == 0) && (!bb.hasRemaining())) { break } // no characters and no room to get more data into bb, then breakout.
            decoder.reset() // TODO ??? why reset. Can we actually discard any decoder state (such as mid-character)?
          }
          continue = true // No continue exists in Scala
        }

        if (!continue) { // No continue exists in Scala, fabricated one
          if (cr.isOverflow) {
            assert(cb.position() > 0)
            break
          }
          Assert.invariant(cr.isMalformed)
          // DFDL Implementors:
          // The whole reason we reimplemented this code in scala is
          // to change this behavior here.
          //
          // DFDL needs decode errors to behave as if end of data was reached.
          // So instead of throw, just set eof true. 
          // cr.throwException
          eof = true
          //
          // Setting eof to true isn't enough. Because we may have successfully
          // parsed some characters already, so we'll be returning a count of 
          // that many.
          // 
          // But since we're at EOF we need to return -1 so the calling context
          // (The addMore member of Page[T] in PagedSeq.scala)
          // knows we are done and no more can be provided.
          //
          // So, we backup the byte buffer by the width of the malformed
          // stuff it has consumed, and then when the next call of this occurs
          // we'll end up here, but zero well-formed characters will have been
          // created, so below, we'll end up returning -1.
          //
          // This assert doesn't hold because bb.position() can be zero, but cr.length can be 1 or more. When the very first thing
          // are malformed, then bb does not get advanced. 
          Assert.invariant(bb.position() >= cr.length())
          bb.position(bb.position() - cr.length())
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
      if (eof) return -1 // note: we have to return -1, having decoded zero characters successfully.
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
