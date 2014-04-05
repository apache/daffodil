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

import java.io.InputStream
import java.nio.channels.ReadableByteChannel
import scala.collection.immutable.PagedSeq
import scala.collection.mutable.HashMap
import scala.util.parsing.input.OffsetPosition
import java.nio.charset.CodingErrorAction
import java.nio.ByteBuffer
import java.io.InputStreamReader
import sun.nio.cs.StreamDecoder
import java.nio.charset.Charset
import java.io.UnsupportedEncodingException
import java.nio.charset.IllegalCharsetNameException
import java.nio.charset.CharsetDecoder
import java.io.FileInputStream
import java.nio.channels.FileChannel
import java.io.IOException
import java.nio.CharBuffer
import scala.util.control.Breaks._
import java.nio.charset.CoderResult
import sun.nio.cs.HistoricallyNamedCharset
import java.nio.channels.Channels
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharSequenceReader
import edu.illinois.ncsa.daffodil.util._

//
// Convention: name index fields like bytePos or bitPos or charPos with suffixes to indicate
// zero based or 1 based. Suffixes are ...0b and ...1b respectively.
//

/**
 * Pure functional Reader[Byte] that gets its data from a DFDL.Input (aka a ReadableByteChannel)
 *
 * All reading of data ultimately comes to this layer which retrieves data on demand.
 *
 * This layer doesn't know anything about bits and bit positions like all the higher layers do.
 */
class DFDLByteReader private (psb: PagedSeq[Byte], val bytePos0b: Int = 0)
  extends scala.util.parsing.input.Reader[Byte] with Logging {

  def this(in: ReadableByteChannel) = this(PagedSeq.fromIterator(new IterableReadableByteChannel(in)), 0)

  /**
   * Note: calling this will force the entire input into memory.
   */
  def lengthInBytes: Long = psb.length

  lazy val first: Byte = psb(bytePos0b)

  lazy val rest: DFDLByteReader = new DFDLByteReader(psb, bytePos0b + 1)

  // needed because of contract from Reader superclass.
  lazy val pos: scala.util.parsing.input.Position = new DFDLBytePosition(bytePos0b)

  lazy val atEnd: Boolean = !psb.isDefinedAt(bytePos0b)

  def atPos(bytePosition0b: Int): DFDLByteReader = {
    // note: do NOT slice. That copies the psb.
    //new DFDLByteReader(psb.slice(bytePosition), 0) 
    if (bytePosition0b == bytePos0b) this // already at this position.
    else new DFDLByteReader(psb, bytePosition0b)
  }

  def getByte(bytePosition0b: Int): Byte = {
    val res = psb(bytePosition0b)
    res
  }

  def getByteArray(bytePosition0b: Int, numBytes: Int): Array[Byte] = {
    
    // It is important to sanitize the numBytes request. 
    // If the parser is off the rails, it might get gibberish for 
    // a stored length value. We don't want to allocate 
    // a giant thing (and perhaps fail when doing so), if 
    // the number is just noise.
    //
    // first let's check if the data is defined at this offset
    // 
    val requestedEndBytePos = bytePosition0b + numBytes
    val realNumBytes =
      if (psb.isDefinedAt(requestedEndBytePos)) numBytes
      else {
        //
        // The requested length is bigger than our data, 
        // so we can clamp this to the length of the data
        //
        lengthInBytes - bytePosition0b
      }
    val arr = new Array[Byte](realNumBytes.toInt)
    for (i <- 0 to (numBytes - 1)) {
      arr(i) = getByte(bytePosition0b + i)
    }
    arr
  }

  /**
   * Factory for a Reader[Char] that constructs characters by decoding them from this
   * Reader[Byte] for a specific encoding starting at a particular bit position.
   *
   * Yes, I said "bit" position. Some characters are not a full byte wide (7-bit, 6-bit, and even 5-bit
   * encodings exist)
   *
   * These are kept in the processor state for reuse.
   */
  def newCharReader(charset: Charset, bitPos: Long, bitLimit: Long): DFDLCharReader = {
    log(LogLevel.Debug, "DFDLByteReader.newCharReader for bytePos %s.", (bitPos >> 3))
    DFDLCharReader(psb, bitPos, bitLimit, charset)
  }

}

object DFDLCharReader {

  // TODO: make a specialized DFDLSingleByteCharReader for known single-byte character sets.
  // This can bypass the PagedSeq[Char] entirely.
  def apply(thePsb: PagedSeq[Byte], bitPosition: Long, bitLimit: Long, charset: Charset): DFDLCharReader = {

    Assert.usage(bitPosition <= Int.MaxValue, "bit positions are limited to 32-bit signed integer by underlying libraries.")
    val bitPos = bitPosition.toInt

    val bitOffset = bitPos & 0x7
    val bytePos = bitPos >> 3

    val is = {
      // 
      // Removed slice call: psb.slice makes a copy of the psb.
      // now passes the psb and start/end to the IteratorInputStream
      // which manages delivery of bytes one by one so as to not 
      // do any copying that isn't necessary.
      //
      val endBytePos =
        if (bitLimit == -1) -1
        // Here we want to limit the PagedSeq[Byte] via bitLimit
        // because we need to determine the ending byte position from
        // the bit limit we must divide by 8.0 (must divide by double)
        // in order to round to the appropriate byte position
        else scala.math.ceil(bitLimit / 8.0).toInt
      new IteratorInputStream(thePsb, bytePos, endBytePos)
    }

    // TODO: Why is bitLimit not working for DFDLJavaIOInputStreamReader?
    // it appears to not be implemented, why is it there at all?
    val r = DFDLJavaIOInputStreamReader(is, charset, bitOffset, bitLimit)
    // TRW - The following line was changed because the fromSource
    // method was causing the readLine method of the BufferedReader class to be
    // called.  This resulted in the loss of \n, \r and \r\n characters from the data.
    //val psc = PagedSeq.fromSource(scala.io.Source.fromInputStream(is)(codec))
    val psc = PagedSeq.fromReader(r)
    val charOffset = 0
    // val rdr = new DFDLPagedSeqCharReader(charset, bitOffset, bitLimit, psc, charOffset, thePsb)
    val rdr = new DFDLPagedSeqCharReader(charset, bitPos, bitLimit, psc, charOffset, thePsb)
    rdr
  }
}

/**
 * Our version of Reader[Char] differs slightly from the Scala-provided 
 * one in that end-of-data is signified by the first member value of 
 * (-1.toChar). (Note: Scala's Reader[Char] uses ^Z i.e., 26.toChar, but we
 * want to preserve the ability for all 256 byte values to be used.  
 *
 * This trait allows for multiple different implementations for performance
 * reasons.
 *
 * Some implementations deal with the general issue of variable-width
 * character encodings.
 *
 * Others are specialized for 1-to-1 single-byte character encodings
 * like US-ASCII or ISO-8859-1, where the mapping to unicode characters
 * is either trivial, or requires just a small lookup table.
 */

trait DFDLCharReader
  extends Reader[Char] {
  def first: Char
  def rest: DFDLCharReader
  def atEnd: Boolean
  def atCharPos(cp0b: Int): DFDLCharReader
  def atBitPos(bp0b: Long): DFDLCharReader
  def getCharsetName: String
  def characterPos: Int
  def charset: Charset
  def bitLimit: Long
  /**
   * Returns string
   */
  def getStringInChars(nChars: Int): CharSequence = {
    var next: DFDLCharReader = this
    val sb = new StringBuilder(nChars)
    1 to nChars foreach { _ =>
      if (atEnd) {
        return sb.toString
      }
      sb.append(next.first)
      next = next.rest
    }
    val str: String = sb.toString
    str
  }
}

/**
 * This is for unit tests that want to feed data from a string
 */
class DFDLUTStringReader private (rdr: Reader[Char])
  extends DFDLCharReader {
  override def source = rdr.source
  override def offset = rdr.offset
  def this(data: String) = this(new CharSequenceReader(data))
  def first = rdr.first
  def rest = new DFDLUTStringReader(rdr.rest)
  def atEnd = rdr.atEnd
  def pos = rdr.pos
  def atCharPos(cp0b: Int) = Assert.usageError("not to be used in test reader")
  def atBitPos(bp0b: Long) = Assert.usageError("not to be used in test reader")
  def getCharsetName = Assert.usageError("not to be used in test reader")
  def characterPos = Assert.usageError("not to be used in test reader")
  def charset = Assert.usageError("not to be used in test reader")
  def bitLimit = -1
}

// TODO: make this global singleton go away!
// This state should be maintained in the DataProcessor object I think.
object DFDLCharCounter {
  var count: Long = 0
  def incr(n: Long) = synchronized {
    count += n 
  }
  def getAndResetCount = synchronized {
    val c = count
    count = 0
    c
  }
}

/**
 * This is for arbitrary character sets. Uses a PagedSeq[Char] as underlying cache.
 */
class DFDLPagedSeqCharReader(charsetArg: Charset,
  val startingBitPos: Int,
  bitLimitArg: Long,
  psc: PagedSeq[Char],
  override val offset: Int,
  psb: PagedSeq[Byte])
  extends DFDLCharReader with Logging {

  Assert.usage(offset >= 0)
  Assert.usage(startingBitPos >= 0)

  val charset = charsetArg
  val bitLimit = bitLimitArg

  override lazy val source: CharSequence = psc

  def first: Char = {
    if (atEnd) -1.toChar
    else {
      val char = psc(offset)
      char
    }
  }

  def rest: DFDLCharReader =
    if (psc.isDefinedAt(offset)) new DFDLPagedSeqCharReader(charset, startingBitPos, bitLimit, psc, offset + 1, psb)
    else this

  def atEnd: Boolean = !psc.isDefinedAt(offset)

  def pos: scala.util.parsing.input.Position = new OffsetPosition(source, offset) //new DFDLCharPosition(offset)

  override def drop(n: Int): DFDLCharReader = new DFDLPagedSeqCharReader(charset, startingBitPos, bitLimit, psc, offset + n, psb)

  def atCharPos(characterPos: Int): DFDLCharReader = {
    if (characterPos == this.characterPos) this
    else new DFDLPagedSeqCharReader(charset, startingBitPos, bitLimit, psc, characterPos, psb)
  }

  // We really want to be able to ask for a CharReader starting at said bitPos
  def atBitPos(bitPos: Long): DFDLCharReader = {
    log(LogLevel.Debug, "creating new DFDLCharReader.atBytePos(%s)", (bitPos >> 3))
    new DFDLPagedSeqCharReader(charset, startingBitPos = bitPos.toInt, bitLimit, psc, characterPos, psb)
  }

  def getCharsetName: String = charset.name()

  def characterPos: Int = offset

  // def isDefinedAt(charPos : Int) : Boolean = psc.isDefinedAt(charPos)

  def print: String = {
    "DFDLCharReader - " + source.length() + ": " + source + "\nDFDLCharReader - " + characterPos + ": " + source.subSequence(characterPos, source.length())
  }

  override def toString = {
    "DFDLCharReader starting at bitPos " + startingBitPos + " charPos " + characterPos + " bitLimit " + bitLimit
  }
  
}

// Scala Reader stuff is not consistent about whether it is generic over the element type, 
// or specific to Char. We want to have a Reader like abstraction that is over bytes, but 
// be able to create real Reader[Char] from it at any byte position.

object IterableReadableByteChannel {
  private var byteCount: Long = 0
  def getAndResetCalls = synchronized {
    val res = byteCount
    byteCount = 0
    res
  }
  def increment = synchronized {
    byteCount = byteCount + 1
  }
}

/**
 * All this excess buffering layer for lack of a way to convert a ReadableByteChannel directly into
 * a PagedSeq. We need an Iterator[Byte] first to construct a PagedSeq[Byte].
 */
class IterableReadableByteChannel(rbc: ReadableByteChannel)
  extends scala.collection.Iterator[Byte] {

  private final val bufferSize = 10000
  private var currentBuf: java.nio.ByteBuffer = _
  private var sz: Int = _

  private def advanceToNextBuf() {
    currentBuf = java.nio.ByteBuffer.allocate(bufferSize)
    sz = rbc.read(currentBuf)
    currentBuf.flip()
  }

  advanceToNextBuf()

  def hasNext(): Boolean = {
    if (sz == -1) return false
    if (currentBuf.hasRemaining()) return true
    advanceToNextBuf()
    if (sz == -1) return false
    if (currentBuf.hasRemaining()) return true
    return false
  }

  var pos: Int = 0

  def next(): Byte = {
    if (!hasNext()) throw new IndexOutOfBoundsException(pos.toString)
    pos += 1
    IterableReadableByteChannel.increment
    currentBuf.get()
  }
}

/**
 * Scala's Position is document oriented in that it is 1-based indexing and assumes
 * line numbers and column numbers.
 *
 */
class DFDLBytePosition(i: Int) extends scala.util.parsing.input.Position {
  def line = 1
  def column = i + 1
  // IDEA: could we assume a 'line' of bytes is 32 bytes because those print out nicely as 
  // as in HHHHHHHH HHHHHHHH ... etc. on a 72 character line?
  // Could come in handy perhaps. 
  val lineContents = "" // unused. Maybe this should throw. NoSuchOperation, or something.
}

/**
 * Position in a character stream.
 *
 * We ignore line/column structure. It's all one "line" as far as we are concerned.
 */
class DFDLCharPosition(i: Int) extends scala.util.parsing.input.Position {
  def line = 1
  def column = i + 1
  val lineContents = "" // unused
}

/**
 * Whole additional layer of byte-by-byte because there's no way to create
 * a Source (of Char) from a Seq[Byte]. Instead we have to take our
 * PagedSeq[Byte] to an Iterator, create an InputStream from the Iterator,
 * and create a Source (of Char) from that.
 *
 * Convert an iterator of bytes into an InputStream
 */

object IteratorInputStream {
  var calls: Long = 0 // instrumentation for performance analysis.
  def getAndResetCalls: Long = {
    val c = calls
    calls = 0
    c
  }
}

class IteratorInputStream(psb: PagedSeq[Byte], startBytePos0b: Int, endBytePos0b: Int)
  extends InputStream with Logging {

  log(LogLevel.Debug, "Creating an IteratorInputStream. This should happen only once per DataProcessor.parse call")
  var currentBytePos0b: Int = startBytePos0b

  def read(): Int = {
    if (currentBytePos0b == endBytePos0b
      || !psb.isDefinedAt(currentBytePos0b)) -1
    else {
      IteratorInputStream.calls += 1
      val byte = psb(currentBytePos0b)
      // Signed byte comes back. Character code 0xFF or 255 comes back here as -1 since this is a PagedSeq[Byte]
      // So, if it's a negative byte, convert to a positive int.
      val res: Int = if (byte < 0) byte + 256 else byte
      currentBytePos0b += 1
      res
    }
  }

}
