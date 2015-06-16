/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

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
import edu.illinois.ncsa.daffodil.exceptions._
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharSequenceReader
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import Maybe._
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
//
// Convention: name index fields like bytePos or bitPos or charPos with suffixes to indicate
// zero based or 1 based. Suffixes are ...0b and ...1b respectively.
//

///**
// * Our version of Reader[Char] differs slightly from the Scala-provided
// * one in that end-of-data is signified by the first member value of
// * (-1.toChar). (Note: Scala's Reader[Char] uses ^Z i.e., 26.toChar, but we
// * want to preserve the ability for all 256 byte values to be used.
// *
// * This trait allows for multiple different implementations for performance
// * reasons.
// *
// * Some implementations deal with the general issue of variable-width
// * character encodings.
// *
// * Others are specialized for 1-to-1 single-byte character encodings
// * like US-ASCII or ISO-8859-1, where the mapping to unicode characters
// * is either trivial, or requires just a small lookup table.
// */
//
//@deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//trait DFDLCharReader
//  extends Reader[Char] {
//  @deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//  def first: Char
//  @deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//  def rest: DFDLCharReader
//  @deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//  def atEnd: Boolean
//  @deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//  def atCharPos(cp0b: Int): DFDLCharReader
//  @deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//  def copy: DFDLCharReader
//  @deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//  def characterPos: Int
//  @deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//  def bitPos0b: Long
//
//  // needed to fulfill contract of Reader[Char]
//  override def pos: scala.util.parsing.input.Position = Assert.usageError("not to be used")
//}
//
//@deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//object DFDLCharReaderFromDataInputStream {
//  def apply(is: InStream): DFDLCharReader = {
//    val dupIs = is.duplicate() // decouples our position on the input from any other.
//    val dis = dupIs.dataInputStream
//    val markForRestart = dis.mark
//    val iter = dis.asIteratorChar
//    val cr = new DFDLCharReaderFromDataInputStream(0, iter, markForRestart, dupIs)
//    cr
//  }
//}
//
///**
// * Functional-style I/O layer (aka Reader[Char])
// * built atop non-functional DataInputStream.
// *
// * This provides compatibility for parsers written to the older InStream API until
// * converted to directly call DataInputStream methods.
// */
//@deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//class DFDLCharReaderFromDataInputStream private (var charPos0b: Int, var iter: Iterator[Char], var markForRestart: DataInputStream.Mark, var is: InStream)
//  extends DFDLCharReader {
//
//  def inStream = is
//  /**
//   * Make a fully decoupled copy.
//   */
//  def copy() = {
//    val savedCharPos0b = charPos0b
//    //
//    // Kind of twisted way to have to do this.
//    // This reset side effects the current state
//    // but we need the copy to restart the same way, so 
//    // we want a copy of the reset input stream, not
//    //
//    is.dataInputStream.reset(markForRestart)
//    markForRestart = is.dataInputStream.mark
//    var newCopy: DFDLCharReader = DFDLCharReaderFromDataInputStream(is)
//    //
//    // Now we advance it back up to where we were when copy was called.
//    newCopy = newCopy.atCharPos(savedCharPos0b)
//    //
//    // Now, because we reset the state above, we have to restore it.
//    //
//    charPos0b = 0
//    val newThis = this.atCharPos(savedCharPos0b).asInstanceOf[DFDLCharReaderFromDataInputStream] // restore our position
//    this.charPos0b = newThis.characterPos
//    this.iter = newThis.iter
//    this.markForRestart = newThis.markForRestart
//    this.is = newThis.inStream
//    //
//    // Now this instance is back where it started, 
//    // and we can return the new instance.
//    newCopy
//  }
//
//  lazy val first = if (!iter.hasNext) -1.toChar else iter.next
//  lazy val rest = {
//    val f = first
//    if (atEnd)
//      throw new NoSuchElementException("rest on empty reader")
//    else
//      new DFDLCharReaderFromDataInputStream(charPos0b + 1, iter, markForRestart, is)
//  }
//  def bitPos0b = is.dataInputStream.bitPos0b
//  def atEnd = first == -1.toChar
//
//  /**
//   * We just go back to the start, then advance characters.
//   * This kind of backtracking will be inefficient unless the distances going back are very small.
//   */
//  def atCharPos(cp0b: Int): DFDLCharReader = {
//    if (cp0b == charPos0b) return this
//    is.dataInputStream.reset(markForRestart)
//    markForRestart = is.dataInputStream.mark
//    var rdr = DFDLCharReaderFromDataInputStream(is)
//    var i = cp0b
//    while (i > 0) {
//      if (rdr.first != -1.toChar) rdr = rdr.rest
//      i -= 1
//    }
//    rdr
//  }
//
//  def atBitPos(bp0b: Long) = {
//    val newIS = is.duplicate()
//    newIS.withPos(bp0b, -1)
//    DFDLCharReaderFromDataInputStream(newIS)
//  }
//
//  def characterPos = charPos0b
//
//}
//
///**
// * This is to be used in the unparsing methods as they return
// * a string from the infoset.  This allows us to handle the
// * string as if it were a DFDLCharReader and use all of our
// * existing DFA scanning functionality.
// */
//@deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//class DFDLStringReader private (charPos: Int, data: String, rdr: Reader[Char])
//  extends DFDLCharReader {
//  def copy = {
//    var c = new DFDLStringReader(charPos, data, (new CharSequenceReader(data)).drop(charPos))
//    c
//  }
//  override def source = rdr.source
//  override def offset = rdr.offset
//  def this(data: String) = this(0, data, new CharSequenceReader(data))
//  def first = if (atEnd) -1.toChar else rdr.first
//  def rest = new DFDLStringReader(charPos + 1, data, rdr.rest)
//  def atEnd = rdr.atEnd
//  def atCharPos(cp0b: Int) = new DFDLStringReader(cp0b, data, (new CharSequenceReader(data)).drop(cp0b))
//  def atBitPos(bp0b: Long) = Assert.usageError("not to be used")
//  def characterPos = Assert.usageError("not to be used")
//  def bitPos0b = Assert.usageError("not to be used")
//}
//
///**
// * This is for unit tests that want to feed data from a string
// */
//@deprecated("2015-06-22", "Should use methods of DataInputStream directly to iterate characters. Not use this which copies")
//class DFDLUTStringReader private (rdr: Reader[Char])
//  extends DFDLCharReader {
//  def copy = Assert.usageError("not to be used in test reader")
//  override def source = rdr.source
//  override def offset = rdr.offset
//  def this(data: String) = this(new CharSequenceReader(data))
//  def first = rdr.first
//  def rest = new DFDLUTStringReader(rdr.rest)
//  def atEnd = rdr.atEnd
//  def atCharPos(cp0b: Int) = Assert.usageError("not to be used in test reader")
//  def atBitPos(bp0b: Long) = Assert.usageError("not to be used in test reader")
//  def characterPos = Assert.usageError("not to be used in test reader")
//  def bitPos0b = Assert.usageError("not to be used in test reader")
//}

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
