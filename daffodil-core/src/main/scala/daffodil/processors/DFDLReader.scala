package delimsearch

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
import daffodil.exceptions.Assert

/**
 * Pure functional Reader[Byte] that gets its data from a DFDL.Input (aka a ReadableByteChannel)
 */
class DFDLByteReader private (psb : PagedSeq[Byte], val bytePos : Int = 0)
  extends scala.util.parsing.input.Reader[Byte] {

  def this(in : ReadableByteChannel) = this(PagedSeq.fromIterator(new IterableReadableByteChannel(in)), 0)

  lazy val first : Byte = psb(bytePos)

  lazy val rest : DFDLByteReader = new DFDLByteReader(psb, bytePos + 1)

  lazy val pos : scala.util.parsing.input.Position = new DFDLBytePosition(bytePos)

  lazy val atEnd : Boolean = !psb.isDefinedAt(bytePos)

  def atPos(bytePosition : Int) : DFDLByteReader = { new DFDLByteReader(psb, bytePosition) }

  def getByte(bytePosition : Int) : Byte = { psb(bytePosition) }

  lazy val byteArray : Array[Byte] = psb.toArray[Byte]
  lazy val bb : ByteBuffer = ByteBuffer.wrap(byteArray)

  /**
   * Factory for a Reader[Char] that constructs characters by decoding them from this
   * Reader[Byte] for a specific encoding starting at a particular byte position.
   */
  def charReader(csName : String) : scala.util.parsing.input.Reader[Char] = {
    DFDLByteReader.getCharReader(psb, bytePos, csName) // new DFDLCharReader(psb, bytePos, csName)
  }

  // Retrieves a new charReader every time
  def newCharReader(csName : String) : scala.util.parsing.input.Reader[Char] = {
    DFDLByteReader.getNewReader(psb, bytePos, csName)
  }

  def updateCharReader(reader : DFDLCharReader) = {
    DFDLByteReader.setCharReader(reader, psb)
  }

}

// Review: move all quasi-java to separate file. 
//
// Block comment at top should provide motivation for the whole reimplementation, which is NOT
// because we don't want a java file in our code base. It's becasue we needed to change a very
// inner behavior of what happens when the decoder gets a decode error. (Which took me a while to find,
// but search for "DFDL Implementors:" to find it. The whole motivation is to be able to change that
// one inner-loop line of code. We should make that clear.
//
// Also Mixed scala/functional and scala-in-java-style code in one file leaves
// programmers confused as to what style and how to maintain code.
// Put block comments to make legacy of code as scala version of former java code clear.
//
// Preserve relevant javadoc from the java classes.
// 
object DFDLStreamDecoder {
  private val MIN_BYTE_BUFFER_SIZE : Int = 32
  private val DEFAULT_BYTE_BUFFER_SIZE : Int = 8192

  def forInputStreamReader(in : InputStream, lock : Object, charsetName : String) : DFDLStreamDecoder = {

    // REVIEW: I think we should throw if csn is nil. Tolerating this would just lead to bugs.
    Assert.usage(charsetName != Nil)
    // There is no notion of a default charset in DFDL.
    // So this can be val.
    val csn : String = charsetName

    // REVIEW: this next line can then go
    // if (csn == Nil || csn.length() == 0) { csn = Charset.defaultCharset().name() }
    try {
      if (Charset.isSupported(csn)) { return new DFDLStreamDecoder(in, Charset.forName(csn)) }
    } catch {
      case e : IllegalCharsetNameException => // Nothing
    }
    throw new UnsupportedEncodingException(csn)
  }

  def forInputStreamReader(in : InputStream, lock : Object, cs : Charset) : DFDLStreamDecoder = {
    return new DFDLStreamDecoder(in, cs)
  }

  def forInputStreamReader(in : InputStream, lock : Object, dec : CharsetDecoder) : DFDLStreamDecoder = {
    return new DFDLStreamDecoder(in, dec)
  }

  private var channelsAvailable = true

  private def getChannel(in : FileInputStream) : FileChannel = {
    if (!channelsAvailable) return null
    try {
      return in.getChannel
    } catch {
      case e : UnsatisfiedLinkError => {
        channelsAvailable = false
        return null
      }
    }
  }
}
// Review: Naming convention: 
// I had started doing DFDL prefixes on scala functional classes to tag them as our variants of those
// objects. 
// For these reimplementations of the java objects I'd like to use names that make it clear 
// these are not functional I/O but are java-style mutating IO.
// 
// So how about MutatingStreamDecoder or
// If the part after the DFDL prefix is exactly the name of a java class in e.g., package java.io, then maybe
// DFDLJavaIOStreamDecoder
//
class DFDLStreamDecoder(var cs : Charset, var decoder : CharsetDecoder, var bb : ByteBuffer,
                        var in : InputStream, var ch : ReadableByteChannel)
  extends java.io.Reader {

  // 
  // Review: locking related stuff should just be commented out
  // Seriously, are you going to test that this is thread safe, or do a formal proof?
  // No, so don't even try to make the code thread safe.
  //
  // However, as you are transliterating from java that purports to have figure that
  // stuff out (I am always dubious of such claims), you should put the constructs in,
  // but comment them out with comments like "// java code had synchronized method" or
  // "// java code took a lock here"
  //
  @volatile
  private var isOpen : Boolean = true
  def ensureOpen = { if (!isOpen) throw new java.io.IOException("Stream closed") }

  private var haveLeftoverChar : Boolean = false
  private var leftoverChar : Char = 0

  def getEncoding : String = {
    if (isOpen) { return encodingName }
    return null
  }

  override def read : Int = {
    return read0
  }

  // Insert a comment here about leftover char stuff. E.g.,
  //
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
  private def read0 : Int = {
    lock.synchronized {

      // Return the leftover char, if there is one
      if (haveLeftoverChar) {
        haveLeftoverChar = false
        return leftoverChar
      }

      // Convert more bytes
      val cb : Array[Char] = new Array[Char](2)
      val n : Int = read(cb, 0, 2)
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

  // REVIEW: until a programming language lets us associated units
  // with integers so that a byte index and a char index can't be added,
  // I think code which has both byte and char contexts needs to use qualified names.
  // E.g., cLength or bLength instead of length to make the units clear, and cOffset or bOffset
  // and cIndex or bIndex, etc.
  //
  // Having written that, I now see that the code these are all character offsets
  // and lengths of character buffers, so a comment will be sufficient e.g.,
  //
  // All lengths and offsets below are in character units.
  //
  def read(cbuf : Array[Char], offset : Int, length : Int) : Int = {
    var off : Int = offset
    var len : Int = length

    // REVIEW: I would comment out all locking stuff using the style I've used for this one below
    // which leaves the block structure intact as is.
    // Approximately that is. Maybe in this case the whole object was just synchronized. 
    // So modify comment as appropriate.

    // lock.synchronized // in java this was a synchronized method 
    {
      ensureOpen
      if ((off < 0) || (off > cbuf.length) || (len < 0)
        || ((off + len) > cbuf.length) || ((off + len) < 0)) {
        throw new IndexOutOfBoundsException()
      }

      if (len == 0) { return 0 }
      var n : Int = 0

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

      // REVIEW: Insert comment
      // If length is 1, then we recursively end up back here with length 2, 
      // and that's why it's not a stack overflow.

      if (len == 1) {
        // Treat single-character array reads just like read()
        val c : Int = read0
        if (c == -1) {
          return if (n == 0) -1 else n
        }
        cbuf(off) = c.asInstanceOf[Char]
        return n + 1
      }
      return n + implRead(cbuf, off, off + len)
    }

  }

  override def ready : Boolean = {
    lock.synchronized {
      ensureOpen
      return haveLeftoverChar || implReady
    }
  }

  def close : Unit = {
    lock.synchronized {
      if (!isOpen) { return }
      implClose
      isOpen = false
    }
  }

  // REVIEW: move constructors to top of class. Carryover javadoc to explain them if there is some.
  def this(in : InputStream, dec : CharsetDecoder) = {
    // TODO: What's up with super(lock) ?
    //super(lock)

    this(dec.charset(), dec, {
      var myBB : ByteBuffer = null
      myBB = ByteBuffer.allocateDirect(DFDLStreamDecoder.DEFAULT_BYTE_BUFFER_SIZE)
      myBB.flip()
      myBB
    }, in, {
      if (in.isInstanceOf[FileInputStream]) in.asInstanceOf[FileInputStream].getChannel()
      else Channels.newChannel(in)
    }) //in.asInstanceOf[FileInputStream].getChannel())
  }

  // REVIEW: do we want to support this constructor? Might want to just Assert.NYI for this.
  def this(in : InputStream, cs : Charset) = {
    this(in, cs.newDecoder().onMalformedInput(CodingErrorAction.REPLACE).onUnmappableCharacter(CodingErrorAction.REPLACE))
  }

  private def readBytes : Int = {
    bb.compact()
    try {
      if (ch != null) {
        // Read from the channel
        val n : Int = ch.read(bb)
        if (n < 0) return n
      } else {
        // Read from the input stream, and hten update the buffer
        val lim : Int = bb.limit()
        val pos : Int = bb.position()
        assert(pos <= lim)
        val rem : Int = if (pos <= lim) lim - pos else 0
        assert(rem > 0)
        val n : Int = in.read(bb.array, bb.arrayOffset() + pos, rem)
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
    val rem : Int = bb.remaining()
    // assert(rem != 0) : rem
    assert(rem != 0)
    return rem
  }

  // REVIEW: private?
  def implRead(cbuf : Array[Char], off : Int, end : Int) : Int = {
    // In order to handle surrogate pairs, this method requires that
    // the invoker attempt to read at least two characters.  Saving the
    // extra character, if any, at a higher level is easier
    // to deal with it here.
    assert(end - off > 1)
    var cb : CharBuffer = CharBuffer.wrap(cbuf, off, end - off)
    if (cb.position != 0) {
      // Ensure that cb[0] == cbuf[off]
      cb = cb.slice
    }

    var eof : Boolean = false
    var continue : Boolean = false

    breakable {
      while (true) {
        val cr : CoderResult = decoder.decode(bb, cb, eof)
        if (cr.isUnderflow) {
          if (eof) { break }
          if (!cb.hasRemaining()) { break }
          if ((cb.position() > 0) && !inReady) { break }
          val n : Int = readBytes
          if (n < 0) {
            eof = true
            if ((cb.position() == 0) && (!bb.hasRemaining())) { break }
            decoder.reset()
          }
          continue = true
        }

        if (!continue) {
          if (cr.isOverflow) {
            assert(cb.position() > 0)
            break
          }
          // REVIEW: Put louder commentary here.
          // Took me a while to even find this spot.
          // E.g.,I was searching for the catch that would 
          // catch this throw but didn't find it. 

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

  def encodingName : String = {
    return if (cs.isInstanceOf[HistoricallyNamedCharset]) cs.asInstanceOf[HistoricallyNamedCharset].historicalName else cs.name()
  }

  private def inReady : Boolean = {
    try {
      return (((in != null) && (in.available() > 0)) || (ch.isInstanceOf[FileChannel]))
    } catch {
      case e : IOException => return false
    }
  }

  def implReady : Boolean = { return bb.hasRemaining() || inReady }

  def implClose : Unit = {
    if (ch != null) ch.close()
    else in.close()
  }

}

// REVIEW: move quasi-java to separate file.
// 
// Naming convention? Perhaps these should make their java-like behavior clearer
// e.g., MutatingInputStreamReader
// which would make it clear that the object is changing as you read from it.
//
class DFDLInputStreamReader(val in : InputStream, val sd : DFDLStreamDecoder) extends java.io.Reader(in : InputStream) {

  def this(in : InputStream, charsetName : String) = {
    this(in, DFDLStreamDecoder.forInputStreamReader(in, new Object, {
      if (charsetName == null) { throw new NullPointerException("charsetName") }
      charsetName
    }))
  }

  def this(in : InputStream, cs : Charset) = {
    this(in, DFDLStreamDecoder.forInputStreamReader(in, new Object, {
      if (cs == null) { throw new NullPointerException("charset") }
      cs
    }))
  }

  def this(in : InputStream, dec : CharsetDecoder) = {
    this(in, DFDLStreamDecoder.forInputStreamReader(in, new Object, {
      if (dec == null) { throw new NullPointerException("charset decoder") }
      dec
    }))
  }

  def getEncoding : String = sd.getEncoding
  override def read : Int = sd.read()
  def read(cbuf : Array[Char], offset : Int, length : Int) : Int = sd.read(cbuf, offset, length)
  override def ready : Boolean = sd.ready()
  def close : Unit = sd.close()
}

/**
 * Reader[Char] constructed from a specific point within a PagedSeq[Byte], for
 * a particular character set encoding. Ends if there is any error trying to decode a
 * character.
 */
class DFDLCharReader private (charsetName : String, theStartingBytePos : Int, psc : PagedSeq[Char], override val offset : Int)
  extends scala.util.parsing.input.Reader[Char] {

  override lazy val source : CharSequence = psc

  def startingBytePos : Int = theStartingBytePos

  def this(psb : PagedSeq[Byte], bytePos : Int, csName : String) = {
    this(csName, bytePos, {
      // TRW - The following change was made because we want the
      // IteratorInputStream to start at bytePos.
      //
      //val is = new IteratorInputStream(psb.iterator)
      val is = new IteratorInputStream(psb.slice(bytePos).iterator)
      val cs = java.nio.charset.Charset.forName(csName)
      val codec = scala.io.Codec.charset2codec(cs)
      // TODO: Determine if the right thing here to do is to ignore malformed input which is default behavior
      codec.onMalformedInput(CodingErrorAction.REPORT)
      //codec.onMalformedInput(CodingErrorAction.IGNORE)
      // TRW - The following line was changed because the fromSource
      // method was causing the readLine method of the BufferedReader class to be
      // called.  This resulted in the loss of \n, \r and \r\n characters from the data.
      //val psc = PagedSeq.fromSource(scala.io.Source.fromInputStream(is)(codec))
      //val r = new InputStreamReader(is, codec.decoder)
      val r = new DFDLInputStreamReader(is, codec.decoder)
      val psc = PagedSeq.fromReader(r)
      psc
    }, 0)
  }

  def first : Char = psc(offset)

  def rest : scala.util.parsing.input.Reader[Char] =
    if (psc.isDefinedAt(offset)) new DFDLCharReader(charsetName, startingBytePos, psc, offset + 1)
    else this //new DFDLCharReader(psc, offset + 1)

  def atEnd : Boolean = !psc.isDefinedAt(offset)

  def pos : scala.util.parsing.input.Position = new OffsetPosition(source, offset) //new DFDLCharPosition(offset)

  override def drop(n : Int) : DFDLCharReader = new DFDLCharReader(charsetName, startingBytePos, psc, offset + n)

  def atPos(characterPos : Int) : DFDLCharReader = {
    new DFDLCharReader(charsetName, startingBytePos, psc, characterPos)
  }

  def getCharsetName : String = charsetName

  def characterPos : Int = offset

  // def isDefinedAt(charPos : Int) : Boolean = psc.isDefinedAt(charPos)

  def print : String = {
    "DFDLCharReader - " + source.length() + ": " + source + "\nDFDLCharReader - " + characterPos + ": " + source.subSequence(characterPos, source.length())
  }

}

// Scala Reader stuff is not consistent about whether it is generic over the element type, 
// or specific to Char. We want to have a Reader like abstraction that is over bytes, but 
// be able to create real Reader[Char] from it at any byte position.

/**
 * All this excess buffering layer for lack of a way to convert a ReadableByteChannel directly into
 * a PagedSeq. We need an Iterator[Byte] first to construct a PagedSeq[Byte].
 */
class IterableReadableByteChannel(rbc : ReadableByteChannel)
  extends scala.collection.Iterator[Byte] {

  private final val bufferSize = 10000
  private var currentBuf : java.nio.ByteBuffer = _
  private var sz : Int = _

  private def advanceToNextBuf() {
    currentBuf = java.nio.ByteBuffer.allocate(bufferSize)
    sz = rbc.read(currentBuf)
    currentBuf.flip()
  }

  advanceToNextBuf()

  def hasNext() : Boolean = {
    if (sz == -1) return false
    if (currentBuf.hasRemaining()) return true
    advanceToNextBuf()
    if (sz == -1) return false
    if (currentBuf.hasRemaining()) return true
    return false
  }

  var pos : Int = 0

  def next() : Byte = {
    if (!hasNext()) throw new IndexOutOfBoundsException(pos.toString)
    pos += 1
    currentBuf.get()
  }
}

/**
 * Scala's Position is document oriented in that it is 1-based indexing and assumes
 * line numbers and column numbers.
 *
 */
class DFDLBytePosition(i : Int) extends scala.util.parsing.input.Position {
  def line = 1
  def column = i + 1
  // IDEA: could we assume a 'line' of bytes is 32 bytes because those print out nicely as 
  // as in HHHHHHHH HHHHHHHH ... etc. on a 72 character line?
  // Could come in handy perhaps. 
  val lineContents = "" // unused. Maybe this should throw. NoSuchOperation, or something.
}

object DFDLByteReader {
  type PosMap = HashMap[Int, (DFDLCharReader, Int)]
  type CSMap = HashMap[String, PosMap]
  type PSMap = HashMap[PagedSeq[Byte], CSMap]
  private var charReaderMap : PSMap = HashMap.empty

  // CharPosMap [bytePos + csName, CharPos]
  type CharPosMap = HashMap[String, Int]
  private var charPositionsMap : CharPosMap = HashMap.empty

  private def getNewReader(psb : PagedSeq[Byte], bytePos : Int, csName : String) : DFDLCharReader = {
    if (charReaderMap.isEmpty) {
      var csMap : CSMap = HashMap.empty
      val emptyCharReaderMap : PosMap = HashMap.empty
      csMap.put(csName, emptyCharReaderMap)
      charReaderMap.put(psb, csMap)
    }

    // TRW - Added for Compound Pattern Match to work
    if (charReaderMap.get(psb) == None) {
      var csMap : CSMap = HashMap.empty
      val emptyCharReaderMap : PosMap = HashMap.empty
      csMap.put(csName, emptyCharReaderMap)
      charReaderMap.put(psb, csMap)
    }
    val charReaders = charReaderMap.get(psb).get.get(csName).get
    val newrdr = new DFDLCharReader(psb, bytePos, csName)
    charReaders.put(bytePos, newrdr -> 0)
    charPositionsMap.put(bytePos + csName, 0)
    newrdr
  }

  /**
   * Factory for a Reader[Char] that constructs characters by decoding them from this
   * Reader[Byte] for a specific encoding starting at a particular byte position.
   *
   * Memoizes so that we don't re-decode as we backtrack around.
   */
  private def getCharReader(psb : PagedSeq[Byte], bytePos : Int, csName : String) : DFDLCharReader = {
    if (charReaderMap.isEmpty) {
      var csMap : CSMap = HashMap.empty
      val emptyCharReaderMap : PosMap = HashMap.empty
      csMap.put(csName, emptyCharReaderMap)
      charReaderMap.put(psb, csMap)
    }

    // TRW - Added for Compound Pattern Match to work
    if (charReaderMap.get(psb) == None) {
      var csMap : CSMap = HashMap.empty
      val emptyCharReaderMap : PosMap = HashMap.empty
      csMap.put(csName, emptyCharReaderMap)
      charReaderMap.put(psb, csMap)
    }

    // We need to know what bytePositions currently exist in the HashMap
    // so we can determine if there exists an entry containing this bytePos.
    // We need to then ask the reader if it's currently at the end.  If it is then
    // we need to create a new reader.

    val charReaders = charReaderMap.get(psb).get.get(csName).get

    val ks = charReaders.keySet
    val diffs = ks.map(k => k -> (bytePos - k)).filter(x => x._2 >= 0)
    val sortedDiffs = diffs.toList.sortWith((e1, e2) => e1._2 < e2._2)

    if (sortedDiffs.length > 0) {
      val closestEntryKey = sortedDiffs(0)._1
      val result = charReaders.get(closestEntryKey) match {
        case Some((rdr, charPos)) => {
          val closestEntry = rdr.asInstanceOf[DFDLCharReader]
          if (closestEntry.atEnd) {
            val newrdr = new DFDLCharReader(psb, bytePos, csName)
            charReaders.put(bytePos, newrdr -> 0)
            charPositionsMap.put(bytePos + csName, 0)
            newrdr
          } else {
            rdr.atPos(charPos)
          }
        }
        case None => {
          val newrdr = new DFDLCharReader(psb, bytePos, csName)
          charReaders.put(bytePos, newrdr -> 0)
          charPositionsMap.put(bytePos + csName, 0)
          newrdr
        }
      }
      return result
    } else {
      // A valid entry doesn't exist
      val newrdr = new DFDLCharReader(psb, bytePos, csName)
      charReaders.put(bytePos, newrdr -> 0)
      charPositionsMap.put(bytePos + csName, 0)
      return newrdr
    }

    //    charReaders.get(bytePos) match {
    //      case None => {
    //        val newrdr = new DFDLCharReader(psb, bytePos, csName)
    //        charReaders.put(bytePos, newrdr)
    //        newrdr
    //      }
    //      case Some(rdr) => rdr
    //    }
  }

  private def setCharReader(reader : DFDLCharReader, psb : PagedSeq[Byte]) = {
    //    val charReaders = charReaderMap.get(psb).get.get(reader.getCharsetName).get
    //    //charReaders.put(bytePos, reader)
    //    //System.err.println("Before insert: " + charReaders)
    //
    ////    charReaders.get(reader.startingBytePos) match {
    ////      case Some((rdr, _)) => charReaders.put(reader.startingBytePos, (rdr -> reader.characterPos))
    ////      case None => charReaders.put(reader.startingBytePos, (reader -> reader.characterPos))
    ////    }
    //    
    //    // Don't need to do a get here, we only need to check if it exists
    //    if (!charReaders.contains(reader.startingBytePos)) {
    //      // This shouldn't ever really happen, right?  The reader is initially created
    //      // and stored in the HashMap.  So a reader should always be there.
    //        charReaders.put(reader.startingBytePos, (reader -> reader.characterPos))
    //    }
    // This assumes that the reader already exists in the HashMap (as it should)
    // thus avoiding all of the delay for having to do execute a get against the charReaderMap
    charPositionsMap.put(reader.startingBytePos + reader.getCharsetName, reader.characterPos)
  }

}

/**
 * Position in a character stream.
 *
 * We ignore line/column structure. It's all one "line" as far as we are concerned.
 */
class DFDLCharPosition(i : Int) extends scala.util.parsing.input.Position {
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
class IteratorInputStream(ib : Iterator[Byte])
  extends InputStream {

  def read() : Int =
    if (!ib.hasNext) -1
    else {
      val res = ib.next()
      res
    }
}
