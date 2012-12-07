package daffodil.processors

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
 *
 * All reading of data ultimately comes to this layer which retrieves data on demand.
 *
 * This layer doesn't know anything about bits and bit positions like all the higher layers do.
 */
class DFDLByteReader private (psb: PagedSeq[Byte], val bytePos: Int = 0)
  extends scala.util.parsing.input.Reader[Byte] {

  def this(in: ReadableByteChannel) = this(PagedSeq.fromIterator(new IterableReadableByteChannel(in)), 0)

  lazy val first: Byte = psb(bytePos)

  lazy val rest: DFDLByteReader = new DFDLByteReader(psb, bytePos + 1)

  lazy val pos: scala.util.parsing.input.Position = new DFDLBytePosition(bytePos)

  lazy val atEnd: Boolean = !psb.isDefinedAt(bytePos)

  def atPos(bytePosition: Int): DFDLByteReader = {
    //System.err.println("DFDLByteReader psb length: " + psb.length)
    //new DFDLByteReader(psb.slice(bytePosition), 0) 
    new DFDLByteReader(psb, bytePosition)
  }

  def getByte(bytePosition: Int): Byte = { psb(bytePosition) }

  lazy val byteArray: Array[Byte] = psb.toArray[Byte]
  lazy val bb: ByteBuffer = ByteBuffer.wrap(byteArray)

  /**
   * Factory for a Reader[Char] that constructs characters by decoding them from this
   * Reader[Byte] for a specific encoding starting at a particular bit position.
   *
   * Yes, I said "bit" position.
   *
   * These are kept in the processor state for reuse.
   */
  def newCharReader(charset: Charset, bitPos: Long, bitLimit: Long): DFDLCharReader = {
    DFDLCharReader(psb, bitPos, bitLimit, charset)
  }

}

object DFDLCharReader {

  def apply(thePsb: PagedSeq[Byte], bitPosition: Long, bitLimit: Long, charset: Charset) = {

    Assert.usage(bitPosition <= Int.MaxValue, "bit positions are limited to 32-bit signed integer by underlying libraries.")
    val bitPos = bitPosition.toInt

    val bitOffset = bitPos & 0x7
    val bytePos = bitPos >> 3
    val is = new IteratorInputStream(thePsb.slice(bytePos).iterator)
    val r = DFDLJavaIOInputStreamReader(is, charset, bitOffset, bitLimit)
    // TRW - The following line was changed because the fromSource
    // method was causing the readLine method of the BufferedReader class to be
    // called.  This resulted in the loss of \n, \r and \r\n characters from the data.
    //val psc = PagedSeq.fromSource(scala.io.Source.fromInputStream(is)(codec))
    val psc = PagedSeq.fromReader(r)
    val charOffset = 0
    val rdr = new DFDLCharReader(charset, bitOffset, bitLimit, psc, charOffset, thePsb)
    rdr
  }

}

/**
 * Reader[Char] constructed from a specific point within a PagedSeq[Byte], for
 * a particular character set encoding. Ends if there is any error trying to decode a
 * character.
 */
class DFDLCharReader private (charset: Charset, startingBitPos: Int, bitLimit: Long,
                              psc: PagedSeq[Char], override val offset: Int, psb: PagedSeq[Byte])
  extends scala.util.parsing.input.Reader[Char] {

  override lazy val source: CharSequence = psc

  def first: Char = psc(offset)

  def rest: scala.util.parsing.input.Reader[Char] =
    if (psc.isDefinedAt(offset)) new DFDLCharReader(charset, startingBitPos, bitLimit, psc, offset + 1, psb)
    else this //new DFDLCharReader(psc, offset + 1)

  def atEnd: Boolean = !psc.isDefinedAt(offset)

  def pos: scala.util.parsing.input.Position = new OffsetPosition(source, offset) //new DFDLCharPosition(offset)

  override def drop(n: Int): DFDLCharReader = new DFDLCharReader(charset, startingBitPos, bitLimit, psc, offset + n, psb)

  def atPos(characterPos: Int): DFDLCharReader = {
    new DFDLCharReader(charset, startingBitPos, bitLimit, psc, characterPos, psb)
  }

  // We really want to be able to ask for a CharReader starting at said bitPos
  def atBitPos(bitPos: Long): DFDLCharReader = {
    //System.err.println("DFDLCharReader.atBytePos(" + bytePosition + ")")
    DFDLCharReader(psb, bitPos, bitLimit, charset)
  }

  def getCharsetName: String = charset.name()

  def characterPos: Int = offset

  // def isDefinedAt(charPos : Int) : Boolean = psc.isDefinedAt(charPos)

  def print: String = {
    "DFDLCharReader - " + source.length() + ": " + source + "\nDFDLCharReader - " + characterPos + ": " + source.subSequence(characterPos, source.length())
  }
  
  override def toString = {
    "DFDLCharReader at bitPos " + startingBitPos
  }

}

// Scala Reader stuff is not consistent about whether it is generic over the element type, 
// or specific to Char. We want to have a Reader like abstraction that is over bytes, but 
// be able to create real Reader[Char] from it at any byte position.

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

//object DFDLByteReader {
//  type PosMap = HashMap[Int, (DFDLCharReader, Int)]
//  type CSMap = HashMap[String, PosMap]
//  type PSMap = HashMap[PagedSeq[Byte], CSMap]
//  private var charReaderMap: PSMap = HashMap.empty
//
//  // CharPosMap [bytePos + csName, CharPos]
//  type CharPosMap = HashMap[String, Int]
//  private var charPositionsMap: CharPosMap = HashMap.empty
//
//  /**
//   * Constructs a new DFDLCharReader using the same PagedSeq[Byte] but
//   * starts reading characters from the bitPos.
//   */
//  private def getNewReader(psb: PagedSeq[Byte], bitPos: Long, charset: Charset): DFDLCharReader = {
//    Assert.usage(bitPos <= Int.MaxValue) // Limitation due to underlying stuff being so Int oriented.
//    val newrdr = new DFDLCharReader(psb, bitPos.toInt, charset)
//    newrdr
//  }
//
//  /**
//   * Factory for a Reader[Char] that constructs characters by decoding them from this
//   * Reader[Byte] for a specific encoding starting at a particular byte position.
//   *
//   * Memoizes so that we don't re-decode as we backtrack around.
//   *
//   * TRW - 11/21/2012 - This is no longer used.  Reader is kept around now in state.
//   * Code here is left in-case we wish to revert back to using this call.
//   */
//  //  private def getCharReader(psb: PagedSeq[Byte], bytePos: Int, decoder: CharsetDecoder): DFDLCharReader = {
//  //
//  //    val csName = decoder.charset().name()
//  //
//  //    if (charReaderMap.isEmpty) {
//  //      var csMap: CSMap = HashMap.empty
//  //      val emptyCharReaderMap: PosMap = HashMap.empty
//  //      csMap.put(csName, emptyCharReaderMap)
//  //      charReaderMap.put(psb, csMap)
//  //    }
//  //
//  //    // TRW - Added for Compound Pattern Match to work
//  //    if (charReaderMap.get(psb) == None) {
//  //      var csMap: CSMap = HashMap.empty
//  //      val emptyCharReaderMap: PosMap = HashMap.empty
//  //      csMap.put(csName, emptyCharReaderMap)
//  //      charReaderMap.put(psb, csMap)
//  //    }
//  //
//  //    // We need to know what bytePositions currently exist in the HashMap
//  //    // so we can determine if there exists an entry containing this bytePos.
//  //    // We need to then ask the reader if it's currently at the end.  If it is then
//  //    // we need to create a new reader.
//  //
//  //    val charReaders = charReaderMap.get(psb).get.get(csName).get
//  //
//  //    val ks = charReaders.keySet
//  //    val diffs = ks.map(k => k -> (bytePos - k)).filter(x => x._2 >= 0)
//  //    val sortedDiffs = diffs.toList.sortWith((e1, e2) => e1._2 < e2._2)
//  //
//  //    if (sortedDiffs.length > 0) {
//  //      val closestEntryKey = sortedDiffs(0)._1
//  //      val result = charReaders.get(closestEntryKey) match {
//  //        case Some((rdr, charPos)) => {
//  //          val closestEntry = rdr.asInstanceOf[DFDLCharReader]
//  //          if (closestEntry.atEnd) {
//  //            val newrdr = new DFDLCharReader(psb, bytePos, decoder)
//  //            charReaders.put(bytePos, newrdr -> 0)
//  //            charPositionsMap.put(bytePos + csName, 0)
//  //            newrdr
//  //          } else {
//  //            rdr.atPos(charPos)
//  //          }
//  //        }
//  //        case None => {
//  //          val newrdr = new DFDLCharReader(psb, bytePos, decoder)
//  //          charReaders.put(bytePos, newrdr -> 0)
//  //          charPositionsMap.put(bytePos + csName, 0)
//  //          newrdr
//  //        }
//  //      }
//  //      return result
//  //    } else {
//  //      // A valid entry doesn't exist
//  //      val newrdr = new DFDLCharReader(psb, bytePos, decoder)
//  //      charReaders.put(bytePos, newrdr -> 0)
//  //      charPositionsMap.put(bytePos + csName, 0)
//  //      return newrdr
//  //    }
//  //
//  //    //    charReaders.get(bytePos) match {
//  //    //      case None => {
//  //    //        val newrdr = new DFDLCharReader(psb, bytePos, decoder)
//  //    //        charReaders.put(bytePos, newrdr)
//  //    //        newrdr
//  //    //      }
//  //    //      case Some(rdr) => rdr
//  //    //    }
//  //  }
//  //
//  //  private def setCharReader(reader: DFDLCharReader, psb: PagedSeq[Byte]) = {
//  //    //    val charReaders = charReaderMap.get(psb).get.get(reader.getCharsetName).get
//  //    //    //charReaders.put(bytePos, reader)
//  //    //    //System.err.println("Before insert: " + charReaders)
//  //    //
//  //    ////    charReaders.get(reader.startingBytePos) match {
//  //    ////      case Some((rdr, _)) => charReaders.put(reader.startingBytePos, (rdr -> reader.characterPos))
//  //    ////      case None => charReaders.put(reader.startingBytePos, (reader -> reader.characterPos))
//  //    ////    }
//  //    //    
//  //    //    // Don't need to do a get here, we only need to check if it exists
//  //    //    if (!charReaders.contains(reader.startingBytePos)) {
//  //    //      // This shouldn't ever really happen, right?  The reader is initially created
//  //    //      // and stored in the HashMap.  So a reader should always be there.
//  //    //        charReaders.put(reader.startingBytePos, (reader -> reader.characterPos))
//  //    //    }
//  //    // This assumes that the reader already exists in the HashMap (as it should)
//  //    // thus avoiding all of the delay for having to do execute a get against the charReaderMap
//  //    charPositionsMap.put(reader.startingBytePos + reader.getCharsetName, reader.characterPos)
//  //  }
//
//}

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
class IteratorInputStream(ib: Iterator[Byte])
  extends InputStream {

  def read(): Int =
    if (!ib.hasNext) -1
    else {
      val res = ib.next()
      res
    }
}
