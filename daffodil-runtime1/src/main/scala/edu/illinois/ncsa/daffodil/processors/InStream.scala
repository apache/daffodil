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

import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Misc._
import edu.illinois.ncsa.daffodil.util.Bits
import java.io.ByteArrayInputStream
import java.nio.charset.Charset
import java.nio.CharBuffer
import java.io.InputStreamReader
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import java.io.InputStream
import java.io.File
import org.apache.commons.io.IOUtils
import java.nio.file.Files
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.OutStream

object InStream {

  val mandatoryAlignment = 8

  def fromByteChannel(context: ElementRuntimeData,
    in: DFDL.Input,
    bitStartPos0b: Long,
    numBitsLimit: Long, // a count, not a position
    bitOrder: BitOrder) = {
    val bitLimit0b = if (numBitsLimit == -1) -1 else bitStartPos0b + numBitsLimit
    new InStreamFromByteChannel(context, in, bitStartPos0b, bitLimit0b, bitOrder)
  }

  /**
   * textOnly
   * FixedWidth
   * encodingErrorPolicy='replace'
   */
  def forTextOnlyFixedWidthErrorReplace(ei: EncodingInfo, context: ElementRuntimeData, file: File,
    charsetEncodingName: String, lengthLimitInBits: Long): InStream = {
    val charset = CharsetUtils.getCharset(charsetEncodingName)
    val (cBuf, bBuf) = TextOnlyReplaceOnErrorReaderFactory.getCharBuffer(file, charset)
    val fSize = file.length
    val fSizeInBits = fSize * 8
    val bitLimit =
      if (lengthLimitInBits == -1) fSizeInBits
      else lengthLimitInBits
    val bitsPerChar = ei.knownEncodingWidthInBits
    val info = FixedWidthTextInfoCBuf(context, bitsPerChar, mandatoryAlignment, charset,
      bitLimit, cBuf, bBuf)
    val inStream = InStreamFixedWidthTextOnly(0, info)
    inStream
  }

  def forTextOnlyFixedWidthErrorReplace(ei: EncodingInfo, context: ElementRuntimeData, jis: InputStream,
    charsetEncodingName: String,
    lengthLimitInBits: Long): InStream = {
    val charset = CharsetUtils.getCharset(charsetEncodingName)
    val bytes = IOUtils.toByteArray(jis)
    val bBuf = ByteBuffer.wrap(bytes)
    val cBuf = TextOnlyReplaceOnErrorReaderFactory.getCharBuffer(bBuf, charset)
    val bitLimit =
      if (lengthLimitInBits == -1) bytes.length * 8
      else lengthLimitInBits
    val bitsPerChar = ei.knownEncodingWidthInBits
    val info = FixedWidthTextInfoCBuf(context, bitsPerChar, mandatoryAlignment, charset,
      bitLimit, cBuf, bBuf)
    val inStream = InStreamFixedWidthTextOnly(0, info)
    inStream
  }
}

/**
 * Encapsulates the I/O as an abstraction that works something like a java.nio.ByteBuffer
 * but a bit more specialized for DFDL needs, e.g., supports lengths and positions in bits.
 */
trait InStream {

  def assignFrom(other: InStream): Unit

  def duplicate(): InStream
  def bitPos0b: Long
  def bitLimit0b: Long
  def charPos0b: Long
  def charLimit0b: Long
  def reader: Maybe[DFDLCharReader]

  def withPos(newBitPos0b: Long, newCharPos0b: Long, newReader: Maybe[DFDLCharReader]): InStream
  def withPos(newBitPos0b: Long, newCharPos0b: Long): InStream
  def withEndBitLimit(newBitLimit0b: Long): InStream

  /**
   * Checks that all 8-bits are available, and requires alignment also.
   */
  def getByte(bitPos0b: Long, order: java.nio.ByteOrder, bitOrder: BitOrder): Byte

  /**
   * Will deliver a byte even if the bit limit implies only a fragment of a
   * byte is actually available.
   */
  def getRawByte(bitPos0b: Long, order: java.nio.ByteOrder, bitOrder: BitOrder): Byte

  /**
   * Returns up to numBytes. Could be fewer. Does not check bitLimit bounds
   * precisely.
   */
  def getBytes(bitPos0b: Long, numBytes: Long): Array[Byte]

  def getLong(bitPos0b: Long, bitCount: Long, byteOrd: java.nio.ByteOrder, bitOrd: BitOrder): Long

  def getBigInt(bitPos0b: Long, bitCount: Long, order: java.nio.ByteOrder, bitOrder: BitOrder): BigInt

  def getCharReader(charset: Charset, bitPos0b: Long): DFDLCharReader

  /**
   * Calling this forces the entire input into memory.
   */
  def lengthInBytes: Long

  def withBitOrder(bitOrder: BitOrder): InStream

  /**
   * Returns a string using up as much as nBytes, along with the number of bits actually consumed.
   * If end of data is reached before nBytes have been consumed, then result will contain as large
   * a string as is possible up to the end of the data.
   *
   * TODO: replace usage in primitives where they getBytes and then call decode themselves.
   * That should be centralized here.
   */
  def getStringInBytes(nBytes: Int, charset: Charset,
    isCharsetFixedWidth: Boolean,
    charsetFixedWidthInBits: Int): (CharSequence, Int) = {
    Assert.usage(!isCharsetFixedWidth ||
      charsetFixedWidthInBits % 8 == 0)
    if (isCharsetFixedWidth) {
      val charsetWidthInBytes = charsetFixedWidthInBits >> 3
      val nChars = nBytes / charsetWidthInBytes
      getStringInChars(nChars, charset, isCharsetFixedWidth, charsetFixedWidthInBits)
    } else {
      Assert.notYetImplemented("specified length in bytes for variable-width character set encoding")
    }
  }

  /**
   * Returns a string using up as much as nChars, along with the number of bits actually consumed.
   * If end of data is reached before nChars have been consumed, then result will contain as large
   * a string as is possible up to the end of the data.
   */
  def getStringInChars(nChars: Int, charset: Charset,
    isCharsetFixedWidth: Boolean,
    charsetFixedWidthInBits: Int): (CharSequence, Int) = {
    Assert.notYetImplemented(!isCharsetFixedWidth, "specified length in characters is not supported for variable width character set encodings")
    val rdr = getCharReader(charset, bitPos0b)
    val str = rdr.getStringInChars(nChars)
    val nBits = str.length * charsetFixedWidthInBits
    (str, nBits)
  }

}

/**
 * Don't use this class directly. Use the factory on InStream object to create.
 */
case class InStreamFromByteChannel private (
  var context: RuntimeData,
  var byteReader: DFDLByteReader,
  var bitPos0b: Long,
  var bitLimit0b: Long, // the bit position position one past the last valid bit position.
  var charLimit0b: Long,
  var reader: Maybe[DFDLCharReader])
  extends InStream
  with Logging
  with WithParseErrorThrowing {

  override def assignFrom(other: InStream) {
    val oth = other.asInstanceOf[InStreamFromByteChannel]
    context = oth.context
    byteReader = oth.byteReader
    bitPos0b = oth.bitPos0b
    bitLimit0b = oth.bitLimit0b
    charLimit0b = oth.charLimit0b
    reader = oth.reader
  }

  override def duplicate() = copy()
  // Let's eliminate duplicate information between charPos of the PState, the
  // InStream, and the Reader. It's the reader, and only the reader.
  def charPos0b = reader.map { _.characterPos }.getOrElse(-1).toLong
  // 
  // the reason for the private constructor above, and this public constructor is that the methods 
  // of this class should NOT have access to the DFDL.Input argument 'in', but only to the DFDLByteReader
  // created from it.
  // 
  // This guarantees then that nobody is doing I/O by going around the DFDLByteReader layer.
  //
  def this(context: ElementRuntimeData, inArg: DFDL.Input, bitStartPos: Long, bitLimit: Long, bitOrder: BitOrder) =
    this(context, new DFDLByteReader(inArg, bitOrder), bitStartPos, bitLimit, -1, Nope)

  /**
   * withPos changes the bit position of the stream, and maintains the char reader
   * which is available to decode characters at that position.
   *
   * It is critical to performance that the reader be preserved if it can be. That is, if we are
   * moving through characters of text in the same encoding, with no binary data or alignment going on, then
   * we *must* retain the reader. Creating a new reader has high overhead in that as soon as you create one and
   * read anything from it, it will read-ahead a large block of characters. If every element was creating
   * a new reader, we'd be reading data over and over again.
   *
   * So it is NOT ok to just pass None as the third argument. Only do that if you have
   * just been handling binary data, or just did an alignmentFill that really inserted some bits.
   *
   * It is well worth it to test and branch to preserve the reader. E.g., AlignmentFill should not
   * create a new reader unless it actually moved over some number of bits. If the alignment is 1 (bit),
   * or the actual amount of alignment fill to be skipped in a particular data stream is 0, then
   * one should preserve the reader.
   */
  def withPos(newBitPos0b: Long, newCharPos0b: Long, newReader: Maybe[DFDLCharReader]): InStream = {
    Assert.invariant((newCharPos0b == -1 && (newReader == Nope)) || (newCharPos0b > -1 && !(newReader == Nope)))
    //    newReader.foreach { rdr =>
    //      if (rdr.characterPos != newCharPos)
    //        println("withPos newCharPos of %s not same as reader characterPos of %s".format(newCharPos, rdr.characterPos))
    //    }
    val res = this.duplicate()
    res.bitPos0b = newBitPos0b
    res.reader = newReader.map { _.atCharPos(newCharPos0b.toInt) }
    res
  }

  def withPos(newBitPos0b: Long, newCharPos0b: Long): InStream = {
    val rdr = {
      if (!reader.isDefined) Nope
      else {
        if (newCharPos0b == -1) Nope
        else {
          // if (rdr.characterPos != newCharPos)
          // println("withPos newCharPos of %s not same as reader characterPos of %s".format(newCharPos, rdr.characterPos))
          One(reader.get.atCharPos(newCharPos0b.toInt)) // TODO: 32-bit size limit! (not our fault)
        }
      }
    }
    val res = this // .duplicate()
    res.bitPos0b = newBitPos0b
    res.reader = rdr
    res
  }

  def withEndBitLimit(newBitLimit0b: Long): InStream = {
    val res = this //.duplicate()
    res.bitLimit0b = newBitLimit0b
    res
  }

  /**
   * changes the bitOrder - must be done at a byte boundary.
   */
  def withBitOrder(bitOrder: BitOrder) = {
    Assert.usage((bitPos0b % 8) == 0)
    copy(byteReader = byteReader.changeBitOrder(bitOrder))
  }

  def getCharReader(charset: Charset, bitPos0b: Long): DFDLCharReader = {
    val rdr = {
      if (!reader.isDefined) {
        // println("Miss: no reader found in PState")
        byteReader.newCharReader(charset, bitPos0b, bitLimit0b)
      } else {
        val rdr = reader.get
        rdr.charset match {
          case `charset` => {
            // println("getCharReader: rdr.bitLimit = " + rdr.bitLimit + " inStream bitLimit = " + bitLimit)
            if (rdr.bitLimit0b >= bitLimit0b)
              rdr.atBitPos(bitPos0b) // use same reader. Just adjust the bitLimit
            else
              byteReader.newCharReader(charset, bitPos0b, bitLimit0b)
          }
          case _ => {
            Assert.invariant(rdr.charset.name() != charset.name())
            //println("Miss: wrong character set encoding.")
            byteReader.newCharReader(charset, bitPos0b, bitLimit0b)
          }
        }
      }
    }
    reader = One(rdr) // cache the reader in the InStream so it will be here again if needed.
    rdr
  }

  def getBytes(bitPos: Long, numBytes: Long): Array[Byte] = {
    // checkBounds(bitPos, 8 * numBytes)
    if (bitPos % 8 == 0) {
      val bytePos = (bitPos >> 3)
      getByteAlignedBytes(bytePos, numBytes)
    } else {
      val bitOrder = byteReader.getBitOrder()
      //
      // We're just getting bytes, but to allow us to reuse
      // getBitSequence we need to have some byte order consistent with the
      // bit order
      val tempByteOrder = if (bitOrder == BitOrder.LeastSignificantBitFirst) {
        java.nio.ByteOrder.LITTLE_ENDIAN
      } else {
        java.nio.ByteOrder.BIG_ENDIAN
      }
      val bigNum = getBigInt(bitPos, numBytes * 8, tempByteOrder, bitOrder)
      val bytes = bigNum.toByteArray
      bytes
    }
  }

  private def asUnsignedByte(b: Byte): Int = if (b < 0) 256 + b else b
  private def asSignedByte(i: Int) = {
    Assert.usage(i >= 0)
    val res = if (i > 127) i - 256 else i
    res.toByte
  }

  private def getByteAlignedBytes(bytePos: Long, numBytes: Long): Array[Byte] = {
    Assert.usage(numBytes <= Int.MaxValue, "32-bit limit on number of bytes (due to underlying libraries restriction.)")
    Assert.usage(bytePos <= Int.MaxValue, "32-bit limit on byte position (due to underlying libraries restriction.)")
    val res = byteReader.getByteArray(bytePos.toInt, numBytes.toInt)
    res
  }

  def reverseBytesAndReverseBits(a: Array[Byte]) {
    reverseBytes(a)
    reverseBitsWithinBytes(a)
  }

  def reverseBitsWithinBytes(a: Array[Byte]) {
    var i: Int = 0
    val len = a.length
    while (i < len) {
      a(i) = Bits.asLSBitFirst(a(i))
      i = i + 1
    }
  }

  def reverseBytes(a: Array[Byte]) {
    var i: Int = 0
    val len = a.length
    while (i < (len >> 1)) {
      // swap positions end to end, 
      // Do this in-place to avoid unnecessary further allocation.
      val upperByte = a(len - i - 1)
      val lowerByte = a(i)
      a(len - i - 1) = lowerByte
      a(i) = upperByte
      i = i + 1
    }
  }

  def getLong(bitPos0b: Long, bitCount: Long, byteOrd: java.nio.ByteOrder, bitOrd: BitOrder) = {
    Assert.usage(bitCount <= 63)
    //
    // TODO: we should have a way to get a long
    // that does not require allocating anything.
    // For now we just want one code path to maintain.
    // So we pay some performance cost.
    //
    val bigInt = getBigInt(bitPos0b, bitCount, byteOrd, bitOrd)
    bigInt.toLong
  }

  /**
   * Constructs a BigInt from the data stream
   */
  def getBigInt(bitPos0b: Long, bitCount: Long, byteOrd: java.nio.ByteOrder, bitOrd: BitOrder): BigInt = {
    checkBounds(bitPos0b, bitCount)
    val zeroByte = 0.toByte
    var bitOrder = bitOrd
    var byteOrder = byteOrd
    val firstBytePos = bitPos0b >> 3
    var numBitsFirstByte = if ((bitPos0b % 8) == 0) 8 else 8 - (bitPos0b % 8).toInt // if the whole byte is in use. 
    val nextBitPos0b = bitPos0b + bitCount
    val lastValidBitPos = nextBitPos0b - 1
    val lastBytePos = lastValidBitPos >> 3
    val numBytes = if (lastBytePos == firstBytePos) 1 else (lastBytePos - firstBytePos).toInt + 1
    var numBitsLastByte: Int = if ((nextBitPos0b % 8) == 0) 8 else (nextBitPos0b % 8).toInt
    var allBytes = getByteAlignedBytes(firstBytePos, numBytes)
    val lastPos = allBytes.length - 1

    if (bitOrder == BitOrder.LeastSignificantBitFirst) {
      //
      // mask off the unused bits of first byte
      //
      val fbMask = (0xFF << (8 - numBitsFirstByte))
      allBytes(0) = (allBytes(0) & fbMask).toByte
      //
      // mask off the unused bits of last byte
      //
      val lbMask = (1 << numBitsLastByte) - 1
      allBytes(lastPos) = (allBytes(lastPos) & lbMask).toByte
      //
      // shift and orient as big-endian bytes
      //
      reverseBitsWithinBytes(allBytes)
      Bits.shiftLeft(allBytes, 8 - numBitsFirstByte)
      reverseBytesAndReverseBits(allBytes)
      //
      // note: sign is handled elsewhere. We 
      // always want to return a non-negative integer
      //
      var res: BigInt = null
      if (allBytes(0) < 0) {
        // sign bit is on. We want an unsigned BigInt
        res = BigInt(zeroByte +: allBytes)
      } else {
        res = BigInt(allBytes)
      }
      res
    } else if (byteOrder == java.nio.ByteOrder.LITTLE_ENDIAN &&
      firstBytePos < lastBytePos) {
      //
      // Conventional little endian case, and at least two bytes
      // are involved.
      //
      // How little-endian vs. big endian works when dealing with fields that 
      // don't start or end on byte boundaries isn't very clear
      //
      // The DFDL spec has a chunk of code for littleEndian that
      // we have in scala as BitsUtils.littleEndianBitValue. This can be used
      // to determine the correct bit values. But it is unsuitable 
      // as an algorithm since it computes the value of individual bits.
      //
      // Our decision is to take the set of bytes that the bits occupy,
      // treat as a list of whole bytes except the last (right-most) byte. The last byte
      // is padded on the right with zeros (the right being least-significant bits).
      // 
      // In general the field begins in the middle of a byte, and ends in the 
      // middle of a byte. 
      // We shift left until the first bit of the field is the first bit of the
      // first byte.
      val shift1 = (8 - numBitsFirstByte)
      val bytes =
        if (shift1 == 0) allBytes // bypass for the bytes-are-aligned case.
        else {
          Bits.shiftLeft(allBytes, shift1)
          //
          // So we now have the field byte starting on a byte boundary on the
          // left. However, the right-most byte might have become all zero. 
          // That is, by aligning our bytes on the left, we may have shifted all
          // the bits of the field into the bytes preceding the last.
          // So we must check for this case, and drop the last byte if this is the
          // case. The right-most byte might not have the value 0, as it could 
          // still have some bits in it that are from beyond the length of the
          // field.
          //
          if (bitCount <= ((allBytes.length - 1) * 8)) {
            // we don't need all the bytes. The shift moved ALL field bits
            // out of the rightmost byte
            allBytes = allBytes.dropRight(1)
          }
        }
      //
      // we may need to zero out some bits of the last byte that are 
      // past the end of the field.
      //
      val numUnusedFinalBits = (allBytes.length * 8) - bitCount
      val unusedMask = ~((1 << numUnusedFinalBits) - 1) & 0xFF
      val last = allBytes.length - 1 // new last position since might have shortened array above
      val lastByte = allBytes(last)
      val newLastByte = lastByte & unusedMask // preserve only the used bits
      allBytes(last) = newLastByte.toByte
      // 
      // Now we reverse the bytes due to little-endianness 
      //
      reverseBytes(allBytes)
      //
      // note: sign is handled elsewhere. We 
      // always want to return a non-negative integer
      //
      var res: BigInt = null
      if (allBytes(0) < 0) {
        // sign bit is on. We want an unsigned BigInt
        res = BigInt(zeroByte +: allBytes)
      } else {
        res = BigInt(allBytes)
      }
      res
    } else {
      //
      // Big endian case
      //
      // Also handles case where there is only one byte
      // so that we don't care about byte order.
      //
      Assert.invariant(numBytes == 1 || byteOrder == java.nio.ByteOrder.BIG_ENDIAN)
      var rawBigNum: BigInt = null
      if (allBytes(0) < 0) {
        // sign bit is on. We want an unsigned BigInt
        rawBigNum = BigInt(zeroByte +: allBytes)
      } else {
        rawBigNum = BigInt(allBytes)
      }
      val shiftRight = 8 - numBitsLastByte
      val shifted = rawBigNum >> shiftRight
      val mask = ((BigInt(1) << bitCount.toInt) - 1)
      val resultBE = shifted & mask
      resultBE
    }
  }

  def checkBounds(bitStart0b: Long, bitLength: Long) {
    if (bitLimit0b > -1)
      if (!(bitStart0b + bitLength <= bitLimit0b))
        throw new IndexOutOfBoundsException("bitStart: %s bitLength: %s".format(bitStart0b, bitLength))
  }

  // This still requires alignment, and that a whole byte is available

  def getByte(bitPos: Long, byteOrder: java.nio.ByteOrder, bitOrder: BitOrder) = {
    checkBounds(bitPos, 8)
    val b = getRawByte(bitPos, byteOrder, bitOrder)
    b
  }

  def getRawByte(bitPos: Long, ignored: java.nio.ByteOrder, bitOrder: BitOrder) = {
    Assert.usage(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    // byteReader.bb.order(order) // must be aligned, so byte order and bit order are irrelevant.
    val b = byteReader.getByte(bytePos) // NOT called getByte(pos)
    b
  }

  /**
   * Calling this forces the entire input into memory.
   */
  def lengthInBytes: Long = byteReader.lengthInBytes

}

class DataLoc(val bitPos1b: Long, bitLimit1b: Long, inStreamOrOutStream: Any) extends DataLocation {
  private val DEFAULT_DUMP_SIZE = 40

  /**
   * FIXME: This whole code path for creating dumps for debug purposes is a crock and
   * needs to be redone.
   */
  def inStream: InStream = inStreamOrOutStream match {
    case is: InStream => is
    case os: OutStream => {
      ???
    }
    case _ => Assert.usageError("Must be an InStream or an OutStream")
  }

  val bytePos1b = (bitPos1b >> 3) + 1

  override def toString() = "byte " + bitPos1b / 8 +
    "\nUTF-8 text starting at byte " + aligned64BitsPos / 8 + " is: (" + utf8Dump() + ")" +
    "\nData (hex) starting at byte " + aligned64BitsPos / 8 + " is: (" + dump() + ")"

  def aligned64BitsPos = (bitPos1b >> 6) << 6

  def byteDump(numBytes: Int = DEFAULT_DUMP_SIZE) = {
    var bytes: List[Byte] = Nil
    try {
      for (i <- 0 until numBytes) {
        bytes = inStream.getRawByte(aligned64BitsPos + (i * 8), java.nio.ByteOrder.BIG_ENDIAN,
          BitOrder.MostSignificantBitFirst) +: bytes
      }
    } catch {
      case e: IndexOutOfBoundsException =>
    }
    bytes.reverse.toArray
  }

  //val cBuf = 
  /**
   * Assumes utf-8
   */
  def utf8Dump(numBytes: Int = DEFAULT_DUMP_SIZE) = {
    val cb = CharBuffer.allocate(128)
    val is = new ByteArrayInputStream(byteDump(numBytes))
    val ir = new InputStreamReader(is)
    val count = ir.read(cb)
    val arr = cb.array
    val chars = for { i <- 0 to count - 1 } yield arr(i)
    chars.mkString("")
  }

  def dump(numBytes: Int = DEFAULT_DUMP_SIZE) = {
    "0x" + bytes2Hex(byteDump(numBytes))
  }

  /*
   * We're at the end if the position is at the limit. 
   */
  def isAtEnd: Boolean = {
    bitPos1b >= bitLimit1b
  }

}
