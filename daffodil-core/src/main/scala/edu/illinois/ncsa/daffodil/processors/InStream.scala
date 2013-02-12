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

import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Misc._
import java.io.ByteArrayInputStream
import java.nio.charset.Charset
import java.nio.CharBuffer
import java.io.InputStreamReader

object InStream {
  def fromByteChannel(context: ElementBase, in: DFDL.Input, bitOffset: Long, bitLimit: Long) = {
    new InStreamFromByteChannel(context, in, bitOffset, bitLimit)
  }
}

/**
 * Encapsulates the I/O as an abstraction that works something like a java.nio.ByteBuffer
 * but a bit more specialized for DFDL needs, e.g., supports offsets and positions in bits.
 */
trait InStream {

  def bitPos: Long
  def bitLimit: Long
  def charPos: Long
  def charLimit: Long
  def reader: Option[DFDLCharReader]

  def withPos(newBitPos: Long, newCharPos: Long, newReader: Option[DFDLCharReader]): InStream
  def withPos(newBitPos: Long, newCharPos: Long): InStream
  def withEndBitLimit(newBitLimit: Long): InStream

  /**
   * Checks that all 8-bits are available, and requires alignment also.
   */
  def getByte(bitPos: Long, order: java.nio.ByteOrder): Byte

  /**
   * Will deliver a byte even if the bit limit implies only a fragment of a
   * byte is actually available.
   */
  def getRawByte(bitPos: Long, order: java.nio.ByteOrder): Byte

  /**
   * Returns up to numBytes. Could be fewer. Does not check bitLimit bounds
   * precisely.
   */
  def getBytes(bitPos: Long, numBytes: Long): Array[Byte]

  def getBitSequence(bitPos: Long, bitCount: Long, order: java.nio.ByteOrder): (BigInt, Long)

  // TODO: remove if no longer needed
  // def withLimit(startBitPos: Long, endBitPos: Long): InStream

  def getCharReader(charset: Charset, bitPos: Long): DFDLCharReader

  // Removed: bad idea. Makes copy of entire input
  // def getAllBytes: Array[Byte]
}

/**
 * Don't use this class directly. Use the factory on InStream object to create.
 */
case class InStreamFromByteChannel private (val context: ElementBase,
                                            val byteReader: DFDLByteReader,
                                            val bitPos: Long,
                                            val bitLimit: Long,
                                            val charLimit: Long,
                                            var reader: Option[DFDLCharReader])
  extends InStream
  with Logging
  with WithParseErrorThrowing {

  // Let's eliminate duplicate information between charPos of the PState, the
  // InStream, and the Reader. It's the reader, and only the reader.
  def charPos = reader.map { _.characterPos }.getOrElse(-1).toLong
  // 
  // the reason for the private constructor above, and this public constructor is that the methods 
  // of this class should NOT have access to the DFDL.Input argument 'in', but only to the DFDLByteReader
  // created from it.
  // 
  // This guarantees then that nobody is doing I/O by going around the DFDLByteReader layer.
  //
  def this(context: ElementBase, inArg: DFDL.Input, bitOffset: Long, bitLimit: Long) =
    this(context, new DFDLByteReader(inArg), bitOffset, bitLimit, -1, None)

  def withPos(newBitPos: Long, newCharPos: Long, newReader: Option[DFDLCharReader]): InStream = {
    Assert.invariant((newCharPos == -1 && (newReader == None)) || (newCharPos > -1 && !(newReader == None)))
    //    newReader.foreach { rdr =>
    //      if (rdr.characterPos != newCharPos)
    //        println("withPos newCharPos of %s not same as reader characterPos of %s".format(newCharPos, rdr.characterPos))
    //    }
    copy(bitPos = newBitPos, reader = newReader.map { _.atCharPos(newCharPos.toInt) })
  }

  def withPos(newBitPos: Long, newCharPos: Long): InStream = {
    val rdr = reader match {
      case None => {
        //        if (newCharPos != -1)
        //          println("withPos setting newCharPos to value %s when there is no reader".format(newCharPos))
        None
      }
      case Some(rdr) if (newCharPos == -1) => None
      case Some(rdr) => {
        // if (rdr.characterPos != newCharPos)
        // println("withPos newCharPos of %s not same as reader characterPos of %s".format(newCharPos, rdr.characterPos))
        Some(rdr.atCharPos(newCharPos.toInt)) // TODO: 32-bit offset limit! (not our fault)
      }
    }
    copy(bitPos = newBitPos, reader = rdr)
  }

  def withEndBitLimit(newBitLimit: Long): InStream = {
    copy(bitLimit = newBitLimit)
  }

  def getCharReader(charset: Charset, bitPos: Long): DFDLCharReader = {
    val rdr = reader match {
      case None => {
        // println("Miss: no reader found in PState")
        byteReader.newCharReader(charset, bitPos, bitLimit)
      }
      case Some(rdr) => {
        rdr.charset match {
          case `charset` => {
            // println("getCharReader: rdr.bitLimit = " + rdr.bitLimit + " inStream bitLimit = " + bitLimit)
            if (rdr.bitLimit >= bitLimit)
              rdr.atBitPos(bitPos) // use same reader. Just adjust the bitLimit
            else
              byteReader.newCharReader(charset, bitPos, bitLimit)
          }
          case _ => {
            Assert.invariant(rdr.charset.name() != charset.name())
            //println("Miss: wrong character set encoding.")
            byteReader.newCharReader(charset, bitPos, bitLimit)
          }
        }
      }
    }
    reader = Some(rdr) // cache the reader in the InStream so it will be here again if needed.
    rdr
  }

  private val emptyByteArray = Array[Byte]()

  def getBytes(bitPos: Long, numBytes: Long): Array[Byte] = {
    // checkBounds(bitPos, 8 * numBytes)
    if (bitPos % 8 == 0) {
      val bytePos = (bitPos >> 3)
      getByteAlignedBytes(bytePos, numBytes)
    } else {
      getUnalignedBytes(bitPos, numBytes)
    }

  }

  // Removed: Bad idea. Makes copy of entire input
  //  def getAllBytes: Array[Byte] = {
  //    val bb = byteReader.bb
  //    bb.array()
  //  }

  private def asUnsignedByte(b: Byte): Int = if (b < 0) 256 + b else b
  private def asSignedByte(i: Int) = {
    Assert.usage(i >= 0)
    val res = if (i > 127) i - 256 else i
    res.toByte
  }

  private def getUnalignedBytes(bitPos: Long, numBytes: Long): Array[Byte] = {
    Assert.usage(numBytes <= Int.MaxValue, "32-bit limit on number of bytes (due to underlying libraries restriction.)")
    Assert.usage((bitPos >> 3) <= Int.MaxValue, "32-bit limit on byte position (due to underlying libraries restriction.)")
    val bytePos = bitPos >> 3
    val byteShift = bitPos & 0x7
    Assert.usage(byteShift != 0, "Shouldn't be called if bytes are aligned.")
    val byteSuperset = getByteAlignedBytes(bytePos, numBytes + 1).map { asUnsignedByte(_) }
    val bitsKeeping = byteSuperset.map { b => ((b << byteShift) & 0xFF) >> byteShift }.slice(1, byteSuperset.length - 1)
    val bitsAddingToNextByte = byteSuperset.map { b => (b >> byteShift) << byteShift }.slice(1, byteSuperset.length)
    val bothParts = bitsAddingToNextByte zip bitsKeeping
    val resultInts = bothParts.map { case (adding, keeping) => adding | keeping }
    val resultBytes = resultInts.map { asSignedByte(_) }
    resultBytes
  }

  private def getByteAlignedBytes(bytePos: Long, numBytes: Long): Array[Byte] = {
    Assert.usage(numBytes <= Int.MaxValue, "32-bit limit on number of bytes (due to underlying libraries restriction.)")
    Assert.usage(bytePos <= Int.MaxValue, "32-bit limit on byte position (due to underlying libraries restriction.)")
    val res = byteReader.getByteArray(bytePos.toInt, numBytes.toInt)
    res
  }

  abstract class EndianTraits(val startBit: Long, val bitCount: Long) {
    lazy val byteLength = 8.toLong
    lazy val alignmentOffsetLength = startBit & 7
    lazy val isAligned = alignmentOffsetLength == 0
    lazy val startBitInByteRemaining = if (isAligned) byteLength - alignmentOffsetLength else 0
    lazy val shortByteLength = bitCount & 7
    lazy val wholeBytesLength = bitCount - shortByteLength
    lazy val wholeBytesSize = wholeBytesLength >>> 3
    lazy val isShortSplit = alignmentOffsetLength + shortByteLength > byteLength
    lazy val restOfBytesAlignment = (alignmentOffsetLength + initialByteLength) & 7
    lazy val finalBytesAlignment = (alignmentOffsetLength + shortByteLength) & 7
    lazy val isSplit = restOfBytesAlignment != 0
    lazy val longByteLength = if (wholeBytesSize == 0) 0 else byteLength
    val isInitialSplit: Boolean
    val isFinalSplit: Boolean
    val initialShiftLeft: Long
    val nextByteShiftLeft: Long
    val initialByteLength: Long
    val finalByteLength: Long
    lazy val initialTopByteShiftCount = if (isInitialSplit) restOfBytesAlignment else 0
    lazy val topByteShiftCount = restOfBytesAlignment
    lazy val finalTopByteShiftCount = if (isFinalSplit) finalBytesAlignment else 0
    lazy val initialTopByteLength = initialByteLength - initialTopByteShiftCount
    lazy val topByteLength = byteLength - restOfBytesAlignment
    lazy val finalTopByteLength = finalByteLength - finalTopByteShiftCount
    lazy val initialBottomByteLength = initialTopByteShiftCount
    lazy val bottomByteLength = topByteShiftCount
    lazy val finalBottomByteLength = finalTopByteShiftCount
    lazy val hasInitialByte = initialByteLength != 0
    lazy val hasFinalByte = finalByteLength != 0
    lazy val hasShortByte = shortByteLength != 0
  }
  case class BigEndianTraits(override val startBit: Long, override val bitCount: Long) extends EndianTraits(startBit, bitCount) {
    lazy val isInitialSplit = isShortSplit
    lazy val isFinalSplit = isSplit
    lazy val initialShiftLeft = if (hasShortByte) wholeBytesLength else wholeBytesLength - byteLength
    lazy val nextByteShiftLeft = -byteLength
    lazy val initialByteLength = shortByteLength
    lazy val finalByteLength = longByteLength
  }
  case class LittleEndianTraits(override val startBit: Long, override val bitCount: Long) extends EndianTraits(startBit, bitCount) {
    lazy val isInitialSplit = isSplit
    lazy val isFinalSplit = isShortSplit
    lazy val initialShiftLeft = 0.toLong
    lazy val nextByteShiftLeft = byteLength
    lazy val initialByteLength = longByteLength
    lazy val finalByteLength = shortByteLength
  }

  def getEndianTraits(bitPos: Long, bitCount: Long, order: java.nio.ByteOrder) = order match {
    case java.nio.ByteOrder.BIG_ENDIAN => BigEndianTraits(bitPos, bitCount)
    case java.nio.ByteOrder.LITTLE_ENDIAN => LittleEndianTraits(bitPos, bitCount)
    case _ => Assert.invariantFailed("Invalid Byte Order: " + order)
  }

  def getBitSequence(bitPos: Long, bitCount: Long, order: java.nio.ByteOrder): (BigInt, Long) = {
    checkBounds(bitPos, bitCount)
    val worker: EndianTraits = getEndianTraits(bitPos, bitCount, order)
    var result = BigInt(0)
    var position = worker.startBit
    var outShift = worker.initialShiftLeft

    // Read first byte (be it complete or partial)
    if (worker.hasInitialByte) {
      result =
        (BigInt(
          if (worker.isInitialSplit) {
            (getPartialByte(position, worker.initialTopByteLength, worker.initialTopByteShiftCount) |
              getPartialByte(position + worker.initialTopByteLength, worker.initialBottomByteLength, 0)).toByte
          } else {
            getPartialByte(position, worker.initialByteLength, 0)
          }) & 0xFF) << outShift.toInt
      position = position + worker.initialByteLength
      outShift = outShift + worker.nextByteShiftLeft
    }

    // Next all the middle bytes; we skip one byte because that will be handled either in the initial or final handler
    for (thisByte <- 1 until worker.wholeBytesSize.toInt) {
      result = result +
        ((BigInt(
          if (worker.isSplit) {
            (getPartialByte(position, worker.topByteLength, worker.topByteShiftCount) |
              getPartialByte(position + worker.topByteLength, worker.bottomByteLength, 0)).toByte
          } else {
            getPartialByte(position, worker.byteLength, 0)
          }) & 0xFF) << outShift.toInt)
      position = position + worker.byteLength
      outShift = outShift + worker.nextByteShiftLeft
    }

    // Read first byte (be it complete or partial)
    if (worker.hasFinalByte) {
      result = result +
        ((BigInt(
          if (worker.isFinalSplit) {
            (getPartialByte(position, worker.finalTopByteLength, worker.finalTopByteShiftCount) |
              getPartialByte(position + worker.finalTopByteLength, worker.finalBottomByteLength, 0)).toByte
          } else {
            getPartialByte(position, worker.finalByteLength, 0)
          }) & 0xFF) << outShift.toInt)
      position = position + worker.finalByteLength
      outShift = outShift + worker.nextByteShiftLeft
    }

    (result, position)
  }

  // littleEndian shift left except last, bigEndian shift right except first
  def getPartialByte(bitPos: Long, bitCount: Long, shift: Long = 0): Int = {
    Assert.invariant(shift >= 0 && shift + bitCount <= 8)
    val bytePos = (bitPos >>> 3).toInt
    val bitOffset = (bitPos % 8).toByte
    var result: Int = byteReader.getByte(bytePos)
    result = if (result < 0) 256 + result else result

    if (bitCount != 8) {
      Assert.invariant(0 < bitCount && bitCount <= 8 && bitOffset + bitCount <= 8)
      val mask = ((1 << bitCount) - 1) << (8 - bitOffset - bitCount)

      result = (result & mask)

      // Shift so LSB of result is at LSB of octet then mask off top bits
      val finalShift = 8 - bitCount - bitOffset - shift
      val res =
        if (finalShift < 0) result << -finalShift
        else result >> finalShift
      res
    } else {
      // Verify byte alignment and disallow shift
      Assert.invariant(bitOffset == 0 && shift == 0)
      result
    }
  }

  def checkBounds(bitStart: Long, bitLength: Long) {
    if (bitLimit > -1)
      if (!(bitStart + bitLength <= bitLimit))
        throw new IndexOutOfBoundsException("bitStart: %s bitLength: %s".format(bitStart, bitLength))
  }

  // This still requires alignment, and that a whole byte is available

  def getByte(bitPos: Long, order: java.nio.ByteOrder) = {
    checkBounds(bitPos, 8)
    getRawByte(bitPos, order)
  }

  def getRawByte(bitPos: Long, ignored: java.nio.ByteOrder) = {
    Assert.usage(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    // byteReader.bb.order(order) // must be aligned, so byte order is irrelevant.
    byteReader.getByte(bytePos) // NOT called getByte(pos)
  }

  // Let's not actually shorten the stream. There are too many 
  // places that need to deal with running into the length limit, so
  // that really does have to work. This is overkill.
  // TODO: remove this once we're sure we don't need it anymore.
  //  def withLimit(startBitPos: Long, endBitPos: Long): InStream = {
  //    // Appears to only be called from lengthKind=Pattern match code
  //    Assert.invariant((startBitPos & 7) == 0)
  //    Assert.invariant((endBitPos & 7) == 0)
  //    val startByte = startBitPos / 8
  //    val endByte = (endBitPos + 7) / 8
  //    val count = endByte - startByte
  //    var bytes: Array[Byte] = new Array(count.asInstanceOf[Int])
  //    val oldPos = byteReader.bb.position
  //    byteReader.bb.position(startByte.asInstanceOf[Int])
  //    byteReader.bb.get(bytes, 0, count.asInstanceOf[Int])
  //    val inputStream = new ByteArrayInputStream(bytes)
  //    val rbc = java.nio.channels.Channels.newChannel(inputStream)
  //    byteReader.bb.position(oldPos)
  //    val newInStream = InStream.fromByteChannel(context, rbc, sizeHint)
  //    newInStream
  //  }

}

class DataLoc(bitPos: Long, bitLimit: Long, inStream: InStream) extends DataLocation {
  private val DEFAULT_DUMP_SIZE = 40

  override def toString() = "byte " + bitPos / 8 +
    "\nUTF-8 text starting at byte " + aligned64BitsPos / 8 + " is: (" + utf8Dump() + ")" +
    "\nData (hex) starting at byte " + aligned64BitsPos / 8 + " is: (" + dump() + ")"

  def aligned64BitsPos = (bitPos >> 6) << 6

  def byteDump(numBytes: Int = DEFAULT_DUMP_SIZE) = {
    var bytes: List[Byte] = Nil
    try {
      for (i <- 0 until numBytes) {
        bytes = inStream.getRawByte(aligned64BitsPos + (i * 8), java.nio.ByteOrder.BIG_ENDIAN) +: bytes
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
    bytes2Hex(byteDump(numBytes))
  }

  /*
   * We're at the end if an attempt to get a bit fails with an index exception
   */
  def isAtEnd: Boolean = {
    bitPos >= bitLimit
  }
  //  def isAtEnd: Boolean = {
  //    try {
  //      inStream.getBitSequence(bitPos, 1, java.nio.ByteOrder.BIG_ENDIAN)
  //      false
  //    } catch {
  //      case e: IndexOutOfBoundsException => {
  //        val exc = e
  //        true
  //      }
  //    }
  //  }
}
