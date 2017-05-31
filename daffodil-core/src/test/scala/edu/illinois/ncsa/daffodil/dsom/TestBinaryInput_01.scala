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

package edu.illinois.ncsa.daffodil.dsom

import junit.framework.Assert._
import org.junit.Test
import org.junit.After
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.FormatInfo
import java.nio.charset.CharsetEncoder
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width

// Do no harm number 16 of 626 fail in regression, 154 in total of 797

class TestBinaryInput_01 {

  class FakeFormatInfo(val bitOrder: BitOrder, val byteOrder: ByteOrder) extends FormatInfo {
    def encoder: CharsetEncoder = ???
    def decoder: CharsetDecoder = ???
    def reportingDecoder: CharsetDecoder = ???
    def replacingDecoder: CharsetDecoder = ???
    def fillByte: Byte = ???

    def binaryFloatRep: BinaryFloatRep = ???
    def maybeCharWidthInBits: MaybeInt = ???
    def maybeUTF16Width: Maybe[UTF16Width] = ???
    def encodingMandatoryAlignmentInBits: Int = ???
    def encodingErrorPolicy: EncodingErrorPolicy = ???
  }

  var startOver: DataInputStream.Mark = null
  var dis: DataInputStream = null

  def fromBytes(ba: Array[Byte], byteOrd: ByteOrder, bitOrd: BitOrder, bitStartPos: Int = 0) = {
    dis = ByteBufferDataInputStream(ba, bitStartPos)
    startOver = dis.mark("TestBinaryInput_01")
    val finfo = new FakeFormatInfo(bitOrd, byteOrd)
    (dis, finfo)
  }

  def fromString(str: String, byteOrd: ByteOrder, bitOrd: BitOrder, bitStartPos: Int = 0) = {
    dis = ByteBufferDataInputStream(ByteBuffer.wrap(str.getBytes("utf-8")), bitStartPos)
    startOver = dis.mark("TestBinaryInput_01")
    val finfo = new FakeFormatInfo(bitOrd, byteOrd)
    (dis, finfo)
  }

  @After def shutDown {
    dis.discard(startOver)
  }

  def getLong(finfo: FormatInfo, dis: DataInputStream, offset: Int, len: Int) = {
    dis.reset(startOver)
    startOver = dis.mark("TestBinaryInput_01")
    if (offset > 0) dis.skip(offset, finfo)
    dis.getUnsignedLong(len, finfo).toLong
  }

  def getBigInt(finfo: FormatInfo, dis: DataInputStream, offset: Int, len: Int) = {
    dis.reset(startOver)
    startOver = dis.mark("TestBinaryInput_01")
    if (offset > 0) dis.skip(offset, finfo)
    dis.getSignedBigInt(len, finfo)
  }

  /*** DFDL-334 ***/
  // Verify Bit Extraction
  val msbFirst = BitOrder.MostSignificantBitFirst
  val lsbFirst = BitOrder.LeastSignificantBitFirst
  val BE = ByteOrder.BigEndian
  val LE = ByteOrder.LittleEndian

  @Test def testOneBit1() {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("00000011")), BE, msbFirst)

    val bit1 = getLong(finfo, dis, 6, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(finfo, dis, 4, 2)
    assertEquals(0, bit2)
  }

  @Test def testOneBit2() {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("11000000")), BE, msbFirst)

    val bit1 = getLong(finfo, dis, 0, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(finfo, dis, 2, 2)
    assertEquals(0, bit2)
  }

  @Test def testOneBit3() {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("00000011")), BE, msbFirst)

    val n = getLong(finfo, dis, 6, 2)
    assertEquals(3, n)
  }

  @Test
  def testBufferBitExtraction() {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(3, getLong(finfo, dis, 1, 3))
  }

  @Test
  def testBufferBitExtractionShift() {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(12, getLong(finfo, dis, 2, 4))
  }

  @Test
  def testBufferLeastSignificantBitExtractionShift() {
    val (dis, finfo) = fromString("4", BE, msbFirst)

    assertEquals(4, getLong(finfo, dis, 5, 3))
  }

  // Verify aligned byte/short/int/long/bigint extraction
  @Test
  def testBufferByteBigEndianExtraction() {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(51, getLong(finfo, dis, 0, 8))
  }

  @Test
  def testBufferByteLittleEndianExtraction() {
    val (dis, finfo) = fromString("3", LE, msbFirst)

    assertEquals(51, getLong(finfo, dis, 0, 8))
  }

  @Test
  def testBufferShortBigEndianExtraction() {
    val (dis, finfo) = fromString("Om", BE, msbFirst, 0)
    assertEquals(20333, getLong(finfo, dis, 0, 16))
  }

  @Test
  def testBufferShortLittleEndianExtraction() {
    val (dis, finfo) = fromString("Om", LE, msbFirst, 0)
    assertEquals(27983, getLong(finfo, dis, 0, 16))
  }

  @Test
  def testBufferIntBigEndianExtraction() {
    val (dis, finfo) = fromString("Help", BE, msbFirst, 0)
    assertEquals(1214606448, getLong(finfo, dis, 0, 32))
  }

  @Test
  def testBufferIntLittleEndianExtraction() {
    val (dis, finfo) = fromString("Help", LE, msbFirst, 0)
    assertEquals(1886152008, getLong(finfo, dis, 0, 32))
  }

  @Test
  def testBufferLongBigEndianExtraction() {
    val (dis, finfo) = fromString("Harrison", BE, msbFirst, 0)
    assertEquals(BigInt(5215575679192756078L), getBigInt(finfo, dis, 0, 64))
  }

  @Test
  def testBufferLongLittleEndianExtraction() {
    val (dis, finfo) = fromString("Harrison", LE, msbFirst, 0)
    assertEquals(BigInt(7957705963315814728L), getBigInt(finfo, dis, 0, 64))
  }

  @Test
  def testBufferBigIntBigEndianExtraction() {
    val (dis, finfo) = fromString("Something in the way she moves, ", BE, msbFirst, 0)
    val bigInt = getBigInt(finfo, dis, 0, 256)
    assertEquals(BigInt("37738841482167102822784581157237036764884875846207476558974346160344516471840"),
      bigInt)
  }

  @Test
  def testBufferBigIntLittleEndianExtraction() {
    val (dis, finfo) = fromString("Something in the way she moves, ", LE, msbFirst, 0)
    assertEquals(BigInt("14552548861771956163454220823873430243364312915206513831353612029437431082835"),
      getBigInt(finfo, dis, 0, 256))
  }

  // Aligned but not full string
  @Test
  def testBufferPartialIntBigEndianExtraction() {
    val (dis, finfo) = fromString("SBT", BE, msbFirst, 0)
    assertEquals(5456468, getLong(finfo, dis, 0, 24))
  }

  @Test
  def testBufferPartialIntLittleEndianExtraction() {
    val (dis, finfo) = fromString("SBT", LE, msbFirst, 0)
    assertEquals(5522003, getLong(finfo, dis, 0, 24))
  }

  // Non-Aligned 1 Byte or less
  @Test
  def testBufferBitNumberBigEndianExtraction() {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(3, getLong(finfo, dis, 1, 3))
  }

  @Test
  def testBufferBitNumberLittleEndianExtraction() {
    val (dis, finfo) = fromString("3", LE, msbFirst)

    assertEquals(3, getLong(finfo, dis, 1, 3))
  }

  @Test
  def testBufferBitByteBigEndianExtraction() {
    val (dis, finfo) = fromString("3>", BE, msbFirst, 0)
    assertEquals(204, getLong(finfo, dis, 2, 8))
  }

  @Test
  def testBufferBitByteLittleEndianExtraction() {
    val (dis, finfo) = fromString("3>", LE, msbFirst, 0)
    assertEquals(0xCC, getLong(finfo, dis, 2, 8))
  }

  // Non-Aligned multi-byte
  @Test
  def testBufferPartialInt22At0BigEndianExtraction() {
    val (dis, finfo) = fromString("SBT", BE, msbFirst, 0)
    assertEquals(1364117, getLong(finfo, dis, 0, 22))
  }

  @Test
  def testBufferPartialInt22At0LittleEndianExtraction() {
    val (dis, finfo) = fromString("SBT", LE, msbFirst, 0)
    assertEquals(0x154253, getLong(finfo, dis, 0, 22))
    // Corrected. Was 544253, but that omits shifting the most significant byte left 2
    // because the field is only 22 long.
  }

  @Test
  def testBufferPartialInt22At2BigEndianExtraction() {
    val (dis, finfo) = fromString("SBT", BE, msbFirst, 0)
    assertEquals(0x134254, getLong(finfo, dis, 2, 22))
  }

  @Test
  def testBufferPartialInt22At2LittleEndianExtraction() {
    val (dis, finfo) = fromString("SBT", LE, msbFirst, 0)
    assertEquals(0x14094d, getLong(finfo, dis, 2, 22)) // Corrected.
    // Was 0x50094d, but that omits shifting the most significant byte >> 2 to
    // add the bits on the most significant side, not the least significant side.
  }

  @Test def testOneBit1LSBFirst() {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("01100000")), LE, lsbFirst)

    val bit1 = getLong(finfo, dis, 5, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(finfo, dis, 4, 2)
    assertEquals(2, bit2)
  }

  @Test def testOneBit2LSBFirst() {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("01010000")), LE, lsbFirst)

    val bit1 = getLong(finfo, dis, 5, 2)
    assertEquals(2, bit1)
    val bit2 = getLong(finfo, dis, 4, 2)
    assertEquals(1, bit2)
  }

  @Test def testOneBit3LSBFirst() {
    val (dis, finfo) = fromBytes(BigInt(0xE4567A).toByteArray, LE, lsbFirst)

    val bit1 = getLong(finfo, dis, 13, 12)
    assertEquals(0x2B7, bit1)
  }
}
