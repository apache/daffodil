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

// Do no harm number 16 of 626 fail in regression, 154 in total of 797

class TestBinaryInput_01 {

  var startOver: DataInputStream.Mark = null
  var dis: DataInputStream = null

  def fromBytes(ba: Array[Byte], byteOrd: ByteOrder, bitOrd: BitOrder, bitStartPos: Int = 0) = {
    dis = ByteBufferDataInputStream(ba, bitStartPos)
    dis.setBitOrder(bitOrd)
    dis.setByteOrder(byteOrd)
    startOver = dis.mark("TestBinaryInput_01")
    dis
  }

  def fromString(str: String, byteOrd: ByteOrder, bitOrd: BitOrder, bitStartPos: Int = 0) = {
    dis = ByteBufferDataInputStream(ByteBuffer.wrap(str.getBytes("utf-8")), bitStartPos)
    dis.setBitOrder(bitOrd)
    dis.setByteOrder(byteOrd)
    startOver = dis.mark("TestBinaryInput_01")
    dis
  }

  @After def shutDown {
    dis.discard(startOver)
  }

  def getLong(dis: DataInputStream, offset: Int, len: Int) = {
    dis.reset(startOver)
    startOver = dis.mark("TestBinaryInput_01")
    if (offset > 0) dis.skip(offset)
    dis.getUnsignedLong(len).toLong
  }

  def getBigInt(dis: DataInputStream, offset: Int, len: Int) = {
    dis.reset(startOver)
    startOver = dis.mark("TestBinaryInput_01")
    if (offset > 0) dis.skip(offset)
    dis.getSignedBigInt(len)
  }

  /*** DFDL-334 ***/
  // Verify Bit Extraction
  val msbFirst = BitOrder.MostSignificantBitFirst
  val lsbFirst = BitOrder.LeastSignificantBitFirst
  val BE = ByteOrder.BigEndian
  val LE = ByteOrder.LittleEndian

  @Test def testOneBit1() {
    val dis = fromBytes(Misc.bits2Bytes(Seq("00000011")), BE, msbFirst)

    val bit1 = getLong(dis, 6, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(dis, 4, 2)
    assertEquals(0, bit2)
  }

  @Test def testOneBit2() {
    val dis = fromBytes(Misc.bits2Bytes(Seq("11000000")), BE, msbFirst)

    val bit1 = getLong(dis, 0, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(dis, 2, 2)
    assertEquals(0, bit2)
  }

  @Test def testOneBit3() {
    val dis = fromBytes(Misc.bits2Bytes(Seq("00000011")), BE, msbFirst)

    val n = getLong(dis, 6, 2)
    assertEquals(3, n)
  }

  @Test
  def testBufferBitExtraction() {
    val dis = fromString("3", BE, msbFirst)

    assertEquals(3, getLong(dis, 1, 3))
  }

  @Test
  def testBufferBitExtractionShift() {
    val dis = fromString("3", BE, msbFirst)

    assertEquals(12, getLong(dis, 2, 4))
  }

  @Test
  def testBufferLeastSignificantBitExtractionShift() {
    val dis = fromString("4", BE, msbFirst)

    assertEquals(4, getLong(dis, 5, 3))
  }

  // Verify aligned byte/short/int/long/bigint extraction
  @Test
  def testBufferByteBigEndianExtraction() {
    val dis = fromString("3", BE, msbFirst)

    assertEquals(51, getLong(dis, 0, 8))
  }

  @Test
  def testBufferByteLittleEndianExtraction() {
    val dis = fromString("3", LE, msbFirst)

    assertEquals(51, getLong(dis, 0, 8))
  }

  @Test
  def testBufferShortBigEndianExtraction() {
    val dis = fromString("Om", BE, msbFirst, 0)
    assertEquals(20333, getLong(dis, 0, 16))
  }

  @Test
  def testBufferShortLittleEndianExtraction() {
    val dis = fromString("Om", LE, msbFirst, 0)
    assertEquals(27983, getLong(dis, 0, 16))
  }

  @Test
  def testBufferIntBigEndianExtraction() {
    val dis = fromString("Help", BE, msbFirst, 0)
    assertEquals(1214606448, getLong(dis, 0, 32))
  }

  @Test
  def testBufferIntLittleEndianExtraction() {
    val dis = fromString("Help", LE, msbFirst, 0)
    assertEquals(1886152008, getLong(dis, 0, 32))
  }

  @Test
  def testBufferLongBigEndianExtraction() {
    val dis = fromString("Harrison", BE, msbFirst, 0)
    assertEquals(BigInt(5215575679192756078L), getBigInt(dis, 0, 64))
  }

  @Test
  def testBufferLongLittleEndianExtraction() {
    val dis = fromString("Harrison", LE, msbFirst, 0)
    assertEquals(BigInt(7957705963315814728L), getBigInt(dis, 0, 64))
  }

  @Test
  def testBufferBigIntBigEndianExtraction() {
    val dis = fromString("Something in the way she moves, ", BE, msbFirst, 0)
    val bigInt = getBigInt(dis, 0, 256)
    assertEquals(BigInt("37738841482167102822784581157237036764884875846207476558974346160344516471840"),
      bigInt)
  }

  @Test
  def testBufferBigIntLittleEndianExtraction() {
    val dis = fromString("Something in the way she moves, ", LE, msbFirst, 0)
    assertEquals(BigInt("14552548861771956163454220823873430243364312915206513831353612029437431082835"),
      getBigInt(dis, 0, 256))
  }

  // Aligned but not full string
  @Test
  def testBufferPartialIntBigEndianExtraction() {
    val dis = fromString("SBT", BE, msbFirst, 0)
    assertEquals(5456468, getLong(dis, 0, 24))
  }

  @Test
  def testBufferPartialIntLittleEndianExtraction() {
    val dis = fromString("SBT", LE, msbFirst, 0)
    assertEquals(5522003, getLong(dis, 0, 24))
  }

  // Non-Aligned 1 Byte or less
  @Test
  def testBufferBitNumberBigEndianExtraction() {
    val dis = fromString("3", BE, msbFirst)

    assertEquals(3, getLong(dis, 1, 3))
  }

  @Test
  def testBufferBitNumberLittleEndianExtraction() {
    val dis = fromString("3", LE, msbFirst)

    assertEquals(3, getLong(dis, 1, 3))
  }

  @Test
  def testBufferBitByteBigEndianExtraction() {
    val dis = fromString("3>", BE, msbFirst, 0)
    assertEquals(204, getLong(dis, 2, 8))
  }

  @Test
  def testBufferBitByteLittleEndianExtraction() {
    val dis = fromString("3>", LE, msbFirst, 0)
    assertEquals(0xCC, getLong(dis, 2, 8))
  }

  // Non-Aligned multi-byte
  @Test
  def testBufferPartialInt22At0BigEndianExtraction() {
    val dis = fromString("SBT", BE, msbFirst, 0)
    assertEquals(1364117, getLong(dis, 0, 22))
  }

  @Test
  def testBufferPartialInt22At0LittleEndianExtraction() {
    val dis = fromString("SBT", LE, msbFirst, 0)
    assertEquals(0x154253, getLong(dis, 0, 22))
    // Corrected. Was 544253, but that omits shifting the most significant byte left 2
    // because the field is only 22 long.
  }

  @Test
  def testBufferPartialInt22At2BigEndianExtraction() {
    val dis = fromString("SBT", BE, msbFirst, 0)
    assertEquals(0x134254, getLong(dis, 2, 22))
  }

  @Test
  def testBufferPartialInt22At2LittleEndianExtraction() {
    val dis = fromString("SBT", LE, msbFirst, 0)
    assertEquals(0x14094d, getLong(dis, 2, 22)) // Corrected.
    // Was 0x50094d, but that omits shifting the most significant byte >> 2 to
    // add the bits on the most significant side, not the least significant side.
  }

  @Test def testOneBit1LSBFirst() {
    val dis = fromBytes(Misc.bits2Bytes(Seq("01100000")), LE, lsbFirst)

    val bit1 = getLong(dis, 5, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(dis, 4, 2)
    assertEquals(2, bit2)
  }

  @Test def testOneBit2LSBFirst() {
    val dis = fromBytes(Misc.bits2Bytes(Seq("01010000")), LE, lsbFirst)

    val bit1 = getLong(dis, 5, 2)
    assertEquals(2, bit1)
    val bit2 = getLong(dis, 4, 2)
    assertEquals(1, bit2)
  }

  @Test def testOneBit3LSBFirst() {
    val dis = fromBytes(BigInt(0xE4567A).toByteArray, LE, lsbFirst)

    val bit1 = getLong(dis, 13, 12)
    assertEquals(0x2B7, bit1)
  }
}
