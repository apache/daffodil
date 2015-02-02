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
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

// Do no harm number 16 of 626 fail in regression, 154 in total of 797

class TestBinaryInput_01 {

  /*** DFDL-334 ***/
  // Verify Bit Extraction
  val msbFirst = BitOrder.MostSignificantBitFirst
  val lsbFirst = BitOrder.LeastSignificantBitFirst
  val BE = java.nio.ByteOrder.BIG_ENDIAN
  val LE = java.nio.ByteOrder.LITTLE_ENDIAN

  @Test def testOneBit1() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes(Seq("00000011")))
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    val bit1 = inStream.getLong(6, 2, BE, msbFirst)
    assertEquals(3, bit1)
    val bit2 = inStream.getLong(4, 2, BE, msbFirst)
    assertEquals(0, bit2)
  }

  @Test def testOneBit2() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes(Seq("11000000")))
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    val bit1 = inStream.getLong(0, 2, BE, msbFirst)
    assertEquals(3, bit1)
    val bit2 = inStream.getLong(2, 2, BE, msbFirst)
    assertEquals(0, bit2)
  }

  @Test def testOneBit3() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes(Seq("00000011")))
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    val n = inStream.getLong(6, 2, BE, msbFirst)
    assertEquals(3, n)
  }

  @Test
  def testBufferBitExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    assertEquals(3, inStream.getLong(1, 3, BE, msbFirst))
  }

  @Test
  def testBufferBitExtractionShift() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    assertEquals(12, inStream.getLong(2, 4, BE, msbFirst))
  }

  @Test
  def testBufferLeastSignificantBitExtractionShift() {
    var in = Misc.stringToReadableByteChannel("4")
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    assertEquals(4, inStream.getLong(5, 3, BE, msbFirst))
  }

  // Verify aligned byte/short/int/long/bigint extraction
  @Test
  def testBufferByteBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    assertEquals(51, inStream.getLong(0, 8, BE, msbFirst))
  }

  @Test
  def testBufferByteLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    assertEquals(51, inStream.getLong(0, 8, LE, msbFirst))
  }

  @Test
  def testBufferShortBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Om")
    val inStream = InStream.fromByteChannel(null, in, 2, -1, msbFirst)
    assertEquals(20333, inStream.getLong(0, 16, BE, msbFirst))
  }

  @Test
  def testBufferShortLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Om")
    val inStream = InStream.fromByteChannel(null, in, 2, -1, msbFirst)
    assertEquals(27983, inStream.getLong(0, 16, LE, msbFirst))
  }

  @Test
  def testBufferIntBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Help")
    val inStream = InStream.fromByteChannel(null, in, 4, -1, msbFirst)
    assertEquals(1214606448, inStream.getLong(0, 32, BE, msbFirst))
  }

  @Test
  def testBufferIntLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Help")
    val inStream = InStream.fromByteChannel(null, in, 4, -1, msbFirst)
    assertEquals(1886152008, inStream.getLong(0, 32, LE, msbFirst))
  }

  @Test
  def testBufferLongBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Harrison")
    val inStream = InStream.fromByteChannel(null, in, 8, -1, msbFirst)
    assertEquals(BigInt(5215575679192756078L), inStream.getBigInt(0, 64, BE, msbFirst))
  }

  @Test
  def testBufferLongLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Harrison")
    val inStream = InStream.fromByteChannel(null, in, 8, -1, msbFirst)
    assertEquals(BigInt(7957705963315814728L), inStream.getBigInt(0, 64, LE, msbFirst))
  }

  @Test
  def testBufferBigIntBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Something in the way she moves, ")
    val inStream = InStream.fromByteChannel(null, in, 32, -1, msbFirst)
    val bigInt = inStream.getBigInt(0, 256, BE, msbFirst)
    assertEquals(BigInt("37738841482167102822784581157237036764884875846207476558974346160344516471840"),
      bigInt)
  }

  @Test
  def testBufferBigIntLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Something in the way she moves, ")
    val inStream = InStream.fromByteChannel(null, in, 32, -1, msbFirst)
    assertEquals(BigInt("14552548861771956163454220823873430243364312915206513831353612029437431082835"),
      inStream.getBigInt(0, 256, LE, msbFirst))
  }

  // Aligned but not full string
  @Test
  def testBufferPartialIntBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1, msbFirst)
    assertEquals(5456468, inStream.getLong(0, 24, BE, msbFirst))
  }

  @Test
  def testBufferPartialIntLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1, msbFirst)
    assertEquals(5522003, inStream.getLong(0, 24, LE, msbFirst))
  }

  // Non-Aligned 1 Byte or less
  @Test
  def testBufferBitNumberBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    assertEquals(3, inStream.getLong(1, 3, BE, msbFirst))
  }

  @Test
  def testBufferBitNumberLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    assertEquals(3, inStream.getLong(1, 3, LE, msbFirst))
  }

  @Test
  def testBufferBitByteBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3>")
    val inStream = InStream.fromByteChannel(null, in, 2, -1, msbFirst)
    assertEquals(204, inStream.getLong(2, 8, BE, msbFirst))
  }

  @Test
  def testBufferBitByteLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3>")
    val inStream = InStream.fromByteChannel(null, in, 2, -1, msbFirst)
    assertEquals(0xCC, inStream.getLong(2, 8, LE, msbFirst))
  }

  // Non-Aligned multi-byte
  @Test
  def testBufferPartialInt22At0BigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1, msbFirst)
    assertEquals(1364117, inStream.getLong(0, 22, BE, msbFirst))
  }

  @Test
  def testBufferPartialInt22At0LittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1, msbFirst)
    assertEquals(0x544253, inStream.getLong(0, 22, LE, msbFirst))
  }

  @Test
  def testBufferPartialInt22At2BigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1, msbFirst)
    assertEquals(0x134254, inStream.getLong(2, 22, BE, msbFirst))
  }

  @Test
  def testBufferPartialInt22At2LittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1, msbFirst)
    assertEquals(0x50094d, inStream.getLong(2, 22, LE, msbFirst))
  }

  @Test def testOneBit1LSBFirst() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes(Seq("01100000")))
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    val bit1 = inStream.getLong(5, 2, LE, lsbFirst)
    assertEquals(3, bit1)
    val bit2 = inStream.getLong(4, 2, LE, lsbFirst)
    assertEquals(2, bit2)
  }

  @Test def testOneBit2LSBFirst() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes(Seq("01010000")))
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    val bit1 = inStream.getLong(5, 2, LE, lsbFirst)
    assertEquals(2, bit1)
    val bit2 = inStream.getLong(4, 2, LE, lsbFirst)
    assertEquals(1, bit2)
  }

  @Test def testOneBit3LSBFirst() {
    val in = Misc.byteArrayToReadableByteChannel(BigInt(0xE4567A).toByteArray)
    val inStream = InStream.fromByteChannel(null, in, 1, -1, msbFirst)
    val bit1 = inStream.getLong(13, 12, LE, lsbFirst)
    assertEquals(0x2B7, bit1)
  }
}
