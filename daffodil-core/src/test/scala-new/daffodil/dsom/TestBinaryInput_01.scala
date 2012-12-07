package daffodil.dsom

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.processors._
import daffodil.compiler._
import daffodil.debugger.Debugger

// Do no harm number 16 of 626 fail in regression, 154 in total of 797

class TestBinaryInput_01 extends JUnitSuite {

  var runner = {
    val testDir = "/test-suite/tresys-contributed/"
    val aa = testDir + "BinaryInput_01.tdml"
    lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
    runner
  }

  /*** DFDL-334 ***/
  // Verify Bit Extraction

  @Test def testOneBit1() {
    val in = Compiler.byteArrayToReadableByteChannel(Misc.bits2Bytes("00000011"))
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    val bit1 = inStream.getPartialByte(6, 2, 0)
    assertEquals(3, bit1)
    val bit2 = inStream.getPartialByte(4, 2, 0)
    assertEquals(0, bit2)
  }

  @Test def testOneBit2() {
    val in = Compiler.byteArrayToReadableByteChannel(Misc.bits2Bytes("11000000"))
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    val bit1 = inStream.getPartialByte(0, 2, 0)
    assertEquals(3, bit1)
    val bit2 = inStream.getPartialByte(2, 2, 0)
    assertEquals(0, bit2)
  }

  @Test def testOneBit3() {
    val in = Compiler.byteArrayToReadableByteChannel(Misc.bits2Bytes("00000011"))
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    val (n, len) = inStream.getBitSequence(6, 2, java.nio.ByteOrder.BIG_ENDIAN)
    assertEquals(BigInt(3), n)
    assertEquals(8, len)
    //    val bit2 = inStream.getPartialByte(4, 2, 0)
    //    assertEquals(0, bit2)
  }

  @Test
  def testBufferBitExtraction() {
    var in = Compiler.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getPartialByte(1, 3) == 3)
  }

  @Test
  def testBufferBitExtractionShift() {
    var in = Compiler.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getPartialByte(1, 3, 2) == 12)
  }

  @Test
  def testBufferLeastSignificantBitExtractionShift() {
    var in = Compiler.stringToReadableByteChannel("4")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getPartialByte(5, 3, 2) == 16)
  }

  // Verify aligned byte/short/int/long/bigint extraction
  @Test
  def testBufferByteBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(0, 8, java.nio.ByteOrder.BIG_ENDIAN)._1 == 51)
  }

  @Test
  def testBufferByteLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(0, 8, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 51)
  }

  @Test
  def testBufferShortBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Om")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(0, 16, java.nio.ByteOrder.BIG_ENDIAN)._1 == 20333)
  }

  @Test
  def testBufferShortLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Om")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(0, 16, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 27983)
  }

  @Test
  def testBufferIntBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Help")
    val inStream = InStream.fromByteChannel(null, in, 4, -1)
    assertTrue(inStream.getBitSequence(0, 32, java.nio.ByteOrder.BIG_ENDIAN)._1 == 1214606448)
  }

  @Test
  def testBufferIntLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Help")
    val inStream = InStream.fromByteChannel(null, in, 4, -1)
    assertTrue(inStream.getBitSequence(0, 32, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 1886152008)
  }

  @Test
  def testBufferLongBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Harrison")
    val inStream = InStream.fromByteChannel(null, in, 8, -1)
    assertTrue(inStream.getBitSequence(0, 64, java.nio.ByteOrder.BIG_ENDIAN)._1.toString ==
      "5215575679192756078")
  }

  @Test
  def testBufferLongLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Harrison")
    val inStream = InStream.fromByteChannel(null, in, 8, -1)
    assertTrue(inStream.getBitSequence(0, 64, java.nio.ByteOrder.LITTLE_ENDIAN)._1.toString ==
      "7957705963315814728")
  }

  @Test
  def testBufferBigIntBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Something in the way she moves, ")
    val inStream = InStream.fromByteChannel(null, in, 32, -1)
    assertTrue(inStream.getBitSequence(0, 256, java.nio.ByteOrder.BIG_ENDIAN)._1.toString ==
      "37738841482167102822784581157237036764884875846207476558974346160344516471840")
  }

  @Test
  def testBufferBigIntLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("Something in the way she moves, ")
    val inStream = InStream.fromByteChannel(null, in, 32, -1)
    assertTrue(inStream.getBitSequence(0, 256, java.nio.ByteOrder.LITTLE_ENDIAN)._1.toString ==
      "14552548861771956163454220823873430243364312915206513831353612029437431082835")
  }

  // Aligned but not full string
  @Test
  def testBufferPartialIntBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 24, java.nio.ByteOrder.BIG_ENDIAN)._1 == 5456468)
  }

  @Test
  def testBufferPartialIntLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 24, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 5522003)
  }

  // Non-Aligned 1 Byte or less
  @Test
  def testBufferBitNumberBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(1, 3, java.nio.ByteOrder.BIG_ENDIAN)._1 == 3)
  }

  @Test
  def testBufferBitNumberLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(1, 3, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 3)
  }

  @Test
  def testBufferBitByteBigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("3>")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(2, 8, java.nio.ByteOrder.BIG_ENDIAN)._1 == 204)
  }

  @Test
  def testBufferBitByteLittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("3>")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(2, 8, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 204)
  }

  // Non-Aligned multi-byte
  @Test
  def testBufferPartialInt22At0BigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 22, java.nio.ByteOrder.BIG_ENDIAN)._1 == 1364117)
  }

  @Test
  def testBufferPartialInt22At0LittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 22, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 1393235)
  }

  @Test
  def testBufferPartialInt22At2BigEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(2, 22, java.nio.ByteOrder.BIG_ENDIAN)._1 == 1262164)
  }

  @Test
  def testBufferPartialInt22At2LittleEndianExtraction() {
    var in = Compiler.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(2, 22, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 1313101)
  }

  /*** DFDL-307 ***/
  @Test def test_one_octet() { runner.runOneTest("OneOctetBinaryParse") }
  @Test def test_oneBit2() { runner.runOneTest("OneBit2") }

}
