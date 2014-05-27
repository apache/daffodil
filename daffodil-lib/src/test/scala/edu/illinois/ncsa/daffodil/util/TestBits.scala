package edu.illinois.ncsa.daffodil.util

import org.junit.Test
import junit.framework.Assert._

class TestBits {

  @Test def testShiftLeftByteArray1() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftLeft(ba, 3)
    assertEquals(List(0xE0.toByte), ba.toList)
  }

  @Test def testShiftLeftByteArray2() {
    val ba = List(0x7C.toByte, 0x3D.toByte).toArray
    Bits.shiftLeft(ba, 3)
    assertEquals(List(0xE1.toByte, 0xE8.toByte), ba.toList)
  }

  @Test def testShiftLeftByteArray3() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftLeft(ba, 0)
    assertEquals(List(0x7c.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray1() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftRight(ba, 3)
    assertEquals(List(0x0F.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray2() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftRight(ba, 0)
    assertEquals(List(0x7C.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray3() {
    val ba = List(0x7C.toByte, 0x3D.toByte).toArray
    Bits.shiftRight(ba, 3)
    assertEquals(List(0x0F.toByte, 0x87.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray4() {
    val ba = List(0x7C.toByte, 0x3D.toByte, 0x42.toByte).toArray
    Bits.shiftRight(ba, 3)
    assertEquals(List(0x0F.toByte, 0x87.toByte, 0xA8.toByte), ba.toList)
  }
}