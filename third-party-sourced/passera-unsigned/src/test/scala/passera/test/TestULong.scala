package passera.test

import org.junit.Test
import org.junit.Assert._
import passera.unsigned.ULong

class TestULong {

  @Test def testULongToString1 {
    val mm1 = ULong(-1L)
    assertEquals("FFFFFFFFFFFFFFFF", mm1.toHexString.toUpperCase)
    assertEquals(ULong.MaxValueAsBigInt, mm1.toBigInt)
    assertEquals(BigInt(Long.MinValue).abs.toString, mm1.toString)
  }
}