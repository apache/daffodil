/*
 * Copyright (c) 2011-2013, Nate Nystrom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or
 * other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package passera.test

import java.math.{ BigInteger => JBigInt }

import org.junit.Assert.assertEquals
import org.junit.Test
import passera.unsigned.ULong

class TestULong {

  @Test def testULongToString1(): Unit = {
    val mm1 = ULong(-1L)
    assertEquals("FFFFFFFFFFFFFFFF", mm1.toHexString.toUpperCase)
    assertEquals(ULong.MaxValueAsBigInt, mm1.toBigInt)
    assertEquals("18446744073709551615", mm1.toString)
    assertEquals("FFFFFFFFFFFFFFFF", "%x".format(mm1.toBigInt).toUpperCase)
  }

  // DAFFODIL-1714
  @Test def testULongModulus1(): Unit = {
    for (i <- 0 to 16) {
      val numerator = ULong(i)
      val denominator = ULong(8)
      val remainder = numerator % denominator
      assertEquals(ULong(i % 8), remainder)
    }
  }

  @Test def testULongModulus2(): Unit = {
    val mm1 = ULong(-1L)
    val remainder = mm1 % ULong(65536)
    assertEquals(ULong(0x0000ffff), remainder)
  }

  @Test def testULongModulus3(): Unit = {
    val mm1 = ULong(-1L)
    val mm2 = ULong(-2L)
    val remainder = mm1 % mm2
    assertEquals(ULong(1), remainder)
  }

  @Test def testULongMostNegativeLong1(): Unit = {
    val v = ULong(Long.MinValue)
    val vbi = v.toBigInt
    val vhex = vbi.toString(16)
    assertEquals("8000000000000000", vhex)
    assertEquals("8000000000000000", v.toHexString)
    assertEquals(-9223372036854775808L, v.longValue)
    assertEquals("9223372036854775808", v.toString)
  }

  @Test def testULongMaxValue(): Unit = {
    val v = ULong.MaxValue
    assertEquals("FFFFFFFFFFFFFFFF", v.toHexString.toUpperCase)
    assertEquals(ULong(0), v + ULong(1))
    val v1 = ULong.MaxValue.toBigInt.add(JBigInt.valueOf(2)) // 0x8000000000000001
    assertEquals(ULong(1), ULong(v1)) // preserves only 64 bits
  }

  @Test def testULongFromBigInt(): Unit = {
    val zero = JBigInt.ZERO
    val one = JBigInt.ONE
    val two = JBigInt.valueOf(2)
    val minusOne = JBigInt.valueOf(-1)
    val minusTwo = JBigInt.valueOf(-2)
    assertEquals(0, ULong(zero).toInt)
    assertEquals(1, ULong(one).toInt)
    assertEquals(2, ULong(two).toInt)
    assertEquals(-1, ULong(minusOne).toInt)
    assertEquals(-2, ULong(minusTwo).toInt)
    assertEquals("7FFFFFFFFFFFFFFF", (ULong(Long.MinValue) - ULong(1)).toHexString.toUpperCase)
  }

  @Test def testULongShift(): Unit = {
    // The >> operator is arithmetic shift, so for signed numbers this would sign extend,
    // but ULong is unsigned, so there is no difference between >> and >>> (logical shift right).
    assertEquals("7FFFFFFFFFFFFFFF", (ULong.MaxValue >> 1).toHexString.toUpperCase)
    assertEquals("7FFFFFFFFFFFFFFF", (ULong.MaxValue >>> 1).toHexString.toUpperCase)
    assertEquals(1, (ULong.MaxValue >> 63).toInt)
    //
    // Consistent with the usual scala >> operator, shift by the width is a no-op.
    assertEquals(ULong.MaxValue, ULong.MaxValue >> 64)
    //
    // Check enforcement of the width when shifting left. We lose the MSB here.
    assertEquals("7FFFFFFFFFFFFFFF", (ULong.MaxValue << 1 >> 1).toHexString.toUpperCase)
  }

  @Test def testULongFromHex(): Unit = {
    assertEquals(
      "7FFFFFFFFFFFFFFF",
      ULong.fromHexString("7FFFFFFFFFFFFFFF").toHexString.toUpperCase
    )
  }
}
