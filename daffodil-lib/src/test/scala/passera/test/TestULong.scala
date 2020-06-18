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

  @Test def testULongToString1: Unit = {
    val mm1 = ULong(-1L)
    assertEquals("FFFFFFFFFFFFFFFF", mm1.toHexString.toUpperCase)
    assertEquals(ULong.MaxValueAsBigInt, mm1.toBigInt)
    assertEquals(JBigInt.valueOf(Long.MinValue).abs.toString, mm1.toString)
  }

  // DAFFODIL-1714
  @Test def testULongModulus1: Unit = {
    for (i <- 0 to 16 ) {
      val numerator = ULong(i)
      val denominator = ULong(8)
      val remainder = numerator % denominator
      assertEquals(ULong(i%8), remainder)
    }
  }

  @Test def testULongModulus2: Unit = {
    val mm1 = ULong(-1L)
    val remainder = mm1 % ULong(65536)
    assertEquals(ULong(0x0000FFFF), remainder)
  }
  
  @Test def testULongModulus3: Unit = {
    val mm1 = ULong(-1L)
    val mm2 = ULong(-2L)
    val remainder = mm1 % mm2
    assertEquals(ULong(1), remainder)
  }
  
}
