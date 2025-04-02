/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.util

import java.lang.{ Long => JLong, Number => JNumber }
import java.math.{ BigInteger => JBigInt }
import java.text.ParsePosition

import org.apache.daffodil.lib.Implicits.intercept

import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Tests that characterize ICU number parsing specifically with respect
 * to dealing with numbers big enough for unsignedLong.
 */
class TestNumberStuff {

  @Test
  def test1(): Unit = {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "18446744073709551615"
    val bi = p.parse(maxUL)
    assertEquals(maxUL, bi.toString)
  }

  @Test
  def test2(): Unit = {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "-18446744073709551615"
    val exc = intercept[Exception] {
      p.parse(maxUL)
    }
    assertTrue(exc.getMessage().contains("negative"))
  }

  @Test
  def test3(): Unit = {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val tooBig = "18446744073709551616" // one larger than maxUL
    val exc = intercept[Exception] {
      p.parse(tooBig)
    }
    val msg = exc.getMessage()
    // println(msg)
    assertTrue(msg.contains("big"))
  }

  @Test
  def test4(): Unit = {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val v = "1"
    val bi = p.parse(v)
    assertEquals(v, bi.toString)
  }

  @Test
  def test5(): Unit = {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "-1"
    val exc = intercept[Exception] {
      p.parse(maxUL)
    }
    assertTrue(exc.getMessage().contains("negative"))
  }

  @Test
  def test6(): Unit = {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "18446744073709551615"
    val v = "18,446,744,073,709,551,615.0000"
    val bi = p.parse(v)
    assertEquals(maxUL, bi.toString)
  }

  @Test
  def test7(): Unit = {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val v = "definitelyNotANumber"
    val exc = intercept[Exception] {
      p.parse(v)
    }
    assertTrue(exc.getMessage().contains("not a valid"))
  }

  @Test def test8(): Unit = {
    val dl = LongConverter()
    val p = dl.parser
    val v = "definitelyNotANumber"
    val exc = intercept[Exception] {
      p.parse(v)
    }
    assertTrue(exc.getMessage().contains("not a valid"))
  }

  @Test def test9(): Unit = {
    val dl = LongConverter()
    val p = dl.parser
    val v = "1              " // lots of spaces after
    val exc = intercept[Exception] {
      p.parse(v)
    }
    assertTrue(exc.getMessage().contains("consume all"))
  }

  @Test def testHex2Bits(): Unit = {
    val actual = Misc.hex2Bits("ab3")
    assertEquals("101010110011", actual)
  }

  @Test def testBytesToBits(): Unit = {
    val xml = <foo>&#xA2;</foo>
    val bytes = xml.child(0).text.getBytes("utf-8")
    val actual = Misc.bytes2Bits(bytes)
    val expected = Misc.hex2Bits("C2A2")
    assertEquals(expected, actual)
  }

}

abstract class NumVerifier[T] {
  def parse(s: String): T
}

abstract class ConvertTo[S] {
  def getNum(javaNumber: Number): S

  lazy val parser = new NumVerifier[S] {
    def parse(str: String): S = {
      val dfs = new DecimalFormatSymbols()
      dfs.setDecimalSeparator('.')
      dfs.setGroupingSeparator(',')
      val df = new DecimalFormat()
      df.setDecimalFormatSymbols(dfs)
      val pos = new ParsePosition(0)
      val num = df.parse(str, pos)
      if (num == null) throw new Exception("not a valid representation")
      if (pos.getIndex != str.length) throw new Exception("didn't consume all the text")

      // if nothing threw an exception, then we have a number.
      // convert it.
      getNum(num)
    }
  }

}

case class IntConverter() extends ConvertTo[Int] {
  def getNum(j: Number) = j.intValue
}

case class LongConverter() extends ConvertTo[Long] {
  def getNum(j: Number) = j.longValue
}

case class UnsignedLongConverter() extends ConvertTo[JBigInt] {

  val maxUnsignedLong = new JBigInt("18446744073709551615")

  def getNum(j: JNumber) = j match {
    case l: JLong => {
      if (l >= 0L) JBigInt.valueOf(l)
      else throw new Exception("parsed as negative value")
    }
    case icubd: com.ibm.icu.math.BigDecimal => {
      val ul = icubd.toBigIntegerExact
      if (ul.compareTo(maxUnsignedLong) > 0) throw new Exception("too big for unsignedLong")
      if (ul.signum() < 0) throw new Exception("parsed as negative value")
      ul
    }
    case other =>
      throw new Exception("not a valid unsignedLong: " + other + " : " + other.getClass.getName)
  }
}
