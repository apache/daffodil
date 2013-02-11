package edu.illinois.ncsa.daffodil.util

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import com.ibm.icu.text.DecimalFormat
import java.text.ParsePosition
import org.junit.Test

/**
 * Tests that characterize ICU number parsing specifically with respect
 * to dealing with numbers big enough for unsignedLong.
 */
class TestNumberStuff extends JUnitSuite {

  @Test
  def test1() {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "18446744073709551615"
    val bi = p.parse(maxUL)
    assertEquals(maxUL, bi.toString)
  }

  @Test
  def test2() {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "-18446744073709551615"
    val exc = intercept[Exception] {
      val bi = p.parse(maxUL)
    }
    assertTrue(exc.getMessage().contains("negative"))
  }

  @Test
  def test3() {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val tooBig = "18446744073709551616" // one larger than maxUL
    val exc = intercept[Exception] {
      val bi = p.parse(tooBig)
    }
    val msg = exc.getMessage()
    // println(msg)
    assertTrue(msg.contains("big"))
  }

  @Test
  def test4() {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val v = "1"
    val bi = p.parse(v)
    assertEquals(v, bi.toString)
  }

  @Test
  def test5() {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "-1"
    val exc = intercept[Exception] {
      val bi = p.parse(maxUL)
    }
    assertTrue(exc.getMessage().contains("negative"))
  }

  @Test
  def test6() {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val maxUL = "18446744073709551615"
    val v = "18,446,744,073,709,551,615.0000"
    val bi = p.parse(v)
    assertEquals(maxUL, bi.toString)
  }

  @Test
  def test7() {
    val dl = UnsignedLongConverter()
    val p = dl.parser
    val v = "definitelyNotANumber"
    val exc = intercept[Exception] {
      val bi = p.parse(v)
    }
    assertTrue(exc.getMessage().contains("not a valid"))
  }

  @Test def test8() {
    val dl = LongConverter()
    val p = dl.parser
    val v = "definitelyNotANumber"
    val exc = intercept[Exception] {
      val bi = p.parse(v)
    }
    assertTrue(exc.getMessage().contains("not a valid"))
  }

  @Test def test9() {
    val dl = LongConverter()
    val p = dl.parser
    val v = "1              " // lots of spaces after
    val exc = intercept[Exception] {
      val bi = p.parse(v)
    }
    assertTrue(exc.getMessage().contains("consume all"))
  }

  @Test def testHex2Bits() {
    val actual = Misc.hex2Bits("ab3")
    assertEquals("101010110011", actual)
  }

  @Test def testBytesToBits() {
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

  def parser = new NumVerifier[S] {
    def parse(str: String): S = {
      val df = new DecimalFormat()
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

case class UnsignedLongConverter() extends ConvertTo[BigInt] {

  val maxUnsignedLong = new BigInt(new java.math.BigInteger("18446744073709551615"))

  def getNum(j: Number) = j match {
    case l: java.lang.Long => {
      if (l >= 0L) new BigInt(java.math.BigInteger.valueOf(l))
      else throw new Exception("parsed as negative value")
    }
    case bi: java.math.BigInteger => {
      val ul = new BigInt(bi)
      if (ul > maxUnsignedLong) throw new Exception("too big for unsignedLong")
      if (ul < 0) throw new Exception("parsed as negative value")
      ul
    }
    case _ => throw new Exception("not a valid unsignedLong")
  }
}
