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

package org.apache.daffodil.runtime1.processors.input

import java.lang.{ Double => JDouble }
import java.lang.{ Long => JLong }
import java.text.ParsePosition

import org.apache.daffodil.lib.calendar.TextCalendarConstants

import com.ibm.icu.math.{ BigDecimal => ICUBigDecimal }
import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.ULocale
import org.junit.Assert._
import org.junit.Test

class TestICU {

  def pp = new ParsePosition(0)

  /*
   * This test is to ensure the reliability of ICU's fractional seconds. ICU
   * throws an exception once you get about ~21 fractional seconds, and it
   * gives random results (sometimes garbage, sometimes zero) with anything
   * above 9 fractional seconds. Daffodil will only support 9, so these test
   * ensure that ICU can handle 9 fractional seconds.
   */
  @Test def test_maxFractionalSeconds() = {
    def parseFractionalSeconds(numFractionalSeconds: Int) = {
      val numOutFractionalSeconds = TextCalendarConstants.maxFractionalSeconds
      val inDataChar = "1"

      val inPattern = "S" * numFractionalSeconds
      val inData = inDataChar * numFractionalSeconds

      val inFormatter = new SimpleDateFormat(inPattern)

      val pos = new ParsePosition(0)
      val cal = inFormatter.getCalendar.clone.asInstanceOf[Calendar]
      cal.clear

      inFormatter.parse(inData, cal, pos)

      val outPattern = "S" * numOutFractionalSeconds
      val outFormatter = new SimpleDateFormat(outPattern)
      val actual = outFormatter.format(cal)

      val numMillis =
        scala.math.min(numFractionalSeconds, 3) // ICU output only has at most 3 sig figs
      val expected = (inDataChar * numMillis) + "0" * (numOutFractionalSeconds - numMillis)

      assertEquals(expected, actual)
    }

    val r = 1 to TextCalendarConstants.maxFractionalSeconds
    r.foreach(parseFractionalSeconds)
  }

  /**
   * This test shows that error index is never set when parsing with a SimpleDateFormat. If ICU
   * ever changes this to use error index, this test should fail.
   *
   * On success, ParsePosition.getIndex is set to where the parse finished. On error,
   * ParsePosition.getIndex is set to zero and ParsePoition.getErrorIndex is -1 (unset)
   */
  @Test def test_simpleDateFormatParse() = {
    def parseWithPattern(pattern: String, data: String): ParsePosition = {
      val df = new SimpleDateFormat(pattern)
      val cal = Calendar.getInstance()
      val pos = pp
      df.parse(data, cal, pos)
      pos
    }

    {
      // success, consumes all data
      val pos = parseWithPattern("HHmm", "1122")
      assertEquals(4, pos.getIndex)
      assertEquals(-1, pos.getErrorIndex)
    }

    {
      // success, parses only first 4 characters
      val pos = parseWithPattern("HHmm", "1122extra")
      assertEquals(4, pos.getIndex)
      assertEquals(-1, pos.getErrorIndex)
    }

    {
      // failure, only partially correct data
      val pos = parseWithPattern("HHmm", "11cd")
      assertEquals(2, pos.getIndex)
      assertEquals(-1, pos.getErrorIndex)
    }

    {
      // failure, empty string
      val pos = parseWithPattern("HHmm", "")
      assertEquals(0, pos.getIndex)
      assertEquals(-1, pos.getErrorIndex)
    }

  }

  // The three following tests show an old ICU bug where if the decimal pattern
  // uses scientific notation and the number to format/unparse is positive
  // infinity, negative infinity, or not a number, ICU would include the
  // exponent separator when it shouldn't. We used to work around this in
  // Daffodil by handling INF, -INF, and NaN ourselves. As of ICU 67.1, this
  // bug appears to be fixed. These tests are kept around to ensure ICU does
  // not have a regression of this issue. The old broken values that ICU would
  // create are commented out to show what used to be broken.

  @Test def test_scientific_pos_inf() = {
    val dfs = new DecimalFormatSymbols(ULocale.US)
    dfs.setInfinity("INF")
    dfs.setNaN("NaN")
    dfs.setExponentSeparator("x10^")
    val df = new DecimalFormat("000.0#E0", dfs)
    val posInf = JDouble.POSITIVE_INFINITY
    val str = df.format(posInf)
    // assertEquals("INFx10^0", str)
    assertEquals("INF", str)
  }

  @Test def test_scientific_neg_inf() = {
    val dfs = new DecimalFormatSymbols(ULocale.US)
    dfs.setInfinity("INF")
    dfs.setNaN("NaN")
    dfs.setExponentSeparator("x10^")
    val df = new DecimalFormat("000.0#E0", dfs)
    val negInf = JDouble.NEGATIVE_INFINITY
    val str = df.format(negInf)
    // assertEquals("-INFx10^0", str)
    assertEquals("-INF", str)
  }

  @Test def test_scientific_nan() = {
    val dfs = new DecimalFormatSymbols(ULocale.US)
    dfs.setInfinity("INF")
    dfs.setNaN("NaN")
    dfs.setExponentSeparator("x10^")
    val df = new DecimalFormat("000.0#E0", dfs)
    val nan = JDouble.NaN
    val str = df.format(nan)
    // assertEquals("NaNx10^0", str)
    assertEquals("NaN", str)
  }

  // The following test shows that ICU does not reqiure a positive pattern.
  // Because of this we manually check that a positive pattern exists.
  @Test def test_missing_positive_pattern() = {
    // this should throw an exception if ICU ever adds back support for
    // requiring a positive pattern.
    val df = new DecimalFormat(";-000")
  }

  // When there is after-suffix padding, ICU has a bug where it does not update
  // the parse position to after the padding but keeps it at the end of the
  // suffix. So Daffodil does this check manually to ensure all characters
  // after parse position are pad characters. If ICU ever fixes this bug and
  // updates parse position, this test should fail, and we no longer need to do
  // the manual check.
  @Test def test_suffix_padding() = {
    val df = new DecimalFormat("0 SUFFIX*_")
    val pos = new ParsePosition(0)
    val str = "123 SUFFIX____"
    val num = df.parse(str, pos)
    assertEquals(123L, num)
    assertEquals(10, pos.getIndex)
    // assertEquals(str.length, pos.getIndex)
  }

  // This test shows the bug in ICU where it does not correctly handle
  // scientific notiation with an empty exponent separator. If ICU fixes this
  // bug, this test should fail. See DAFFODIL-1981
  @Test def test_empty_exponent_separator() = {
    val dfs = new DecimalFormatSymbols(ULocale.US)
    dfs.setExponentSeparator("")
    dfs.setGroupingSeparator(',')
    dfs.setDecimalSeparator('.')
    val df = new DecimalFormat("##.##E+0", dfs)
    val pp = new ParsePosition(0)
    val num = df.parse("12.34+2", pp)
    assertEquals(12.34, num.doubleValue, 1e-15)
    assertEquals(5, pp.getIndex)
    // assertEquals(1234L, num)
    // assertEquals(7, pp.getIndex)
  }

  // Shows that even if a decimal format pattern doesn't contain a decimal
  // point, the decimal format separator from the locale still has an effect
  // and can cause locale specific behavior
  @Test def test_local_side_effect() = {

    // Germany's locale has a decimal separator of ','
    val dfs = new DecimalFormatSymbols(ULocale.GERMANY)

    // Set the grouping separator to be the same as the decimal separator from
    // the locale. ICU never complains
    dfs.setGroupingSeparator(',')

    // Define a pattern that only has a grouping separator--no decimal separator
    val df = new DecimalFormat("###,###", dfs)

    // We don't have a decimal point character in the pattern, so one might
    // expect that only the grouping separator would be used, with a resulting
    // value of 123456. However, the decimal separator from the locale does
    // have an effect with ICU, which causes the result to be 123.456. This
    // shows that it's often important to set both the grouping and decimal
    // separators, even if the pattern only contains one. Otherwise locale
    // information might be used when parsing.
    val pp = new ParsePosition(0)
    val num = df.parse("123,456", pp)
    assertEquals(7, pp.getIndex)
    assertEquals(123.456, num.doubleValue, 1e-15)
  }

  // Shows that even if a DecimalFormat pattern doesn't contain a decimal, and
  // properties are set to disable parsing decimals, and DeciamlFormatSymbols
  // doesn't explicitly set NaN/INF representations, ICU will still parse NaN
  // and Infinity using the locale symbols and return a Double. There is no way
  // to disable this behavior, like would be needed when parsing integer types
  @Test def test_nan_inf() = {

    // defaults to values from the locale. Note that Daffodil does not set
    // inf/nan representations when parsing integers since they should not
    // apply and aren't used according to the DFDL spec
    val dfs = new DecimalFormatSymbols(ULocale.US)

    // Define a pattern that only has a grouping separator--no decimal
    // separator and should only parse integers
    val df = new DecimalFormat("###,###", dfs)
    df.setParseIntegerOnly(true)
    df.setParseBigDecimal(false)

    val ppNaN = new ParsePosition(0)
    val numNaN = df.parse(dfs.getNaN, ppNaN)
    assertTrue(numNaN.isInstanceOf[Double])
    assertEquals(JDouble.NaN, numNaN.doubleValue, 0.0)

    val ppInf = new ParsePosition(0)
    val numInf = df.parse(dfs.getInfinity, ppInf)
    assertTrue(numInf.isInstanceOf[Double])
    assertEquals(JDouble.POSITIVE_INFINITY, numInf.doubleValue, 0.0)

    val ppNInf = new ParsePosition(0)
    val numNInf = df.parse(dfs.getMinusSign +: dfs.getInfinity, ppNInf)
    assertTrue(numNInf.isInstanceOf[Double])
    assertEquals(JDouble.NEGATIVE_INFINITY, numNInf.doubleValue, 0.0)
  }

  // shows that with parseStrict and decimalPatternMatch required, that ICU requires or
  // disallows a decimal point in the data based on whether or not a decimal point appears in
  // the pattern. Also shows ICU failing to parse infinity/nan when decimalPatternMatchRequired
  // is true and the pattern contains a decimal.
  @Test def test_decimalPatternMatchRequired(): Unit = {
    val dfs = new DecimalFormatSymbols(ULocale.US)

    val df = new DecimalFormat("", dfs)
    df.setDecimalPatternMatchRequired(true)
    df.setParseStrict(true)

    df.applyPattern("0.0")

    assertEquals(JLong.valueOf(1), df.parse("1.0", pp))
    assertEquals(null, df.parse("1", pp))
    assertEquals(null, df.parse(dfs.getInfinity, pp)) // see ICU-22303
    assertEquals(null, df.parse(dfs.getNaN, pp)) // see ICU-22303

    assertEquals("1.0", df.format(1L))
    assertEquals(dfs.getInfinity, df.format(JDouble.POSITIVE_INFINITY))
    assertEquals(dfs.getNaN, df.format(JDouble.NaN))

    df.applyPattern("0")

    assertEquals(null, df.parse("1.0", pp))
    assertEquals(JLong.valueOf(1), df.parse("1", pp))
    assertEquals(JDouble.POSITIVE_INFINITY, df.parse(dfs.getInfinity, pp))
    assertEquals(JDouble.NaN, df.parse(dfs.getNaN, pp))

    assertEquals("1", df.format(1L))
    assertEquals(dfs.getInfinity, df.format(JDouble.POSITIVE_INFINITY))
    assertEquals(dfs.getNaN, df.format(JDouble.NaN))

    df.applyPattern("#.#")

    assertEquals(JLong.valueOf(1), df.parse("1.0", pp))
    assertEquals(null, df.parse("1", pp))
    assertEquals(null, df.parse(dfs.getInfinity, pp)) // see ICU-22303
    assertEquals(null, df.parse(dfs.getNaN, pp)) // see ICU-22303

    assertEquals("1", df.format(1L))
    assertEquals(dfs.getInfinity, df.format(JDouble.POSITIVE_INFINITY))
    assertEquals(dfs.getNaN, df.format(JDouble.NaN))

    df.applyPattern("#")

    assertEquals(null, df.parse("1.0", pp))
    assertEquals(JLong.valueOf(1), df.parse("1", pp))
    assertEquals(JDouble.POSITIVE_INFINITY, df.parse(dfs.getInfinity, pp))
    assertEquals(JDouble.NaN, df.parse(dfs.getNaN, pp))

    assertEquals("1", df.format(1L))
    assertEquals(dfs.getInfinity, df.format(JDouble.POSITIVE_INFINITY))
    assertEquals(dfs.getNaN, df.format(JDouble.NaN))
  }

  /**
   * Regardless of the ICU number pattern or text being parsed, ICU always returns the following
   * types
   *
   * - Double if the parsed value is INF, -INF, NaN, or negative zero
   * - Long if the parsed value has no fractional parts and fits in a long
   * - ICUBigDecimal otherwise
   */
  @Test def test_floatingPointReturnType(): Unit = {
    val dfs = new DecimalFormatSymbols(ULocale.US)
    val df = new DecimalFormat("", dfs)
    df.applyPattern("###0.0##;-###0.0##")

    assertTrue(df.parse("1.0", pp).isInstanceOf[Long])
    assertTrue(df.parse("-1.0", pp).isInstanceOf[Long])
    assertTrue(df.parse("0.0", pp).isInstanceOf[Long])
    assertTrue(df.parse("9223372036854775807", pp).isInstanceOf[Long])

    assertTrue(df.parse(dfs.getNaN, pp).isInstanceOf[Double])
    assertTrue(df.parse(dfs.getInfinity, pp).isInstanceOf[Double])
    assertTrue(df.parse("-" + dfs.getInfinity, pp).isInstanceOf[Double])
    assertTrue(df.parse("-0.0", pp).isInstanceOf[Double])

    assertTrue(df.parse("9223372036854775808", pp).isInstanceOf[ICUBigDecimal])
    assertTrue(df.parse("122.75", pp).isInstanceOf[ICUBigDecimal])
    assertTrue(df.parse("0.3", pp).isInstanceOf[ICUBigDecimal])
  }

}
