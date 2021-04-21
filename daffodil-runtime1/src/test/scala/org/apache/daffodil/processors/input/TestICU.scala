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

package org.apache.daffodil.processors.input

import org.junit.Assert._
import java.text.ParsePosition

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import org.junit.Test
import org.apache.daffodil.calendar.TextCalendarConstants

class TestICU {

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

      val numMillis = scala.math.min(numFractionalSeconds, 3) // ICU output only has at most 3 sig figs
      val expected = (inDataChar * numMillis) + "0" * (numOutFractionalSeconds - numMillis)

      assertEquals(expected, actual)
    }

    val r = 1 to TextCalendarConstants.maxFractionalSeconds
    r.foreach(parseFractionalSeconds)
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
    val dfs = new DecimalFormatSymbols()
    dfs.setInfinity("INF")
    dfs.setNaN("NaN")
    dfs.setExponentSeparator("x10^")
    val df = new DecimalFormat("000.0#E0", dfs)
    val posInf = java.lang.Double.POSITIVE_INFINITY
    val str = df.format(posInf)
    //assertEquals("INFx10^0", str)
    assertEquals("INF", str)
  }

  @Test def test_scientific_neg_inf() = {
    val dfs = new DecimalFormatSymbols()
    dfs.setInfinity("INF")
    dfs.setNaN("NaN")
    dfs.setExponentSeparator("x10^")
    val df = new DecimalFormat("000.0#E0", dfs)
    val negInf = java.lang.Double.NEGATIVE_INFINITY
    val str = df.format(negInf)
    //assertEquals("-INFx10^0", str)
    assertEquals("-INF", str)
  }

  @Test def test_scientific_nan() = {
    val dfs = new DecimalFormatSymbols()
    dfs.setInfinity("INF")
    dfs.setNaN("NaN")
    dfs.setExponentSeparator("x10^")
    val df = new DecimalFormat("000.0#E0", dfs)
    val nan = java.lang.Double.NaN
    val str = df.format(nan)
    //assertEquals("NaNx10^0", str)
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
    //assertEquals(str.length, pos.getIndex)
  }

  // This test shows the bug in ICU where it does not correctly handle
  // scientific notiation with an empty exponent separator. If ICU fixes this
  // bug, this test should fail. See DAFFODIL-1981
  @Test def test_empty_exponent_separator() = {
    val dfs = new DecimalFormatSymbols()
    dfs.setExponentSeparator("")
    dfs.setGroupingSeparator(',')
    dfs.setDecimalSeparator('.')
    val df = new DecimalFormat("##.##E+0", dfs)
    val pp = new ParsePosition(0)
    val num = df.parse("12.34+2", pp)
    assertEquals(12.34, num.doubleValue, 1e-15)
    assertEquals(5, pp.getIndex)
    //assertEquals(1234L, num)
    //assertEquals(7, pp.getIndex)
  }

  // Shows that even if a decimal format pattern doesn't contain a decimal
  // point, the decimal format separator from the locale still has an effect
  // and can cause locale specific behavior
  @Test def test_local_side_effect() = {

    // Germany's locale has a decimal separator of ','
    val dfs = new DecimalFormatSymbols(java.util.Locale.GERMANY)

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
}
