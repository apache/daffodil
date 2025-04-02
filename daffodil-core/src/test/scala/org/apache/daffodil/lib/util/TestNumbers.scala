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

import org.apache.daffodil.lib.Implicits._

import org.junit.Assert._
import org.junit.Test; object INoWarn { ImplicitsSuppressUnusedImportWarning() }
import java.math.RoundingMode
import java.math.{ BigDecimal => JBigDecimal }

class TestNumbers {
  @Test def testDecimalDivision1(): Unit = {
    val n = new JBigDecimal("90.0")
    assertEquals(1, n.scale())
    val m = new JBigDecimal("1048575.0")
    assertEquals(1, m.scale())
    val quotient = n.divide(m, RoundingMode.HALF_UP)
    val expected = new JBigDecimal("0.0")
    assertEquals(expected, quotient)
  }

  @Test def testDecimalDivision2(): Unit = {
    val n = new JBigDecimal("90.0000000000")
    assertEquals(10, n.scale())
    val m = new JBigDecimal("1048575.0000000000")
    assertEquals(10, m.scale())
    val quotient = n.divide(m, RoundingMode.HALF_UP)
    val expected = new JBigDecimal("0.0000858308")
    assertEquals(expected, quotient)
  }

  @Test def testDoubleToDecimal1(): Unit = {
    val bd = new JBigDecimal(java.lang.Double.MIN_VALUE)
    // val bdString = bd.toPlainString()
    //
    // Notice the vast number of digits below
    //
    val expected = new JBigDecimal(
      "0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004940656458412465441765687928682213723650598026143247644255856825006755072702087518652998363616359923797965646954457177309266567103559397963987747960107818781263007131903114045278458171678489821036887186360569987307230500063874091535649843873124733972731696151400317153853980741262385655911710266585566867681870395603106249319452715914924553293054565444011274801297099995419319894090804165633245247571478690147267801593552386115501348035264934720193790268107107491703332226844753335720832431936092382893458368060106011506169809753078342277318329247904982524730776375927247874656084778203734469699533647017972677717585125660551199131504891101451037862738167250955837389733598993664809941164205702637090279242767544565229087538682506419718265533447265625"
    )
    assertEquals(expected, bd)
  }

  @Test def testDecimalAsDoubleDivision1(): Unit = {
    val n = (new JBigDecimal("90.0")).doubleValue()
    val m = (new JBigDecimal("1048575.0")).doubleValue()
    val quotient = n / m
    val expected1 = 8.583077033116372e-5
    assertEquals(expected1, quotient, 1e-15)
    val decimalQuotient = new JBigDecimal(quotient)
    //
    // The giant number of digits below is in fact what you get as the
    // closest finite base-10 number to the floating point value
    // you see above. If we don't set the scale specifically, then
    // it will use the largest number of possible digits, which could be
    // really large. So even when the exponent is small you can get far more
    // than the max number of digits you see from a double float when printed.
    //
    val expected2 = new JBigDecimal(
      "0.00008583077033116372211996036867276416160166263580322265625"
    )
    assertEquals(expected2, decimalQuotient)

    val double2 = expected2.doubleValue()
    assertEquals(expected1, double2, 1e-15)
  }

  @Test def testDecimalAsDoubleDivision2(): Unit = {
    val n = (new JBigDecimal("90.0000000000")).doubleValue()
    val m = (new JBigDecimal("1048575.0000000000")).doubleValue()
    val quotient = n / m
    val expected1 = 8.583077033116372e-5
    assertEquals(expected1, quotient, 1e-15)
    //
    // Must have a rounding mode here, or we get a ArithmeticException
    // rounding-necessary
    //
    val decimalQuotient = (new JBigDecimal(quotient)).setScale(10, RoundingMode.HALF_EVEN)
    val expected2 = new JBigDecimal("0.0000858308")
    assertEquals(expected2, decimalQuotient)
  }

  @Test def testIsDecimalDouble(): Unit = {
    val foo = new JBigDecimal("0.2")
    val bar = new JBigDecimal("0.200000000000")
    assertTrue(Numbers.isDecimalDouble(foo))
    assertFalse(Numbers.isDecimalDouble(bar))
  }

  @Test def testIsDecimalDouble2(): Unit = {
    assertTrue(Numbers.isDecimalDouble(new JBigDecimal("90.0")))
    assertTrue(Numbers.isDecimalDouble(new JBigDecimal("1048575.0")))
    assertFalse(Numbers.isDecimalDouble(new JBigDecimal("90.0000000000")))
    assertFalse(Numbers.isDecimalDouble(new JBigDecimal("1048575.0000000000")))
  }
}
