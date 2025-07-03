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

package org.apache.daffodil.lib.functionality.icu

import java.text.ParsePosition

import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import org.junit.Assert._
import org.junit.Test

class TestBigInteger {

  /**
   * There's an issue within Daffodil where we run test_BigInteger1 and it 
   * fails when using the pattern listed below.  However a separate unit 
   * test (the one below) using the same pattern and settings as the Daffodil
   * test, passes.
   * 
   * The issue appears to be that somehow on or about line 1840 of ICU's 
   * DecimalFormat.parse(String, ParsePosition, Array[Currency]) that the 
   * digitList goes from being "0.6864797660130609714981900799081393217269
   * 435300143305409394463459185543183397656052122559640661454554977296311
   * 39148085803712198799971664381257402829111505715x10^156" to
   * "0.686479766013060971498190079908139321726943530014330540939446345918
   * 554318339765605212255964066145455497729x10^105"
   * 
   * As you can see, the digit length is reduced from 156 to 105.  If you
   * count the number of leading zeroes in the 'str' input, you will notice
   * that it accounts for this difference of 51 characters as there are 51
   * leading zeroes.  So it would seem as though these are related.
   * 
   * We've tried running the test below within the 'context' of Daffodil by
   * placing it in the runtime module and calling it from the place of error:
   * PrimitivesTextNumber1.scala's CovnertTextNumberParser.parse method, the
   * line containing df.get.parse(str,pos).
   */
  @Test def testBigInteger(): Unit = {
    val pattern =
      "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,000"

    val dfs = new DecimalFormatSymbols()
    dfs.setDecimalSeparator('.')
    dfs.setGroupingSeparator(',')
    dfs.setExponentSeparator("E")

    val df = new DecimalFormat(pattern, dfs)
    df.setParseStrict(false)
    df.setRoundingMode(java.math.RoundingMode.UNNECESSARY.ordinal())
    df.setRoundingIncrement(0)
    df.setMaximumFractionDigits(0)
    df.setDecimalSeparatorAlwaysShown(false)
    df.setParseIntegerOnly(true)

    val str =
      "000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,686,479,766,013,060,971,498,190,079,908,139,321,726,943,530,014,330,540,939,446,345,918,554,318,339,765,605,212,255,964,066,145,455,497,729,631,139,148,085,803,712,198,799,971,664,381,257,402,829,111,505,715"
    val pos = new ParsePosition(0)
    val res = df.parse(str, pos)

    // Console.out.println(res)

    assertEquals(
      "686479766013060971498190079908139321726943530014330540939446345918554318339765605212255964066145455497729631139148085803712198799971664381257402829111505715",
      res.toString()
    )

  }

}
