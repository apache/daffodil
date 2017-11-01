/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.functionality.icu

import org.junit.Test
import junit.framework.Assert._
import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import java.text.ParsePosition

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
  @Test def testBigInteger() {
    val pattern = "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,000"
    
    val dfs = new DecimalFormatSymbols()
    dfs.setGroupingSeparator(',')
    dfs.setExponentSeparator("E")
    
    val df = new DecimalFormat(pattern, dfs)
    df.setParseStrict(false)
    df.setRoundingMode(java.math.BigDecimal.ROUND_UNNECESSARY)
    df.setRoundingIncrement(0)
    df.setMaximumFractionDigits(0)
    df.setDecimalSeparatorAlwaysShown(false)
    df.setParseIntegerOnly(true)

    val str = "000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,686,479,766,013,060,971,498,190,079,908,139,321,726,943,530,014,330,540,939,446,345,918,554,318,339,765,605,212,255,964,066,145,455,497,729,631,139,148,085,803,712,198,799,971,664,381,257,402,829,111,505,715"
    val pos = new ParsePosition(0)
    val res = df.parse(str, pos)

    //Console.out.println(res)
    
    assertEquals("686479766013060971498190079908139321726943530014330540939446345918554318339765605212255964066145455497729631139148085803712198799971664381257402829111505715", res.toString())

  }

}
