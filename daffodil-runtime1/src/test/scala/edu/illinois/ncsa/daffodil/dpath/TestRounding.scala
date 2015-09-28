/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dpath

import junit.framework.Assert.assertEquals
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

class TestRounding {

  @Test def test_howToRoundFloatingPoint = {
    //
    // We ran into an issue where just passing the Float
    // to BigDecimal would change the value passed to it.
    // This would cause an issue with the round-half-to-even
    // function results
    //
    // This is a test to demonstrate that behavior and our
    // solution.
    //
    // We expect that a round-half-to-even function
    // applied to this value would yield 3.46 using a precision of 2.
    //
    val f: Float = 3.455f

    //val bd0 = BigDecimal(f) // 3.4549999237060547 Deprecated
    //val bd1 = BigDecimal.valueOf(f) // 3.4549999237060547 Deprecated
    val bd2 = BigDecimal(f.doubleValue()) // 3.4549999237060547
    val bd3 = BigDecimal(f.toString) // 3.455

    def round(value: BigDecimal, precision: Int): BigDecimal = {
      val rounded = value.setScale(precision, BigDecimal.RoundingMode.HALF_EVEN)
      rounded
    }

    val expected0_to_2 = BigDecimal(3.45) // Incorrect/unexpected result
    val expected3 = BigDecimal(3.46) // Correct result

    //val res0 = round(bd0, 2) Deprecated
    //val res1 = round(bd1, 2) Deprecated
    val res2 = round(bd2, 2)
    val res3 = round(bd3, 2)

    //assertEquals(0, res0.compare(expected0_to_2)) Deprecated
    //assertEquals(0, res1.compare(expected0_to_2)) Deprecated
    assertEquals(0, res2.compare(expected0_to_2))
    assertEquals(0, res3.compare(expected3))

  }

}
