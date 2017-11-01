/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.section05.simple_types

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestSimpleTypesNew {
  private val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "SimpleTypes.tdml")

  @AfterClass def shutdown {
    runner.reset
  }
}

class TestSimpleTypesNew {
  import TestSimpleTypesNew._

  @Test def test_time_calendarTimeZone_EmptyString() { runner.runOneTest("time_calendarTimeZone_EmptyString") }
  @Test def test_time_calendarTimeZone_EST() { runner.runOneTest("time_calendarTimeZone_EST") }
  @Test def test_date_calendarTimeZone_EmptyString() { runner.runOneTest("date_calendarTimeZone_EmptyString") }
  @Test def test_date_calendarTimeZone_EST() { runner.runOneTest("date_calendarTimeZone_EST") }
  @Test def test_dateTime_calendarTimeZone_EmptyString() { runner.runOneTest("dateTime_calendarTimeZone_EmptyString") }
  @Test def test_dateTime_calendarTimeZone_EST() { runner.runOneTest("dateTime_calendarTimeZone_EST") }

  @Test def test_hexBinary_specifiedLengthUnaligned() { runner.runOneTest("hexBinary_specifiedLengthUnaligned") }
}
