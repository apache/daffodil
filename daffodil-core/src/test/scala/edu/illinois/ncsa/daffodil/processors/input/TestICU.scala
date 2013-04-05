package edu.illinois.ncsa.daffodil.processors.input

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.exceptions.Assert

import java.text.ParsePosition
import java.util.Date
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.{ Calendar, TimeZone, GregorianCalendar, ULocale }

import edu.illinois.ncsa.daffodil.processors.TextCalendarConstants

class TestICU extends JUnitSuite {

  /*
   * This test is to ensure the reliability of ICU's fractional seconds. ICU
   * throws an exception once you get about ~21 fractional seconds, and it
   * gives random results (sometimes garbage, sometimes zero) with anything
   * above 9 fractional seconds. Daffodil will only support 9, so these test
   * ensure that ICU can handle 9 fractional seconds.
   */
  @Test def test_maxFractionalSeconds = {
    def parseFractionalSeconds(numFractionalSeconds: Int) = {
      val numOutFractionalSeconds = TextCalendarConstants.maxFractionalSeconds
      val inDataChar = "1"

      val inPattern = "S" * numFractionalSeconds
      val inData    = inDataChar * numFractionalSeconds

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
}
