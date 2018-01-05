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

import junit.framework.Assert._
import org.junit.Test
import java.text.ParsePosition
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import org.junit.Test
import org.apache.daffodil.processors.parsers.TextCalendarConstants

class TestICU {

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
}
