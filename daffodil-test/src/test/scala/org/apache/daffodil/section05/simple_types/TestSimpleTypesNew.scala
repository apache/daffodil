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

package org.apache.daffodil.section05.simple_types

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestSimpleTypesNew {
  private val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "SimpleTypes.tdml")
  val runner1 = Runner(testDir, "BitOrder.tdml")
  val runner2 = Runner(testDir, "SimpleTypes2.tdml")
  @AfterClass def shutdown {
    runner.reset
    runner1.reset
    runner2.reset
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

  // DAFFODIL-1001 fixed.
  @Test def test_bigEndianLeastFirst() { runner1.runOneTest("bigEndianLeastFirst") }

  // DAFFODIL-2204
  @Test def test_terminatorErrorMessage() { runner2.runOneTest("terminatorErrorMessage") }
}
