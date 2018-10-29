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

import junit.framework.Assert._
import org.junit.Test
import org.apache.daffodil.Implicits._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSimpleTypesDebug {

  val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "SimpleTypes.tdml")
  val runnerAL = Runner(testDir, "AL.tdml")
  val runner2 = Runner(testDir, "WhiteSpace.tdml")
  val runnerAJ = Runner(testDir, "AJ.tdml")
  val runnerAK = Runner(testDir, "AK.tdml")
  val runner1 = Runner(testDir, "BitOrder.tdml")
  val runnerST = Runner(testDir, "simple-type-bases.tdml")
  val runner3 = Runner(testDir, "BitOrderInvalid.tdml")

  @AfterClass def shutDown() {
    runner.reset
    runnerAL.reset
    runner2.reset
    runnerAJ.reset
    runnerAK.reset
    runner1.reset
    runnerST.reset
  }
}

class TestSimpleTypesDebug {

  import TestSimpleTypesDebug._

  @Test def test_warning_exercise() {
    val exc = intercept[Exception] {
      runner.runOneTest("warning_exercise")
    }
    assertTrue(exc.getMessage().contains("Did not find"))
  }

  //////////////////////// DFDL-529 /////////////////////////////

  @Test def test_dateCalendarCenturyStart() { runner.runOneTest("dateCalendarCenturyStart") }
  @Test def test_dateCalendarCenturyStart2() { runner.runOneTest("dateCalendarCenturyStart2") }

  //////////////////////// DFDL-845 ////////////////////////////

  @Test def test_whiteSpaceDuringValidShort() { runner.runOneTest("whiteSpaceDuringValidShort") }
  @Test def test_whiteSpaceDuringValidInteger() { runner.runOneTest("whiteSpaceDuringValidInteger") }
  @Test def test_whiteSpaceDuringValidUnsignedLong() { runner.runOneTest("whiteSpaceDuringValidUnsignedLong") }
  @Test def test_whiteSpaceDuringValidUnsignedShort() { runner.runOneTest("whiteSpaceDuringValidUnsignedShort") }
  @Test def test_whiteSpaceDuringValidUnsignedByte() { runner.runOneTest("whiteSpaceDuringValidUnsignedByte") }
  @Test def test_whiteSpaceDuringValidUnsignedInt() { runner.runOneTest("whiteSpaceDuringValidUnsignedInt") }
  @Test def test_whiteSpaceDuringValidInt() { runner.runOneTest("whiteSpaceDuringValidInt") }
  @Test def test_whiteSpaceDuringValidLong() { runner.runOneTest("whiteSpaceDuringValidLong") }
  @Test def test_whiteSpaceDuringValidByte() { runner.runOneTest("whiteSpaceDuringValidByte") }

  /////////////////////// DFDL-1042 /////////////////////////////////////////////

  @Test def test_dateStrictCheckPolicy01() { runner.runOneTest("dateStrictCheckPolicy01") }
  @Test def test_timeStrictCheckPolicy01() { runner.runOneTest("timeStrictCheckPolicy01") }
  @Test def test_timeStrictCheckPolicy02() { runner.runOneTest("timeStrictCheckPolicy02") }
  @Test def test_timeFormatting5() { runner.runOneTest("timeFormatting5") }

  ///////////////////////////////////////////////////////////////////////////////////

  @Test def test_posinteger_binary_01() { runner.runOneTest("nonNegInt_binary_01") }

  @Test def test_whiteSpaceAfterLax() { runner2.runOneTest("whiteSpaceAfterLax") }
  @Test def test_redefinedFormat() { runner2.runOneTest("redefinedFormat") }

  @Test def test_bitOrderChange() { runner1.runOneTest("bitOrderChange") }
  @Test def test_bitOrderDocument() { runner1.runOneTest("bitOrderDocument") }
  @Test def test_bitOrderTypeByte() { runner1.runOneTest("bitOrderTypeByte") }
  @Test def test_bitOrderChangeInvalid3() { runner1.runOneTest("bitOrderChangeInvalid3") }

  @Test def test_bitOrderChangeInvalid() { runner3.runOneTest("bitOrderChangeInvalid") }

  //////////////////////// DAFFODIL-1945 /////////////////////////////
  @Test def test_dateCalendarDaysInFirstWeek3() { runner.runOneTest("dateCalendarDaysInFirstWeek3") }
  @Test def test_dateCalendarDaysInFirstWeek5() { runner.runOneTest("dateCalendarDaysInFirstWeek5") }

  @Test def test_dateCalendarFirstDayOfWeek03() { runner.runOneTest("dateCalendarFirstDayOfWeek03") }
  @Test def test_dateCalendarFirstDayOfWeek04() { runner.runOneTest("dateCalendarFirstDayOfWeek04") }

  // DAFFODIL-1946
  @Test def test_dateTimeImplicitPatternFail5() { runner.runOneTest("dateTimeImplicitPatternFail5") }
}
