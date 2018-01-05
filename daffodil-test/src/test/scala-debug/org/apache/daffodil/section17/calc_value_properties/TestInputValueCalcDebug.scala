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

package org.apache.daffodil.section17.calc_value_properties

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestInputValueCalcDebug {
  val testDir = "/org/apache/daffodil/section17/calc_value_properties/"

  val runner = Runner(testDir, "inputValueCalc.tdml", validateTDMLFile = false)
  val runnerAR = Runner(testDir, "AR.tdml")
  val runnerAQ = Runner(testDir, "AQ.tdml")
  val runnerAA = Runner(testDir, "AA.tdml")

  @AfterClass def shutDown {
    runner.reset
    runnerAR.reset
    runnerAQ.reset
    runnerAA.reset
  }

}

class TestInputValueCalcDebug {

  import TestInputValueCalcDebug._

  //DFDL-1025
  @Test def test_InputValueCalc_refers_self() { runner.runOneTest("InputValueCalc_refers_self") }
  @Test def test_InputValueCalc_circular_ref() { runner.runOneTest("InputValueCalc_circular_ref") }

  //DFDL-1024
  @Test def test_InputValueCalc_optional_elem() { runner.runOneTest("InputValueCalc_optional_elem") }
  @Test def test_InputValueCalc_array_elem() { runner.runOneTest("InputValueCalc_array_elem") }
  @Test def test_InputValueCalc_global_elem() { runner.runOneTest("InputValueCalc_global_elem") }

  @Test def test_AQ001() { runnerAQ.runOneTest("AQ001") } // This appears to expect an error, but doesn't state why.

}
