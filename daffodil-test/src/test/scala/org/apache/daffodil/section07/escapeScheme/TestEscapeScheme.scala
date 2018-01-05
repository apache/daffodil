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

package org.apache.daffodil.section07.escapeScheme

import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestEscapeScheme {
  val testDir = "/org/apache/daffodil/section07/escapeScheme/"
  val runner = Runner(testDir, "escapeScheme.tdml", validateTDMLFile = false)
  val runnerNeg = Runner(testDir, "escapeSchemeNeg.tdml", validateTDMLFile = false)
  val runner2 = Runner(testDir, "escapeScenarios.tdml", validateTDMLFile = false)

  @AfterClass def shutDown() {
    runner.reset
    runnerNeg.reset
    runner2.reset
  }
}

class TestEscapeScheme {
  import TestEscapeScheme._
  // Debug Template
  // @Test def test_name() = Debugger.withDebugger {
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  // runner.runOneTest("test_name")
  // }

  @Test def test_escapeSchemeSimple() { runner.runOneTest("escapeSchemeSimple") }
  @Test def test_escapeSchemeEmpty() { runner.runOneTest("escapeSchemeEmpty") }
  @Test def test_escapeSchemeUnused() { runner.runOneTest("escapeSchemeUnused") }
  @Test def test_escapeSchemeFail() { runner.runOneTest("escapeSchemeFail") }
  @Test def test_escapeSchemeFail2() { runner.runOneTest("escapeSchemeFail2") }
  @Test def test_escapeSchemeFail3() { runner.runOneTest("escapeSchemeFail3") }
  @Test def test_escapeSchemeNonEmpty() { runner.runOneTest("escapeSchemeNonEmpty") }

  @Test def test_escapeExpressions_01() { runner.runOneTest("escapeExpressions_01") }
  @Test def test_escapeExpressions_01b() { runner.runOneTest("escapeExpressions_01b") }
  @Test def test_escapeExpressions_02() { runner.runOneTest("escapeExpressions_02") }
  @Test def test_escapeExpressions_03() { runner.runOneTest("escapeExpressions_03") }
  @Test def test_escapeExpressions_04() { runner.runOneTest("escapeExpressions_04") }
  @Test def test_escapeExpressions_05() { runner.runOneTest("escapeExpressions_05") }
  @Test def test_escapeExpressions_06() { runner.runOneTest("escapeExpressions_06") }

  @Test def test_escapeSchemeNeg() { runnerNeg.runOneTest("escapeSchemeNeg") }

  @Test def test_scenario1_1() { runner2.runOneTest("scenario1_1") }
  @Test def test_scenario1_2() { runner2.runOneTest("scenario1_2") }
  @Test def test_scenario1_3() { runner2.runOneTest("scenario1_3") }
  @Test def test_scenario1_4() { runner2.runOneTest("scenario1_4") }
  @Test def test_scenario1_5() { runner2.runOneTest("scenario1_5") }
  @Test def test_scenario1_6() { runner2.runOneTest("scenario1_6") }
  @Test def test_scenario1_7() { runner2.runOneTest("scenario1_7") }
  @Test def test_scenario1_7_postfix() { runner2.runOneTest("scenario1_7_postfix") }
  @Test def test_scenario1_8() { runner2.runOneTest("scenario1_8") }
  @Test def test_scenario1_8_req_term() { runner2.runOneTest("scenario1_8_req_term") }
  @Test def test_scenario1_9() { runner2.runOneTest("scenario1_9") }
  @Test def test_scenario1_9_postfix() { runner2.runOneTest("scenario1_9_postfix") }
  @Test def test_scenario1_10() { runner2.runOneTest("scenario1_10") }
  @Test def test_scenario1_10_postfix() { runner2.runOneTest("scenario1_10_postfix") }
  @Test def test_scenario1_11() { runner2.runOneTest("scenario1_11") }
  @Test def test_scenario1_11_postfix() { runner2.runOneTest("scenario1_11_postfix") }
  @Test def test_scenario1_12() { runner2.runOneTest("scenario1_12") }
  @Test def test_scenario1_12_postfix() { runner2.runOneTest("scenario1_12_postfix") }
  @Test def test_scenario1_13() { runner2.runOneTest("scenario1_13") }
  @Test def test_scenario1_13_postfix() { runner2.runOneTest("scenario1_13_postfix") }

  @Test def test_scenario2_1() { runner2.runOneTest("scenario2_1") }
  @Test def test_scenario2_2() { runner2.runOneTest("scenario2_2") }
  @Test def test_scenario2_3() { runner2.runOneTest("scenario2_3") }
  @Test def test_scenario2_4() { runner2.runOneTest("scenario2_4") }
  @Test def test_scenario2_5() { runner2.runOneTest("scenario2_5") }
  @Test def test_scenario2_6() { runner2.runOneTest("scenario2_6") }
  @Test def test_scenario2_7() { runner2.runOneTest("scenario2_7") }
  @Test def test_scenario2_8() { runner2.runOneTest("scenario2_8") }
  @Test def test_scenario2_9() { runner2.runOneTest("scenario2_9") }
  @Test def test_scenario2_10() { runner2.runOneTest("scenario2_10") }
  @Test def test_scenario2_10_postfix() { runner2.runOneTest("scenario2_10_postfix") }
  @Test def test_scenario2_11() { runner2.runOneTest("scenario2_11") }
  @Test def test_scenario2_11_req_term() { runner2.runOneTest("scenario2_11_req_term") }
  @Test def test_scenario2_12() { runner2.runOneTest("scenario2_12") }
  @Test def test_scenario2_12_postfix() { runner2.runOneTest("scenario2_12_postfix") }
  @Test def test_scenario2_13() { runner2.runOneTest("scenario2_13") }
  @Test def test_scenario2_13_postfix() { runner2.runOneTest("scenario2_13_postfix") }
  @Test def test_scenario2_14() { runner2.runOneTest("scenario2_14") }
  @Test def test_scenario2_14_req_term() { runner2.runOneTest("scenario2_14_req_term") }

  @Test def test_scenario3_1() { runner2.runOneTest("scenario3_1") }
  @Test def test_scenario3_2() { runner2.runOneTest("scenario3_2") }
  @Test def test_scenario3_3() { runner2.runOneTest("scenario3_3") }
  @Test def test_scenario3_4() { runner2.runOneTest("scenario3_4") }
  @Test def test_scenario3_5() { runner2.runOneTest("scenario3_5") }
  @Test def test_scenario3_6() { runner2.runOneTest("scenario3_6") }
  @Test def test_scenario3_7() { runner2.runOneTest("scenario3_7") }
  @Test def test_scenario3_8() { runner2.runOneTest("scenario3_8") }
  @Test def test_scenario3_9() { runner2.runOneTest("scenario3_9") }
  @Test def test_scenario3_10() { runner2.runOneTest("scenario3_10") }
  @Test def test_scenario3_10_postfix() { runner2.runOneTest("scenario3_10_postfix") }
  @Test def test_scenario3_11() { runner2.runOneTest("scenario3_11") }
  @Test def test_scenario3_12() { runner2.runOneTest("scenario3_12") }
  @Test def test_scenario3_12_req_term() { runner2.runOneTest("scenario3_12_req_term") }
  @Test def test_scenario3_13() { runner2.runOneTest("scenario3_13") }
  @Test def test_scenario3_13_postfix() { runner2.runOneTest("scenario3_13_postfix") }
  @Test def test_scenario3_14() { runner2.runOneTest("scenario3_14") }
  @Test def test_scenario3_14_req_term() { runner2.runOneTest("scenario3_14_req_term") }

  @Test def test_scenario4_1() { runner2.runOneTest("scenario4_1") }
  @Test def test_scenario4_2() { runner2.runOneTest("scenario4_2") }
  @Test def test_scenario4_3() { runner2.runOneTest("scenario4_3") }
  @Test def test_scenario4_4() { runner2.runOneTest("scenario4_4") }
  @Test def test_scenario4_5() { runner2.runOneTest("scenario4_5") }
  @Test def test_scenario4_6() { runner2.runOneTest("scenario4_6") }
  @Test def test_scenario4_7() { runner2.runOneTest("scenario4_7") }
  @Test def test_scenario4_7_req_term() { runner2.runOneTest("scenario4_7_req_term") }
  @Test def test_scenario4_8() { runner2.runOneTest("scenario4_8") }
  @Test def test_scenario4_8_postfix() { runner2.runOneTest("scenario4_8_postfix") }
  @Test def test_scenario4_9() { runner2.runOneTest("scenario4_9") }
  @Test def test_scenario4_9_req_term() { runner2.runOneTest("scenario4_9_req_term") }
  @Test def test_scenario4_10() { runner2.runOneTest("scenario4_10") }
  @Test def test_scenario4_10_req_term() { runner2.runOneTest("scenario4_10_req_term") }
  @Test def test_scenario4_11() { runner2.runOneTest("scenario4_11") }
  @Test def test_scenario4_11_postfix() { runner2.runOneTest("scenario4_11_postfix") }
  @Test def test_scenario4_12() { runner2.runOneTest("scenario4_12") }
  @Test def test_scenario4_12_req_term() { runner2.runOneTest("scenario4_12_req_term") }

  @Test def test_scenario5_1() { runner2.runOneTest("scenario5_1") }
}
