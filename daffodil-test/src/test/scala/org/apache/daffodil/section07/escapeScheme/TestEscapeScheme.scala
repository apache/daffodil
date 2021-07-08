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

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runnerNeg.reset
    runner2.reset
  }
}

class TestEscapeScheme {
  import TestEscapeScheme._
  // Debug Template
  // @Test def test_name() = Debugger.withDebugger {
  // runner.runOneTest("test_name")
  // }

  @Test def test_escapeSchemeSimple(): Unit = { runner.runOneTest("escapeSchemeSimple") }
  @Test def test_escapeSchemeEmpty(): Unit = { runner.runOneTest("escapeSchemeEmpty") }
  @Test def test_escapeSchemeUnused(): Unit = { runner.runOneTest("escapeSchemeUnused") }
  @Test def test_escapeSchemeFail(): Unit = { runner.runOneTest("escapeSchemeFail") }
  @Test def test_escapeSchemeFail2(): Unit = { runner.runOneTest("escapeSchemeFail2") }
  @Test def test_escapeSchemeFail3(): Unit = { runner.runOneTest("escapeSchemeFail3") }
  @Test def test_escapeSchemeNonEmpty(): Unit = { runner.runOneTest("escapeSchemeNonEmpty") }
  // DAFFODIL-844
  //@Test def test_escapeSchemeNonUnique() { runner.runOneTest("escapeSchemeNonUnique") }

  @Test def test_escapeExpressions_01(): Unit = { runner.runOneTest("escapeExpressions_01") }
  @Test def test_escapeExpressions_01b(): Unit = { runner.runOneTest("escapeExpressions_01b") }
  @Test def test_escapeExpressions_02(): Unit = { runner.runOneTest("escapeExpressions_02") }
  @Test def test_escapeExpressions_03(): Unit = { runner.runOneTest("escapeExpressions_03") }
  @Test def test_escapeExpressions_04(): Unit = { runner.runOneTest("escapeExpressions_04") }
  @Test def test_escapeExpressions_05(): Unit = { runner.runOneTest("escapeExpressions_05") }
  @Test def test_escapeExpressions_06(): Unit = { runner.runOneTest("escapeExpressions_06") }

  @Test def test_escapeSchemeNeg(): Unit = { runnerNeg.runOneTest("escapeSchemeNeg") }

  @Test def test_scenario1_1(): Unit = { runner2.runOneTest("scenario1_1") }
  @Test def test_scenario1_2(): Unit = { runner2.runOneTest("scenario1_2") }
  @Test def test_scenario1_3(): Unit = { runner2.runOneTest("scenario1_3") }
  @Test def test_scenario1_4(): Unit = { runner2.runOneTest("scenario1_4") }
  @Test def test_scenario1_5(): Unit = { runner2.runOneTest("scenario1_5") }
  @Test def test_scenario1_6(): Unit = { runner2.runOneTest("scenario1_6") }
  @Test def test_scenario1_7(): Unit = { runner2.runOneTest("scenario1_7") }
  @Test def test_scenario1_7_postfix(): Unit = { runner2.runOneTest("scenario1_7_postfix") }
  @Test def test_scenario1_8(): Unit = { runner2.runOneTest("scenario1_8") }
  @Test def test_scenario1_8_req_term(): Unit = { runner2.runOneTest("scenario1_8_req_term") }
  @Test def test_scenario1_9(): Unit = { runner2.runOneTest("scenario1_9") }
  @Test def test_scenario1_9_postfix(): Unit = { runner2.runOneTest("scenario1_9_postfix") }
  @Test def test_scenario1_10(): Unit = { runner2.runOneTest("scenario1_10") }
  @Test def test_scenario1_10_postfix(): Unit = { runner2.runOneTest("scenario1_10_postfix") }
  @Test def test_scenario1_11(): Unit = { runner2.runOneTest("scenario1_11") }
  @Test def test_scenario1_11_postfix(): Unit = { runner2.runOneTest("scenario1_11_postfix") }
  @Test def test_scenario1_12(): Unit = { runner2.runOneTest("scenario1_12") }
  @Test def test_scenario1_12_postfix(): Unit = { runner2.runOneTest("scenario1_12_postfix") }
  @Test def test_scenario1_13(): Unit = { runner2.runOneTest("scenario1_13") }
  @Test def test_scenario1_13_postfix(): Unit = { runner2.runOneTest("scenario1_13_postfix") }

  @Test def test_scenario2_1(): Unit = { runner2.runOneTest("scenario2_1") }
  @Test def test_scenario2_11_req_term(): Unit = { runner2.runOneTest("scenario2_11_req_term") }
  @Test def test_scenario2_14_req_term(): Unit = { runner2.runOneTest("scenario2_14_req_term") }

  @Test def test_scenario3_1(): Unit = { runner2.runOneTest("scenario3_1") }

  @Test def test_scenario4_7_req_term(): Unit = { runner2.runOneTest("scenario4_7_req_term") }
  @Test def test_scenario4_9_req_term(): Unit = { runner2.runOneTest("scenario4_9_req_term") }
  @Test def test_scenario4_10_req_term(): Unit = { runner2.runOneTest("scenario4_10_req_term") }
  @Test def test_scenario4_12_req_term(): Unit = { runner2.runOneTest("scenario4_12_req_term") }

  @Test def test_scenario5_1(): Unit = { runner2.runOneTest("scenario5_1") }

  @Test def test_escBlkAllQuotes(): Unit = { runner.runOneTest("escBlkAllQuotes") }
  @Test def test_escBlkEndSame(): Unit = { runner.runOneTest("escBlkEndSame") }
  //@Test def test_escBlkMultipleEEC() { runner.runOneTest("escBlkMultipleEEC") } // DAFFODIL-1972
}
