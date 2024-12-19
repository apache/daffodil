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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestEscapeScheme extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/escapeScheme/escapeScheme.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

object TestEscapeSchemeNeg extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/escapeScheme/escapeSchemeNeg.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

object TestEscapeScenarios extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/escapeScheme/escapeScenarios.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestEscapeScheme extends TdmlTests {
  val tdmlSuite = TestEscapeScheme

  @Test def escapeSchemeSimple = test
  @Test def escapeSchemeEmpty = test
  @Test def escapeSchemeUnused = test
  @Test def escapeSchemeFail = test
  @Test def escapeSchemeFail2 = test
  @Test def escapeSchemeFail3 = test
  @Test def escapeSchemeNonEmpty = test
  // DAFFODIL-844
  @Ignore @Test def escapeSchemeNonUnique = test

  @Test def escapeExpressions_01 = test
  @Test def escapeExpressions_01b = test
  @Test def escapeExpressions_02 = test
  @Test def escapeExpressions_03 = test
  @Test def escapeExpressions_04 = test
  @Test def escapeExpressions_05 = test
  @Test def escapeExpressions_06 = test
  @Test def escapeExpressions_07 = test
  @Test def escapeExpressions_08 = test

  @Test def escBlkAllQuotes = test
  @Test def escBlkEndSame = test
  @Test def escBlkEndSame2 = test
  @Test def escBlkEndSame3 = test
  @Ignore @Test def escBlkMultipleEEC = test

  @Test def escapeScheme_with_comment = test
}

class TestEscapeSchemeNeg extends TdmlTests {
  val tdmlSuite = TestEscapeSchemeNeg

  @Test def escapeSchemeNeg = test
}

class TestEscapeScenarios extends TdmlTests {
  val tdmlSuite = TestEscapeScenarios

  @Test def scenario1_1 = test
  @Test def scenario1_2 = test
  @Test def scenario1_3 = test
  @Test def scenario1_4 = test
  @Test def scenario1_5 = test
  @Test def scenario1_6 = test
  @Test def scenario1_7 = test
  @Test def scenario1_7_postfix = test
  @Test def scenario1_8 = test
  @Test def scenario1_8_req_term = test
  @Test def scenario1_9 = test
  @Test def scenario1_9_postfix = test
  @Test def scenario1_10 = test
  @Test def scenario1_10_postfix = test
  @Test def scenario1_11 = test
  @Test def scenario1_11_postfix = test
  @Test def scenario1_12 = test
  @Test def scenario1_12_postfix = test
  @Test def scenario1_13 = test
  @Test def scenario1_13_postfix = test

  @Test def scenario2_1 = test
  @Test def scenario2_11_req_term = test
  @Test def scenario2_14_req_term = test

  @Test def scenario3_1 = test

  @Test def scenario4_7_req_term = test
  @Test def scenario4_9_req_term = test
  @Test def scenario4_10_req_term = test
  @Test def scenario4_12_req_term = test

  @Test def scenario5_1 = test
}
