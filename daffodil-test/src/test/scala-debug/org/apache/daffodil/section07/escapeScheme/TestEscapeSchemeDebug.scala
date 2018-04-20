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

import org.junit.Test
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestEscapeSchemeDebug {
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

class TestEscapeSchemeDebug {

  import TestEscapeSchemeDebug._

  @Test def test_escapeSchemeNonUnique() { runner.runOneTest("escapeSchemeNonUnique") }

  val bb = testDir + "escapeScenarios.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(bb), validateTDMLFile = false)

  //DFDL-961
  @Test def test_scenario3_11_postfix() { runner2.runOneTest("scenario3_11_postfix") }


  //DAFFODIL-1923
  @Test def test_escBlkAllQuotes() { runner.runOneTest("escBlkAllQuotes") }
  @Test def test_escBlkEndSame() { runner.runOneTest("escBlkEndSame") }

}
