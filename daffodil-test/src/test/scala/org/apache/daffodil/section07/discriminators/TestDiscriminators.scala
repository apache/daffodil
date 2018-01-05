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

package org.apache.daffodil.section07.discriminators

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestDiscriminators {
  val testDir = "/org/apache/daffodil/section07/discriminators/"
  val runner = Runner(testDir, "discriminator.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }

}

class TestDiscriminators {

  import TestDiscriminators._

  @Test def test_discriminatorGuidesChoice() { runner.runOneTest("discriminatorGuidesChoice") }
  @Test def test_discriminatorGuidesChoice2() { runner.runOneTest("discriminatorGuidesChoice2") }
  @Test def test_discriminatorGuidesChoice3() = {
    //LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    runner.runOneTest("discriminatorGuidesChoice3")
  }
  @Test def test_discriminatorGuidesChoice4() { runner.runOneTest("discriminatorGuidesChoice4") }
  @Test def test_discriminatorGuidesChoice5() { runner.runOneTest("discriminatorGuidesChoice5") }

  @Test def test_discrimPatternPass() { runner.runOneTest("discrimPatternPass") }
  @Test def test_discrimPatternFail() { runner.runOneTest("discrimPatternFail") }

  @Test def test_discrimPatternFail2() { runner.runOneTest("discrimPatternFail2") }
  @Test def test_discrimPatternFail3() { runner.runOneTest("discrimPatternFail3") }
  @Test def test_choiceBranchDiscrim() { runner.runOneTest("choiceBranchDiscrim") }
  @Test def test_unparseDiscrimIgnored() { runner.runOneTest("unparseDiscrimIgnored") }

  @Test def test_discrimInvalidSchema() { runner.runOneTest("discrimInvalidSchema") }
  @Test def test_discrimOnSimpleType() { runner.runOneTest("discrimOnSimpleType") }
  @Test def test_discrimOnGroupRef() { runner.runOneTest("discrimOnGroupRef") }
  @Test def test_discrimOnGroupRef2() { runner.runOneTest("discrimOnGroupRef2") }
  @Test def test_discrimOnElementRef() { runner.runOneTest("discrimOnElementRef") }
  @Test def test_choiceBranchDiscrimFail() = { runner.runOneTest("choiceBranchDiscrimFail") }

  @Test def test_discrimPatternMatch() = { runner.runOneTest("discrimPatternMatch") }
  @Test def test_discrimPatternNoMatch() = { runner.runOneTest("discrimPatternNoMatch") }

  @Test def test_discrimExpression_01() = { runner.runOneTest("discrimExpression_01") }
  @Test def test_discrimExpression_02() = { runner.runOneTest("discrimExpression_02") }
  @Test def test_discrimExpression_03() = { runner.runOneTest("discrimExpression_03") }
  @Test def test_discrimExpression_04() = { runner.runOneTest("discrimExpression_04") }


  @Test def test_discrimFailStopsFollowingAssert1() { runner.runOneTest("discrimFailStopsFollowingAssert1") }
  @Test def test_discrimPEnotSDE1() { runner.runOneTest("discrimPEnotSDE1") }
  @Test def test_assertSDENotPE1() { runner.runOneTest("assertSDENotPE1") }
  @Test def test_occursCountSDENotPE1() { runner.runOneTest("occursCountSDENotPE1") }
  @Test def test_discrimPEvalueLength1() { runner.runOneTest("discrimPEvalueLength1") }
  @Test def test_discrimPEvalueLengthEnclosingParent1() { runner.runOneTest("discrimPEvalueLengthEnclosingParent1") }

}
