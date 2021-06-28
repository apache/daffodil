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
  val runner2 = Runner(testDir, "multipleDiscriminators.tdml")
  val runner3 = Runner(testDir, "discriminator2.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner2.reset
    runner3.reset
  }

}

class TestDiscriminators {

  import TestDiscriminators._

  @Test def test_discriminatorGuidesChoice(): Unit = { runner.runOneTest("discriminatorGuidesChoice") }
  @Test def test_discriminatorGuidesChoice2(): Unit = { runner.runOneTest("discriminatorGuidesChoice2") }
  @Test def test_discriminatorGuidesChoice3(): Unit = { runner.runOneTest("discriminatorGuidesChoice3") }
  @Test def test_discriminatorGuidesChoice4(): Unit = { runner.runOneTest("discriminatorGuidesChoice4") }
  @Test def test_discriminatorGuidesChoice5(): Unit = { runner.runOneTest("discriminatorGuidesChoice5") }

  @Test def test_discrimPatternPass(): Unit = { runner.runOneTest("discrimPatternPass") }
  @Test def test_discrimPatternFail(): Unit = { runner.runOneTest("discrimPatternFail") }

  @Test def test_discrimPatternFail2(): Unit = { runner.runOneTest("discrimPatternFail2") }
  @Test def test_discrimPatternFail3(): Unit = { runner.runOneTest("discrimPatternFail3") }
  @Test def test_choiceBranchDiscrim(): Unit = { runner.runOneTest("choiceBranchDiscrim") }
  @Test def test_unparseDiscrimIgnored(): Unit = { runner.runOneTest("unparseDiscrimIgnored") }

  @Test def test_discrimInvalidSchema(): Unit = { runner.runOneTest("discrimInvalidSchema") }
  @Test def test_discrimOnSimpleType(): Unit = { runner.runOneTest("discrimOnSimpleType") }
  @Test def test_discrimOnGroupRef(): Unit = { runner.runOneTest("discrimOnGroupRef") }
  @Test def test_discrimOnGroupRef2(): Unit = { runner.runOneTest("discrimOnGroupRef2") }
  @Test def test_discrimOnElementRef(): Unit = { runner.runOneTest("discrimOnElementRef") }
  @Test def test_choiceBranchDiscrimFail() = { runner.runOneTest("choiceBranchDiscrimFail") }

  @Test def test_discrimPatternMatch() = { runner.runOneTest("discrimPatternMatch") }
  @Test def test_discrimPatternNoMatch() = { runner.runOneTest("discrimPatternNoMatch") }

  @Test def test_discrimExpression_01() = { runner.runOneTest("discrimExpression_01") }
  @Test def test_discrimExpression_02() = { runner.runOneTest("discrimExpression_02") }
  @Test def test_discrimExpression_03() = { runner.runOneTest("discrimExpression_03") }

  // DAFFODIL-1971
  // @Test def test_discrimExpression_04() = { runner.runOneTest("discrimExpression_04") }

  @Test def test_discrimFailStopsFollowingAssert1(): Unit = { runner.runOneTest("discrimFailStopsFollowingAssert1") }
  @Test def test_discrimPEnotSDE1(): Unit = { runner.runOneTest("discrimPEnotSDE1") }
  @Test def test_assertSDENotPE1(): Unit = { runner.runOneTest("assertSDENotPE1") }
  @Test def test_occursCountSDENotPE1(): Unit = { runner.runOneTest("occursCountSDENotPE1") }
  @Test def test_discrimPEvalueLength1(): Unit = { runner.runOneTest("discrimPEvalueLength1") }
  @Test def test_discrimPEvalueLengthEnclosingParent1(): Unit = { runner.runOneTest("discrimPEvalueLengthEnclosingParent1") }

  @Test def test_multipleDiscriminators1(): Unit = { runner2.runOneTest("multipleDiscriminators1") }
  @Test def test_multipleDiscriminators2(): Unit = { runner2.runOneTest("multipleDiscriminators2") }
  @Test def test_multipleDiscriminators3(): Unit = { runner2.runOneTest("multipleDiscriminators3") }
  @Test def test_multipleDiscriminators4(): Unit = { runner2.runOneTest("multipleDiscriminators4") }
  @Test def test_multipleDiscriminators5(): Unit = { runner2.runOneTest("multipleDiscriminators5") }

  @Test def test_discrimPlacementSDW(): Unit = { runner.runOneTest("discrimPlacementSDW") }
  @Test def test_assertPlacementSDW(): Unit = { runner.runOneTest("assertPlacementSDW") }

  @Test def test_nameDOB_test1(): Unit = { runner3.runOneTest("nameDOB_test1") }
  @Test def test_nameDOB_test_bad_date_first_row(): Unit = { runner3.runOneTest("nameDOB_test_bad_date_first_row") }

  // DAFFODIL-2486 - discriminator bug - interaction with separators
  // @Test def test_nameDOB_test_bad_1(): Unit = { runner3.runOneTest("nameDOB_test_bad_1") }

  @Test def test_nameDOB_test_bad_using_terminators(): Unit = { runner3.runOneTest("nameDOB_test_bad_using_terminators") }
}
