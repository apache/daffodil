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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestDiscriminator extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/discriminators/discriminator.tdml"
}

object TestDiscriminator2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/discriminators/discriminator2.tdml"
}

object TestMultipleDiscriminators extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/discriminators/multipleDiscriminators.tdml"
}

class TestDiscriminator extends TdmlTests {
  val tdmlSuite = TestDiscriminator

  @Test def discriminatorGuidesChoice = test
  @Test def discriminatorGuidesChoice2 = test
  @Test def discriminatorGuidesChoice3 = test
  @Test def discriminatorGuidesChoice4 = test
  @Test def discriminatorGuidesChoice5 = test

  @Test def discrimPatternPass = test
  @Test def discrimPatternFail = test

  @Test def discrimPatternFail2 = test
  @Test def discrimPatternFail3 = test
  @Test def choiceBranchDiscrim = test
  @Test def unparseDiscrimIgnored = test

  @Test def discrimInvalidSchema = test
  @Test def discrimOnSimpleType = test
  @Test def discrimOnGroupRef = test
  @Test def discrimOnGroupRef2 = test
  @Test def discrimOnElementRef = test
  @Test def choiceBranchDiscrimFail = test

  @Test def discrimPatternMatch = test
  @Test def discrimPatternNoMatch = test

  @Test def discrimExpression_01 = test
  @Test def discrimExpression_02 = test
  @Test def discrimExpression_03 = test

  // DAFFODIL-1971
  @Ignore @Test def discrimExpression_04 = test

  @Test def discrimFailStopsFollowingAssert1 = test
  @Test def discrimPEnotSDE1 = test
  @Test def assertSDENotPE1 = test
  @Test def occursCountSDENotPE1 = test
  @Test def discrimPEvalueLength1 = test
  @Test def discrimPEvalueLengthEnclosingParent1 = test
  @Test def discrimOnChoiceArray_01 = test

  @Test def discrimPlacementExpressionSDW = test
  @Test def discrimPlacementPatternSDW = test
  @Test def assertPlacementExpressionSDW = test
  @Test def assertPlacementPatternSDW = test
}

class TestDiscriminator2 extends TdmlTests {
  val tdmlSuite = TestDiscriminator2

  @Test def nameDOB_test1 = test
  @Test def nameDOB_test_bad_date_first_row = test
  // DAFFODIL-2486 - discriminator bug - interaction with separators
  @Ignore @Test def nameDOB_test_bad_1 = test

  @Test def nameDOB_test_bad_using_terminators = test
}

class TestMultipleDiscriminators extends TdmlTests {
  val tdmlSuite = TestMultipleDiscriminators

  @Test def multipleDiscriminators1 = test
  @Test def multipleDiscriminators2 = test
  @Test def multipleDiscriminators3 = test
  @Test def multipleDiscriminators4 = test
  @Test def multipleDiscriminators5 = test
}
