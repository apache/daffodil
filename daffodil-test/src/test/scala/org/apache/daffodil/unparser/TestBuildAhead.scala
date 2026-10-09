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

package org.apache.daffodil.unparser

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestBuildAhead extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/unparser/buildAhead.tdml"
}

class TestBuildAhead extends TdmlTests {
  val tdmlSuite = TestBuildAhead

  @Test def ovcSuspension = test
  @Test def arrayChoiceSeparator = test
  @Test def absentTrailingOptionalSuppressesSeparator = test
  @Test def hiddenChoice = test
  @Test def trailingArrayPostfixSeparator = test
  @Test def choiceBranchWithAbsentOptionalElement = test
  @Test def choiceBranchWithPresentOptionalElement = test
  @Test def manyOccurrenceArrayWithSmallBuildAheadLimit = test
  @Test def nestedBareSequence = test
  @Test def fixedLengthChoicePadding = test
  @Test def hiddenGroup = test
  @Test def initiatorTerminator = test
  @Test def nillableComplexWithEmptyContentNotNilled = test
  @Test def nillableComplexWithEmptyContentNilled = test
  @Test def hiddenGroupWithEmptyBody = test
  @Test def escapeScheme = test
  @Test def layeredSequence = test
  @Test def layeredSequenceLengthMismatchError = test
  @Test def nestedOvcSuspensions = test
  @Test def binaryIntArray = test
  @Test def variableLengthExpression = test
  @Test def prefixedLength = test
  @Test def prefixedLengthComplexContent = test
  @Test def ovcCountOfPrecedingArray = test
  @Test def ovcCountOfPrecedingArrayAfterBuildFullyFinishes = test
  @Test def dynamicTerminatorReferencingArrayCount = test
  @Test def ivcExistsOverNestedArray = test
  @Test def manyValueLengthForwardReferencesThrottled = test
  @Test def nviScopedVariableWithValueLengthOVC = test
  @Test def singleNviScopedVariableWithValueLengthOVC = test
  @Test def nviScopedSetVariableWithNoForwardReference = test
  @Test def delimitedVariableLengthExpression = test
  @Test def delimitedComplexVariableLengthExpression = test
  @Test def purelyContentLengthOVC = test
  @Test def mixedResolvableAndContentLengthOVC = test
  @Test def eventDrivenInfosetMismatchReportsDataLocation = test
}
