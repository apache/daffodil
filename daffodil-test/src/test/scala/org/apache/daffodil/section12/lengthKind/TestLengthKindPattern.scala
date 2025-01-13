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

package org.apache.daffodil.section12.lengthKind

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestLengthKindPattern extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/PatternTests.tdml"
}

object TestLengthKindPatternAI extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/AI.tdml"
}

class TestLengthKindPattern extends TdmlTests {
  val tdmlSuite = TestLengthKindPattern

  @Test def unmatchedPattern01 = test
  @Test def unmatchedPattern02 = test
  @Test def unmatchedPattern03 = test

  @Test def invalid_pattern = test
  @Test def invalid_pattern2 = test
  @Test def invalid_pattern3 = test

  @Test def AI000_rev = test
  @Test def LengthKindPattern = test
  @Test def LengthKindPatternCompound = test
  @Test def LengthKindPatternCompound2 = test
  @Test def lengthKindPattern_01 = test
  @Test def lengthKindPattern_02 = test
  @Test def lengthKindPattern_03 = test
  @Test def lengthKindPattern_04 = test

  @Test def LengthPatternIllegalBits_01 = test
  @Test def LengthPatternLegalBits_01 = test

  // DFDL-309
  @Test def LengthPatternIllegalBits_02_EncodingErrorPolicy_Replace = test
  // DFDL-935 dfdl:encodingErrorPolicy='error'
  @Ignore @Test def LengthPatternIllegalBits_02_EncodingErrorPolicy_Error = test

  @Test def LengthPatternLegalBits_02 = test
  @Test def lengthKindPatternFail = test

  @Test def ComplexWithBinaryChild = test

  @Test def LengthPatternNil_NoNil = test
  @Test def LengthPatternNil_FindsNil = test
  @Test def LengthPatternNil_EmptyStringAllowed = test
  @Test def nested_patterns = test
  @Test def nested_patterns_01 = test
  @Test def nested_patterns_02 = test
  @Test def nested_patterns_03 = test

  @Test def hexBinaryLengthKindPattern01 = test

  @Test def lengthPatternEncodingErrorReplace = test

  @Test def lengthPatternBinaryPatternLimit = test
}

class TestLengthKindPatternAI extends TdmlTests {
  val tdmlSuite = TestLengthKindPatternAI

  @Test def AI000 = test
}
