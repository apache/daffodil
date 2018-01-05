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

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestLengthKindPattern {

  val testDir = "/org/apache/daffodil/section12/lengthKind/"

  val runner = Runner(testDir, "PatternTests.tdml")

  val runnerAI = Runner(testDir, "AI.tdml")

  @AfterClass def shutDown {
    runner.reset
    runnerAI.reset
  }
}

class TestLengthKindPattern {

  import TestLengthKindPattern._

  @Test def test_unmatchedPattern01() { runner.runOneTest("unmatchedPattern01") }
  @Test def test_unmatchedPattern02() { runner.runOneTest("unmatchedPattern02") }
  @Test def test_unmatchedPattern03() { runner.runOneTest("unmatchedPattern03") }

  @Test def test_invalid_pattern() { runner.runOneTest("invalid_pattern") }
  @Test def test_invalid_pattern2() { runner.runOneTest("invalid_pattern2") }
  @Test def test_invalid_pattern3() { runner.runOneTest("invalid_pattern3") }

  @Test def test_AI000_rev() { runner.runOneTest("AI000_rev") } // round trip
  @Test def test_LengthKindPattern() { runner.runOneTest("LengthKindPattern") } // round trip
  @Test def test_LengthKindPatternCompound() { runner.runOneTest("LengthKindPatternCompound") }
  @Test def test_LengthKindPatternCompound2() { runner.runOneTest("LengthKindPatternCompound2") } // round trip
  @Test def test_lengthKindPattern_01() { runner.runOneTest("lengthKindPattern_01") } // round trip
  @Test def test_lengthKindPattern_02() { runner.runOneTest("lengthKindPattern_02") } // round trip
  @Test def test_lengthKindPattern_03() { runner.runOneTest("lengthKindPattern_03") } // round trip
  @Test def test_lengthKindPattern_04() { runner.runOneTest("lengthKindPattern_04") } // round trip

  @Test def test_LengthPatternIllegalBits_01() { runner.runOneTest("LengthPatternIllegalBits_01") }
  @Test def test_LengthPatternLegalBits_01() { runner.runOneTest("LengthPatternLegalBits_01") }

  // DFDL-309
  @Test def test_LengthPatternIllegalBits_02_EncodingErrorPolicy_Replace() { runner.runOneTest("LengthPatternIllegalBits_02_EncodingErrorPolicy_Replace") }

  @Test def test_LengthPatternLegalBits_02() { runner.runOneTest("LengthPatternLegalBits_02") } // round trip
  @Test def test_lengthKindPatternFail() { runner.runOneTest("lengthKindPatternFail") }

  @Test def test_ComplexWithBinaryChild() { runner.runOneTest("ComplexWithBinaryChild") }

  @Test def test_AI000() { runnerAI.runOneTest("AI000") }

  @Test def test_LengthPatternNil_NoNil() { runner.runOneTest("LengthPatternNil_NoNil") } // round trip
  @Test def test_LengthPatternNil_FindsNil() { runner.runOneTest("LengthPatternNil_FindsNil") } // round trip
  @Test def test_LengthPatternNil_EmptyStringAllowed() { runner.runOneTest("LengthPatternNil_EmptyStringAllowed") }
  @Test def test_nested_patterns() { runner.runOneTest("nested_patterns") }
  @Test def test_nested_patterns_01() { runner.runOneTest("nested_patterns_01") }
  @Test def test_nested_patterns_02() { runner.runOneTest("nested_patterns_02") }
  @Test def test_nested_patterns_03() { runner.runOneTest("nested_patterns_03") }

  @Test def test_hexBinaryLengthKindPattern01() { runner.runOneTest("hexBinaryLengthKindPattern01") }
}
