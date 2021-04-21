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

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runnerAI.reset
  }
}

class TestLengthKindPattern {

  import TestLengthKindPattern._

  @Test def test_unmatchedPattern01(): Unit = { runner.runOneTest("unmatchedPattern01") }
  @Test def test_unmatchedPattern02(): Unit = { runner.runOneTest("unmatchedPattern02") }
  @Test def test_unmatchedPattern03(): Unit = { runner.runOneTest("unmatchedPattern03") }

  @Test def test_invalid_pattern(): Unit = { runner.runOneTest("invalid_pattern") }
  @Test def test_invalid_pattern2(): Unit = { runner.runOneTest("invalid_pattern2") }
  @Test def test_invalid_pattern3(): Unit = { runner.runOneTest("invalid_pattern3") }

  @Test def test_AI000_rev(): Unit = { runner.runOneTest("AI000_rev") } // round trip
  @Test def test_LengthKindPattern(): Unit = { runner.runOneTest("LengthKindPattern") } // round trip
  @Test def test_LengthKindPatternCompound(): Unit = { runner.runOneTest("LengthKindPatternCompound") }
  @Test def test_LengthKindPatternCompound2(): Unit = { runner.runOneTest("LengthKindPatternCompound2") } // round trip
  @Test def test_lengthKindPattern_01(): Unit = { runner.runOneTest("lengthKindPattern_01") } // round trip
  @Test def test_lengthKindPattern_02(): Unit = { runner.runOneTest("lengthKindPattern_02") } // round trip
  @Test def test_lengthKindPattern_03(): Unit = { runner.runOneTest("lengthKindPattern_03") } // round trip
  @Test def test_lengthKindPattern_04(): Unit = { runner.runOneTest("lengthKindPattern_04") } // round trip

  @Test def test_LengthPatternIllegalBits_01(): Unit = { runner.runOneTest("LengthPatternIllegalBits_01") }
  @Test def test_LengthPatternLegalBits_01(): Unit = { runner.runOneTest("LengthPatternLegalBits_01") }

  // DFDL-309
  @Test def test_LengthPatternIllegalBits_02_EncodingErrorPolicy_Replace(): Unit = { runner.runOneTest("LengthPatternIllegalBits_02_EncodingErrorPolicy_Replace") }
  // DFDL-935 dfdl:encodingErrorPolicy='error'
  //@Test def test_LengthPatternIllegalBits_02_EncodingErrorPolicy_Error() { runner.runOneTest("LengthPatternIllegalBits_02_EncodingErrorPolicy_Error") }

  @Test def test_LengthPatternLegalBits_02(): Unit = { runner.runOneTest("LengthPatternLegalBits_02") } // round trip
  @Test def test_lengthKindPatternFail(): Unit = { runner.runOneTest("lengthKindPatternFail") }

  @Test def test_ComplexWithBinaryChild(): Unit = { runner.runOneTest("ComplexWithBinaryChild") }

  @Test def test_AI000(): Unit = { runnerAI.runOneTest("AI000") }

  @Test def test_LengthPatternNil_NoNil(): Unit = { runner.runOneTest("LengthPatternNil_NoNil") } // round trip
  @Test def test_LengthPatternNil_FindsNil(): Unit = { runner.runOneTest("LengthPatternNil_FindsNil") } // round trip
  @Test def test_LengthPatternNil_EmptyStringAllowed(): Unit = { runner.runOneTest("LengthPatternNil_EmptyStringAllowed") }
  @Test def test_nested_patterns(): Unit = { runner.runOneTest("nested_patterns") }
  @Test def test_nested_patterns_01(): Unit = { runner.runOneTest("nested_patterns_01") }
  @Test def test_nested_patterns_02(): Unit = { runner.runOneTest("nested_patterns_02") }
  @Test def test_nested_patterns_03(): Unit = { runner.runOneTest("nested_patterns_03") }

  @Test def test_hexBinaryLengthKindPattern01(): Unit = { runner.runOneTest("hexBinaryLengthKindPattern01") }

  @Test def test_lengthPatternEncodingErrorReplace(): Unit = { runner.runOneTest("lengthPatternEncodingErrorReplace") }

  @Test def test_lengthPatternBinaryPatternLimit(): Unit = { runner.runOneTest("lengthPatternBinaryPatternLimit") }
}
