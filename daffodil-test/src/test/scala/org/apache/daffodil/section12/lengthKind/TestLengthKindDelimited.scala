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
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLengthKindDelimited {
  private val testDir = "/org/apache/daffodil/section12/lengthKind/"

  val runner = Runner(testDir, "DelimitedTests.tdml")
  val runnerAB = Runner(testDir, "AB.tdml")
  val runnerAN = Runner(testDir, "AN.tdml")
  val runner_01 = Runner("/org/apache/daffodil/ibm-tests/", "dpaext1.tdml")

  @AfterClass def shutDown {
    runner.reset
    runnerAB.reset
    runnerAN.reset
    runner_01.reset
  }

}

class TestLengthKindDelimited {

  import TestLengthKindDelimited._

  @Test def test_delimited_binary_int_seqSep() = { runner.runOneTest("delimited_binary_int_seqSep") }

  @Test def test_DoubleNewLineTerminator() = { runner.runOneTest("TestDoubleNewLineTerminator") }
  @Test def test_DoubleNewLineSeparator() = { runner.runOneTest("TestDoubleNewLineSeparator") }
  @Test def test_DoubleNewLineSeparatorBasic() = { runner.runOneTest("TestDoubleNewLineSeparatorBasic") }

  @Test def test_NumSeq_00a() = { runner.runOneTest("NumSeq_00a") }
  @Test def test_NumSeq_00nl() = { runner.runOneTest("NumSeq_00nl") }
  @Test def test_NumSeq_01() = { runner.runOneTest("NumSeq_01") }
  @Test def test_nested_NumSeq_01() = { runner.runOneTest("nested_NumSeq_01") }
  @Test def test_eofTest1() { runner.runOneTest("eofTest1") }
  @Test def test_NumSeq_03() { runner.runOneTest("NumSeq_03") }
  @Test def test_NumSeq_04() { runner.runOneTest("NumSeq_04") }
  @Test def test_NumSeq_05() { runner.runOneTest("NumSeq_05") }
  @Test def test_NumSeq_06() { runner.runOneTest("NumSeq_06") }
  @Test def test_NumSeq_07() { runner.runOneTest("NumSeq_07") }
  @Test def test_NumSeq_08() { runner.runOneTest("NumSeq_08") }
  @Test def test_NumSeq_09() { runner.runOneTest("NumSeq_09") }
  //  @Test def test_NumSeq_10() { runner.runOneTest("NumSeq_10") }
  @Test def test_lengthKindDelimited_01() { runner.runOneTest("lengthKindDelimited_01") }
  @Test def test_lengthKindDelimited_02() { runner.runOneTest("lengthKindDelimited_02") }
  @Test def test_lengthKindDelimited_03() { runner.runOneTest("lengthKindDelimited_03") }
  @Test def test_lengthKindDelimited_04() { runner.runOneTest("lengthKindDelimited_04") }
  @Test def test_NumSeq_11() { runner.runOneTest("NumSeq_11") }
  @Test def test_NumSeq_12() { runner.runOneTest("NumSeq_12") }
  @Test def test_NumSeq_13() { runner.runOneTest("NumSeq_13") }
  @Test def test_NumSeq_13Fail() { runner.runOneTest("NumSeq_13Fail") }
  @Test def test_NumSeq_14() { runner.runOneTest("NumSeq_14") }
  // Tests that initiator is found when on ElementRef
  @Test def test_refInitiator() { runner.runOneTest("refInitiator") }
  // Tests that initiator is found when on GlobalElmentDecl
  @Test def test_refInitiator2() { runner.runOneTest("refInitiator2") }
  @Test def test_binary_delimited_fail() { runner.runOneTest("binary_delimited_fail") }
  @Test def test_Lesson1_lengthKind_delimited() { runner.runOneTest("Lesson1_lengthKind_delimited") }
  @Test def test_Lesson4_delimited_fixed_length() { runner.runOneTest("Lesson4_delimited_fixed_length") }
  @Test def test_delimited_construct() { runner.runOneTest("delimited_construct") }

  @Test def test_AB000() { runnerAB.runOneTest("AB000") }
  @Test def test_AB001() { runnerAB.runOneTest("AB001") }
  @Test def test_AB002() { runnerAB.runOneTest("AB002") }
  @Test def test_AB003() { runnerAB.runOneTest("AB003") }
  @Test def test_AB004() { runnerAB.runOneTest("AB004") }
  @Test def test_AB005() { runnerAB.runOneTest("AB005") }

  @Test def test_AN000() { runnerAN.runOneTest("AN000") }
  @Test def test_AN001() { runnerAN.runOneTest("AN001") }

  @Test def test_introduction_1_02() { runner_01.runOneTest("introduction_1_02") }
  @Test def test_length_delimited_12_03() { runner_01.runOneTest("length_delimited_12_03") }
  @Test def test_length_delimited_12_02() { runner_01.runOneTest("length_delimited_12_02") }
  @Test def test_multiple_delimiters() { runner_01.runOneTest("multiple_delimiters") }

}
