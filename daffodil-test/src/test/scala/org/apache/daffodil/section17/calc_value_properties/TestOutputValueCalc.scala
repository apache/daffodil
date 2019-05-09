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

package org.apache.daffodil.section17.calc_value_properties

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestOutputValueCalc {
  private val testDir = "/org/apache/daffodil/section17/calc_value_properties/"

  val runner = Runner(testDir, "outputValueCalc.tdml")
  val runner2 = Runner(testDir, "outputValueCalc2.tdml")
  val runner3 = Runner(testDir, "outputValueCalc3.tdml")

  @AfterClass def shutdown {
    runner.reset
    runner2.reset
    runner3.reset
  }
}

class TestOutputValueCalc {
  import TestOutputValueCalc._

  @Test def test_OutputValueCalc_01() { runner.runOneTest("OutputValueCalc_01") }
  @Test def test_OutputValueCalc_02() { runner.runOneTest("OutputValueCalc_02") }
  @Test def test_OutputValueCalc_03() { runner.runOneTest("OutputValueCalc_03") }
  @Test def test_OutputValueCalc_04() { runner.runOneTest("OutputValueCalc_04") }
  @Test def test_OutputValueCalc_05() { runner.runOneTest("OutputValueCalc_05") }
  @Test def test_OutputValueCalc_06() { runner.runOneTest("OutputValueCalc_06") }
  @Test def test_OutputValueCalc_07() { runner.runOneTest("OutputValueCalc_07") }
  @Test def test_OutputValueCalc_08() { runner.runOneTest("OutputValueCalc_08") }

  @Test def test_binaryInteger_BigEndian() { runner.runOneTest("binaryIntegerBigEndian") }
  @Test def test_binaryInteger_LittleEndian() { runner.runOneTest("binaryIntegerLittleEndian") }

  @Test def test_ovcHiddenCalculations1() { runner2.runOneTest("ovcHiddenCalculations1") }
  @Test def test_ovcHiddenCalculations2() { runner2.runOneTest("ovcHiddenCalculations2") }
  @Test def test_ovcHiddenCalculations3() { runner2.runOneTest("ovcHiddenCalculations3") }
  @Test def test_hiddenGroupOvcError() { runner2.runOneTest("hiddenGroupOvcError") }
  @Test def test_hiddenGroupArrayWithOvc() { runner2.runOneTest("hiddenGroupArrayWithOvc") }
  @Test def test_optionalWithOvc() { runner2.runOneTest("optionalWithOvc") }

  @Test def test_ovcAllowMissingOVCElem() { runner2.runOneTest("ovcAllowMissingOVCElem") }
  @Test def test_ovcIgnoreOVCElem() { runner2.runOneTest("ovcIgnoreOVCElem") }

  @Test def test_ovc_w_runtime_initiator() { runner2.runOneTest("ovc_w_runtime_initiator") }
  @Test def test_ovc_w_runtime_dec_sep() { runner2.runOneTest("ovc_w_runtime_dec_sep") }
  @Test def test_ovc_w_runtime_group_sep() { runner2.runOneTest("ovc_w_runtime_group_sep") }
  @Test def test_ovc_w_runtime_exp_rep() { runner2.runOneTest("ovc_w_runtime_exp_rep") }
  @Test def test_ovc_w_runtime_cal_lang() { runner2.runOneTest("ovc_w_runtime_cal_lang") }
  @Test def test_ovc_w_runtime_escape_char() { runner2.runOneTest("ovc_w_runtime_escape_char") }
  @Test def test_ovc_w_runtime_escape_escape_char() { runner2.runOneTest("ovc_w_runtime_escape_escape_char") }

  @Test def test_OutputValueCalc_09() { runner.runOneTest("OutputValueCalc_09") }
  @Test def test_OutputValueCalc_10() { runner.runOneTest("OutputValueCalc_10") }

  @Test def test_errorZeroArg() { runner.runOneTest("errorZeroArg") }
  @Test def test_errorOneArg() { runner.runOneTest("errorOneArg") }
  @Test def test_errorTwoArg() { runner.runOneTest("errorTwoArg") }
  @Test def test_errorThreeArg() { runner.runOneTest("errorThreeArg") }

  // DAFFODIL-2069
  @Test def test_ovcHexBinaryLSBF1() { runner3.runOneTest("rHexBinaryLSBF1") }
  @Test def test_ovcHexBinaryLSBF2() { runner3.runOneTest("rHexBinaryLSBF2") }
  @Test def test_ovcStringLSBF1() { runner3.runOneTest("rStringLSBF1") }

  @Test def test_ovcBitOrderChange() { runner3.runOneTest("ovc_bitOrderChange") }
}
