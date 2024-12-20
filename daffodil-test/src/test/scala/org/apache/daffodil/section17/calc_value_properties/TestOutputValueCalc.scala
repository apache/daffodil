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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestOutputValueCalc1 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section17/calc_value_properties/outputValueCalc.tdml"
}

object TestOutputValueCalc2 extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section17/calc_value_properties/outputValueCalc2.tdml"
}

object TestOutputValueCalc3 extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section17/calc_value_properties/outputValueCalc3.tdml"
}

class TestOutputValueCalc1 extends TdmlTests {
  val tdmlSuite = TestOutputValueCalc1

  @Test def OutputValueCalc_01 = test
  @Test def OutputValueCalc_02 = test
  @Test def OutputValueCalc_03 = test
  @Test def OutputValueCalc_04 = test
  @Test def OutputValueCalc_05 = test
  @Test def OutputValueCalc_06 = test
  @Test def OutputValueCalc_07 = test
  @Test def OutputValueCalc_08 = test

  @Test def binaryIntegerBigEndian = test
  @Test def binaryIntegerLittleEndian = test

  @Test def OutputValueCalc_09 = test
  @Test def OutputValueCalc_10 = test

  @Test def errorZeroArg = test
  @Test def errorOneArg = test
  @Test def errorTwoArg = test
  @Test def errorThreeArg = test

  // DAFFODIL-1701
  @Test def refSimpleTypeElemWithOvc = test
  @Test def refComplexTypeElemNoOvc = test
  @Test def refComplexTypeElemWithOvc = test

  // DAFFODIL-2167
  @Test def arrayWithFollowingOVC = test

  // DAFFODIL-1595
  @Test def OVCTooLargeForElem = test
}

class TestOutputValueCalc2 extends TdmlTests {
  val tdmlSuite = TestOutputValueCalc2

  @Test def ovcHiddenCalculations1 = test
  @Test def ovcHiddenCalculations2 = test
  @Test def ovcHiddenCalculations3 = test
  @Test def hiddenGroupOvcError = test
  @Test def hiddenGroupArrayWithOvc = test
  @Test def optionalWithOvc = test

  @Test def ovcAllowMissingOVCElem = test
  @Test def ovcIgnoreOVCElem = test

  @Test def ovc_w_runtime_initiator = test
  @Test def ovc_w_runtime_dec_sep = test
  @Test def ovc_w_runtime_group_sep = test
  @Test def ovc_w_runtime_exp_rep = test
  @Test def ovc_w_runtime_cal_lang = test
  @Test def ovc_w_runtime_escape_char = test
  @Test def ovc_w_runtime_escape_escape_char = test
}

class TestOutputValueCalc3 extends TdmlTests {
  val tdmlSuite = TestOutputValueCalc3

  // DAFFODIL-2069
  @Test def rHexBinaryLSBF1 = test
  @Test def rHexBinaryLSBF2 = test
  @Test def rStringLSBF1 = test

  @Test def ovc_bitOrderChange = test
}
