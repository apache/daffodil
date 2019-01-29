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

package org.apache.daffodil.section23.dfdl_expressions

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestDFDLExpressions2 {

  val testDir = "org/apache/daffodil/section23/dfdl_expressions/"
  val runner = Runner(testDir, "expressions.tdml")

  val testDir3 = "/org/apache/daffodil/section23/runtime_properties/"
  val runner3 = Runner(testDir3, "runtime-properties.tdml")

  val testDir2 = "/org/apache/daffodil/section23/dfdl_functions/"
  val runner2 = Runner(testDir2, "Functions.tdml")

  val runner_fun = Runner(testDir, "functions.tdml")

  val testDir4 = "/org/apache/daffodil/section23/dfdl_expressions/"
  val runner4 = Runner(testDir4, "expressions.tdml")

  val runner5 = Runner(testDir4, "expressions2.tdml")

  val runner6 = Runner(testDir, "valueLength.tdml")

  val runner7 = Runner(testDir4, "expressions2.tdml", compileAllTopLevel = true)

  @AfterClass def shutdown = {
    runner.reset
    runner2.reset
    runner4.reset
    runner_fun.reset
    runner5.reset
    runner6.reset
    runner7.reset
  }
}

class TestDFDLExpressions2 {
  import TestDFDLExpressions2._

  //DFDL-1233
  @Test def test_nilled_02() { runner2.runOneTest("nilled_02") }
  @Test def test_nilled_03() { runner2.runOneTest("nilled_03") }

  // DFDL-1669
  @Test def test_dfdl_1669_unsignedLong_conversion() { runner5.runOneTest("test_dfdl_1669_unsignedLong_conversion") }

  //DFDL-1657
  @Test def test_valueLengthRef1 { runner6.runOneTest("valueLengthRef1") }

  //DFDL-1706
  @Test def test_valueLengthDfdlLength { runner6.runOneTest("valueLengthDfdlLength") }
  @Test def test_valueLengthDfdlOccursCount { runner6.runOneTest("valueLengthDfdlOccursCount") }
  @Test def test_valueLengthDfdlEncoding { runner6.runOneTest("valueLengthDfdlEncoding") }

  //DFDL-1691
  @Test def test_div01 { runner.runOneTest("div01") }
  @Test def test_div02 { runner.runOneTest("div02") }
  @Test def test_div03 { runner.runOneTest("div03") }
  @Test def test_div04 { runner.runOneTest("div04") }
  @Test def test_div05 { runner.runOneTest("div05") }
  @Test def test_div06 { runner.runOneTest("div06") }
  @Test def test_div07 { runner.runOneTest("div07") }
  @Test def test_div08 { runner.runOneTest("div08") }
  @Test def test_div09 { runner.runOneTest("div09") }
  @Test def test_div10 { runner.runOneTest("div10") }
  @Test def test_div11 { runner.runOneTest("div11") }
  @Test def test_div12 { runner.runOneTest("div12") }
  @Test def test_div13 { runner.runOneTest("div13") }
  @Test def test_div14 { runner.runOneTest("div14") }
  @Test def test_div15 { runner.runOneTest("div15") }
  @Test def test_div16 { runner.runOneTest("div16") }
  @Test def test_div17 { runner.runOneTest("div17") }
  @Test def test_div18 { runner.runOneTest("div18") }
  @Test def test_div19 { runner.runOneTest("div19") }
  @Test def test_div20 { runner.runOneTest("div20") }
  @Test def test_div21 { runner.runOneTest("div21") }
  @Test def test_div22 { runner.runOneTest("div22") }

  @Test def test_idiv01 { runner.runOneTest("idiv01") }
  @Test def test_idiv02 { runner.runOneTest("idiv02") }
  @Test def test_idiv03 { runner.runOneTest("idiv03") }
  @Test def test_idiv04 { runner.runOneTest("idiv04") }
  @Test def test_idiv05 { runner.runOneTest("idiv05") }
  @Test def test_idiv06 { runner.runOneTest("idiv06") }
  @Test def test_idiv07 { runner.runOneTest("idiv07") }
  @Test def test_idiv08 { runner.runOneTest("idiv08") }
  @Test def test_idiv09 { runner.runOneTest("idiv09") }
  @Test def test_idiv10 { runner.runOneTest("idiv10") }
  @Test def test_idiv11 { runner.runOneTest("idiv11") }
  @Test def test_idiv12 { runner.runOneTest("idiv12") }
  @Test def test_idiv13 { runner.runOneTest("idiv13") }
  @Test def test_idiv14 { runner.runOneTest("idiv14") }
  @Test def test_idiv15 { runner.runOneTest("idiv15") }
  @Test def test_idiv16 { runner.runOneTest("idiv16") }
  @Test def test_idiv17 { runner.runOneTest("idiv17") }
  @Test def test_idiv18 { runner.runOneTest("idiv18") }
  @Test def test_idiv19 { runner.runOneTest("idiv19") }
  @Test def test_idiv20 { runner.runOneTest("idiv20") }


  // DFDL-1719
  @Test def test_if_expression_type_01 { runner5.runOneTest("if_expression_type_01") }
  @Test def test_if_expression_type_02 { runner5.runOneTest("if_expression_type_02") }
  @Test def test_if_expression_type_03 { runner5.runOneTest("if_expression_type_03") }
  @Test def test_if_expression_type_04 { runner5.runOneTest("if_expression_type_04") }
  @Test def test_if_expression_type_05 { runner5.runOneTest("if_expression_type_05") }
  @Test def test_if_expression_type_06 { runner5.runOneTest("if_expression_type_06") }
}
