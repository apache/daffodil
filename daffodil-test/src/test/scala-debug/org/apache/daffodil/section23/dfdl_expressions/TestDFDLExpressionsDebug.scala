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

object TestDFDLExpressionsDebug {
  val testDir = "/org/apache/daffodil/section23/dfdl_expressions/"
  val testDir2 = "/org/apache/daffodil/section23/dfdl_functions/"

  // I'm not sure these belong in section23, but there is no section of the spec that
  // is about all these properties together, yet, since there is common mechanism here
  // I really think their tests should not be scattered all over to the sections where each
  // property is defined.

  val testDir4 = "/org/apache/daffodil/section23/runtime_properties/"

  val runner = Runner(testDir, "expressions.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner2 = Runner(testDir2, "Functions.tdml")
  val runner2_utf8 = Runner(testDir2, "Functions_UTF8.tdml")
  val runner2b = Runner(testDir2, "Functions-neg.tdml")
  val runner3 = Runner(testDir, "expression_fail.tdml", validateTDMLFile = false)
  val runner4 = Runner(testDir4, "runtime-properties.tdml", validateTDMLFile = true, validateDFDLSchemas = false)
  val runner_fun = Runner(testDir, "functions.tdml")

  val testDir5 = "/org/apache/daffodil/section23/dfdl_expressions/"
  val runner5 = Runner(testDir5, "expressions.tdml")

  @AfterClass def shutDown() {
    runner4.reset
    runner.reset
    runner_fun.reset
    runner2.reset
    runner2_utf8.reset
    runner2b.reset
    runner3.reset
  }
}

class TestDFDLExpressionsDebug {

  import TestDFDLExpressionsDebug._

  // DAFFODIL-1986
  @Test def test_comparison_operators_46a() { runner.runOneTest("comparison_operators_46a") }

  //DFDL-1287
  @Test def test_internal_space_preserved4() { runner.runOneTest("internal_space_preserved4") }
  @Test def test_internal_space_not_preserved2() { runner.runOneTest("internal_space_not_preserved2") }

  //DFDL-1146
  @Test def test_attribute_axis_01() { runner.runOneTest("attribute_axis_01") }
  @Test def test_attribute_axis_02() { runner.runOneTest("attribute_axis_02") }
  @Test def test_attribute_axis_03() { runner.runOneTest("attribute_axis_03") }

  //DFDL-1111
  @Test def test_diagnostics_01() { runner.runOneTest("diagnostics_01") }
  @Test def test_diagnostics_02() { runner.runOneTest("diagnostics_02") }
  @Test def test_diagnostics_03() { runner.runOneTest("diagnostics_03") }

  //DFDL-1221
  @Test def test_beyondRoot_01() { runner.runOneTest("beyondRoot_01") }

  //DFDL-1035 - tests need better diagnostic
  @Test def test_dfdlCheckConstraints() { runner.runOneTest("dfdlCheckConstraints") }
  @Test def test_dfdlCheckConstraints2() { runner.runOneTest("dfdlCheckConstraints2") }

  @Test def test_regexCompatFail() { runner.runOneTest("regexCompatFail") }

  // lengthUnits bytes with variable-width charater set and specified lengthKind
  @Test def test_lke3_rel() { runner.runOneTest("lke3_rel") } // uses lengthUnits bytes with utf-8

  // DFDL-1043
  @Test def test_checkConstraintsComplexTypeFails() { runner.runOneTest("checkConstraintsComplexTypeFails") }

  // DFDL-1044
  @Test def test_expression_type_error2() { runner.runOneTest("expression_type_error2") }

  //DFDL-1164
  @Test def test_predicate_02() { runner.runOneTest("predicate_02") }
  @Test def test_predicate_03() { runner.runOneTest("predicate_03") }

  //DFDL-1059
  @Test def test_self_axis_01() { runner.runOneTest("self_axis_01") }
  @Test def test_multiple_axis_01() { runner.runOneTest("multiple_axis_01") }

  //DFDL-711
  @Test def test_short_parent_axis_01() { runner.runOneTest("short_parent_axis_01") }

  //DFDL-1076
  @Test def test_not_04() { runner2.runOneTest("not_04") }

  //DFDL-1115
  @Test def test_xsDateTime_constructor_03() { runner2.runOneTest("xsDateTime_constructor_03") }

  //DFDL-1159 (unordered sequences)
  @Test def test_count_05() { runner2.runOneTest("count_05") }
  @Test def test_count_06() { runner2.runOneTest("count_06") }
  @Test def test_count_08() { runner2.runOneTest("count_08") }

  //DFDL-1118
  @Test def test_more_count_0() { runner2.runOneTest("more_count_0") }
  @Test def test_more_count_1() { runner2.runOneTest("more_count_1") }
  @Test def test_more_count_1b_2() { runner2.runOneTest("more_count_1b_2") }
  @Test def test_more_count_2() { runner2.runOneTest("more_count_2") }

  //DFDL-1075
  @Test def test_not_05() { runner2.runOneTest("not_05") }
  @Test def test_not_07() { runner2.runOneTest("not_07") }

  //DFDL-1120
  @Test def test_exists_10() { runner2.runOneTest("exists_10") }

  //DFDL-1124
  @Test def test_date_constructor_01() { runner2.runOneTest("date_constructor_01") }

  //DFDL-1080
  @Test def test_empty_02() { runner2.runOneTest("empty_02") }
  @Test def test_exists_02() { runner2.runOneTest("exists_02") }

  //DFDL-1189
  @Test def test_exactly_one_01() { runner2.runOneTest("exactly_one_01") }
  @Test def test_exactly_one_02() { runner2.runOneTest("exactly_one_02") }
  @Test def test_exactly_one_03() { runner2.runOneTest("exactly_one_03") }
  @Test def test_exactly_one_04() { runner2.runOneTest("exactly_one_04") }
  @Test def test_exactly_one_05() { runner2.runOneTest("exactly_one_05") }
  @Test def test_exactly_one_06() { runner2.runOneTest("exactly_one_06") }

  //DFDL-1091
  @Test def test_count_05b() { runner2.runOneTest("count_05b") }

  // Fails due to invariant failure slotIndexInParent
  @Test def test_local_name_07() { runner2.runOneTest("local_name_07") }

  //DFDL-1097
  @Test def test_local_name_06() { runner2.runOneTest("local_name_06") }

  //DFDL-1101
  @Test def test_namespace_uri_01() { runner2.runOneTest("namespace_uri_01") }
  @Test def test_namespace_uri_02() { runner2.runOneTest("namespace_uri_02") }
  //DFDL-1114
  @Test def test_namespace_uri_03() { runner2.runOneTest("namespace_uri_03") }
  @Test def test_namespace_uri_04() { runner2.runOneTest("namespace_uri_04") }
  @Test def test_namespace_uri_05() { runner2.runOneTest("namespace_uri_05") }
  @Test def test_namespace_uri_06() { runner2.runOneTest("namespace_uri_06") }

  // These tests had the same names as others in the tdml file, and so were not
  // being seen or run. Names changed so they run, and they fail.
  @Test def test_date_constructor_02a() { runner2.runOneTest("date_constructor_02a") }
  @Test def test_date_constructor_03a() { runner2.runOneTest("date_constructor_03a") }
  @Test def test_nonNeg_constructor_02a() { runner2.runOneTest("nonNeg_constructor_02a") }

  @Test def test_element_long_form_whitespace() { runner.runOneTest("element_long_form_whitespace") }
}
