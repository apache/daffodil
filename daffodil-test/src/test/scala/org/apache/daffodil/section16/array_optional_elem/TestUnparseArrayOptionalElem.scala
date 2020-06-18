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

package org.apache.daffodil.section16.array_optional_elem

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit._

object TestUnparseArrayOptionalElem {

  val testDir = "/org/apache/daffodil/section16/array_optional_elem/"

  val runner_fixed = Runner(testDir, "UnparseArrayFixedOptionalElem.tdml")
  val runner_imp = Runner(testDir, "UnparseArrayImplicitOptionalElem.tdml")
  val runner_parsed = Runner(testDir, "UnparseArrayParsedOptionalElem.tdml")
  val runner_expr = Runner(testDir, "UnparseArrayExpressionConstant.tdml")
  val runner_delim = Runner(testDir, "UnparseArrayDelimitedOptionalElem.tdml")

  @AfterClass def tearDown(): Unit = {
    runner_fixed.reset
    runner_imp.reset
    runner_parsed.reset
    runner_expr.reset
    runner_delim.reset
  }

}

class TestUnparseArrayOptionalElem {

  import TestUnparseArrayOptionalElem._

  @Test def test_exprOptPresent(): Unit = { runner_expr.runOneTest("exprOptPresent") }
  @Test def test_exprOptPresentArray(): Unit = { runner_expr.runOneTest("exprOptPresentArray") }
  @Test def test_exprOptAbsentArray(): Unit = { runner_expr.runOneTest("exprOptAbsentArray") }
  @Test def test_exprOptTwoArrays(): Unit = { runner_expr.runOneTest("exprOptTwoArrays") }
  @Test def test_exprOptScalarThenArray(): Unit = { runner_expr.runOneTest("exprOptScalarThenArray") }
  @Test def test_exprOptArrayThenScalar(): Unit = { runner_expr.runOneTest("exprOptArrayThenScalar") }

  @Test def test_exprOptParsedData_01(): Unit = { runner_expr.runOneTest("exprOptParsedData_01") }
  @Test def test_exprOptParsedData_02(): Unit = { runner_expr.runOneTest("exprOptParsedData_02") }
  @Test def test_exprOptParsedData_03(): Unit = { runner_expr.runOneTest("exprOptParsedData_03") }
  @Test def test_exprOptParsedData_04(): Unit = { runner_expr.runOneTest("exprOptParsedData_04") }

  @Test def test_fixedUnparseArrayTooManyElements01(): Unit = { runner_fixed.runOneTest("fixedUnparseArrayTooManyElements01") }
  @Test def test_fixedUnparseArrayTooFewElements01(): Unit = { runner_fixed.runOneTest("fixedUnparseArrayTooFewElements01") }
  @Test def test_impOptScalarThenArray03(): Unit = { runner_imp.runOneTest("impOptScalarThenArray03") }
  @Test def test_impOptArrayThenScalar03(): Unit = { runner_imp.runOneTest("impOptArrayThenScalar03") }

  @Test def test_fixedOptPresent(): Unit = { runner_fixed.runOneTest("fixedOptPresent") }
  @Test def test_fixedOptPresentArray(): Unit = { runner_fixed.runOneTest("fixedOptPresentArray") }
  @Test def test_fixedOptAbsentArray(): Unit = { runner_fixed.runOneTest("fixedOptAbsentArray") }
  @Test def test_fixedOptTwoArrays(): Unit = { runner_fixed.runOneTest("fixedOptTwoArrays") }
  @Test def test_fixedOptScalarThenArray(): Unit = { runner_fixed.runOneTest("fixedOptScalarThenArray") }
  @Test def test_fixedOptArrayThenScalar(): Unit = { runner_fixed.runOneTest("fixedOptArrayThenScalar") }

  @Test def test_impOptPresent(): Unit = { runner_imp.runOneTest("impOptPresent") }
  @Test def test_impOptPresentArray(): Unit = { runner_imp.runOneTest("impOptPresentArray") }
  @Test def test_impOptPresentArrayMax2(): Unit = { runner_imp.runOneTest("impOptPresentArrayMax2") }
  @Test def test_impOptAbsentArray(): Unit = { runner_imp.runOneTest("impOptAbsentArray") }
  @Test def test_impOptTwoArrays(): Unit = { runner_imp.runOneTest("impOptTwoArrays") }

  @Test def test_impOptScalarThenArray(): Unit = { runner_imp.runOneTest("impOptScalarThenArray") }
  @Test def test_impOptScalarThenArray02(): Unit = { runner_imp.runOneTest("impOptScalarThenArray02") }

  @Test def test_impOptArrayThenScalar(): Unit = { runner_imp.runOneTest("impOptArrayThenScalar") }
  @Test def test_impOptArrayThenScalar02(): Unit = { runner_imp.runOneTest("impOptArrayThenScalar02") }
  @Test def test_impOptArrayThenScalar02parse(): Unit = { runner_imp.runOneTest("impOptArrayThenScalar02parse") }

  @Test def test_scalarThenImpOptArray01(): Unit = { runner_imp.runOneTest("scalarThenImpOptArray01") }
  @Test def test_scalarThenImpOptArray02(): Unit = { runner_imp.runOneTest("scalarThenImpOptArray02") }
  @Test def test_scalarThenImpOptArray03(): Unit = { runner_imp.runOneTest("scalarThenImpOptArray03") }

  @Test def test_parsedOptPresent(): Unit = { runner_parsed.runOneTest("parsedOptPresent") }
  @Test def test_parsedOptPresentArray(): Unit = { runner_parsed.runOneTest("parsedOptPresentArray") }
  @Test def test_parsedOptAbsentArray(): Unit = { runner_parsed.runOneTest("parsedOptAbsentArray") }
  @Test def test_parsedOptTwoArrays(): Unit = { runner_parsed.runOneTest("parsedOptTwoArrays") }

  @Test def test_parsedOptScalarThenArray(): Unit = { runner_parsed.runOneTest("parsedOptScalarThenArray") }
  @Test def test_parsedOptScalarThenArray02(): Unit = { runner_parsed.runOneTest("parsedOptScalarThenArray02") }
  @Test def test_parsedOptArrayThenScalar(): Unit = { runner_parsed.runOneTest("parsedOptArrayThenScalar") }
  @Test def test_parsedOptArrayThenScalar02(): Unit = { runner_parsed.runOneTest("parsedOptArrayThenScalar02") }
  @Test def test_parsedOptArrayThenScalar03(): Unit = { runner_parsed.runOneTest("parsedOptArrayThenScalar03") }

  @Test def test_delimOptPresent(): Unit = { runner_delim.runOneTest("delimOptPresent") }
  @Test def test_delimOptPresentArray(): Unit = { runner_delim.runOneTest("delimOptPresentArray") }
  @Test def test_delimOptPresentArrayMax2(): Unit = { runner_delim.runOneTest("delimOptPresentArrayMax2") }
  @Test def test_delimOptAbsentArray(): Unit = { runner_delim.runOneTest("delimOptAbsentArray") }
  @Test def test_delimOptTwoArrays(): Unit = { runner_delim.runOneTest("delimOptTwoArrays") }

  @Test def test_delimOptScalarThenArray(): Unit = { runner_delim.runOneTest("delimOptScalarThenArray") }
  @Test def test_delimOptScalarThenArray02(): Unit = { runner_delim.runOneTest("delimOptScalarThenArray02") }
  @Test def test_delimOptScalarThenArray03(): Unit = { runner_delim.runOneTest("delimOptScalarThenArray03") }
  @Test def test_delimOptArrayThenScalar(): Unit = { runner_delim.runOneTest("delimOptArrayThenScalar") }
  @Test def test_delimOptArrayThenScalar02(): Unit = { runner_delim.runOneTest("delimOptArrayThenScalar02") }
  @Test def test_delimOptArrayThenScalar03(): Unit = { runner_delim.runOneTest("delimOptArrayThenScalar03") }

}
