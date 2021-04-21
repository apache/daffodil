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
import org.junit.AfterClass

object TestArrayOptionalElem {
  private val testDir = "/org/apache/daffodil/section16/array_optional_elem/"
  private val testDir01 = "/org/apache/daffodil/section05/facets/"

  val runner = Runner(testDir, "ArrayOptionalElem.tdml")
  val runner01 = Runner(testDir01, "Facets.tdml", validateTDMLFile = false)
  val rBack = Runner(testDir, "backtracking.tdml")
  val runnerAC = Runner(testDir, "ArrayComb.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner01.reset
    rBack.reset
    runnerAC.reset
  }

}

class TestArrayOptionalElem {

  import TestArrayOptionalElem._

  // DAFFODIL-1964
  @Test def test_arrayComb1() = { runnerAC.runOneTest("arrayComb1") }
  @Test def test_arrayComb2() = { runnerAC.runOneTest("arrayComb2") }

  @Test def test_arrayExpressions01(): Unit = { runner.runOneTest("arrayExpressions01") }
  @Test def test_arrayExpressions02(): Unit = { runner.runOneTest("arrayExpressions02") }
  @Test def test_arrayExpressions02b(): Unit = { runner.runOneTest("arrayExpressions02b") }
  @Test def test_arrayExpressions02c(): Unit = { runner.runOneTest("arrayExpressions02c") }
  @Test def test_arrayExpressions02d(): Unit = { runner.runOneTest("arrayExpressions02d") }
  @Test def test_arrayExpressions03(): Unit = { runner.runOneTest("arrayExpressions03") }
  @Test def test_arrayExpressions04(): Unit = { runner.runOneTest("arrayExpressions04") }
  @Test def test_arrayExpressions05(): Unit = { runner.runOneTest("arrayExpressions05") }

  @Test def test_error01(): Unit = { runner.runOneTest("error01") }
  @Test def test_postfixNoErr(): Unit = { runner.runOneTest("postfixNoErr") }

  @Test def test_optionalElem(): Unit = { runner.runOneTest("optionalElem") }
  @Test def test_optionalWithSeparators(): Unit = { runner.runOneTest("optionalWithSeparators") }
  @Test def test_Lesson6_optional_element(): Unit = { runner.runOneTest("Lesson6_optional_element") }
  @Test def test_Lesson6_optional_element_01(): Unit = { runner.runOneTest("Lesson6_optional_element_01") }
  @Test def test_Lesson6_fixed_array(): Unit = { runner.runOneTest("Lesson6_fixed_array") }
  @Test def test_Lesson6_variable_array(): Unit = { runner.runOneTest("Lesson6_variable_array") }
  @Test def test_Lesson6_variable_array_01(): Unit = { runner.runOneTest("Lesson6_variable_array_01") }
  @Test def test_Lesson6_variable_array_02(): Unit = { runner.runOneTest("Lesson6_variable_array_02") }

  @Test def test_leftOverData_Neg(): Unit = { runner01.runOneTest("leftOverData_Neg") }

  @Test def test_backtrack1Text() = { rBack.runOneTest("backtrack1Text") }

  @Test def test_occursCountKindImplicitSeparators01a(): Unit = { runner.runOneTest("occursCountKindImplicitSeparators01a") }
  @Test def test_occursCountKindImplicitSeparators01b(): Unit = { runner.runOneTest("occursCountKindImplicitSeparators01b") }
  @Test def test_occursCountKindImplicitSeparators02(): Unit = { runner.runOneTest("occursCountKindImplicitSeparators02") }
  @Test def test_occursCountKindImplicitSeparators03(): Unit = { runner.runOneTest("occursCountKindImplicitSeparators03") }
  @Test def test_occursCountKindImplicitSeparators04(): Unit = { runner.runOneTest("occursCountKindImplicitSeparators04") }
  @Test def test_occursCountKindImplicitSeparators05(): Unit = { runner.runOneTest("occursCountKindImplicitSeparators05") }
  @Test def test_occursCountKindImplicitSeparators05Strict(): Unit = { runner.runOneTest("occursCountKindImplicitSeparators05Strict") }
  @Test def test_occursCountKindImplicitSeparatorsUnparser(): Unit = { runner.runOneTest("occursCountKindImplicitSeparatorsUnparser") }

  @Test def test_ambigSep1(): Unit = { runner.runOneTest("ambigSep1") }
  @Test def test_ambigSep2(): Unit = { runner.runOneTest("ambigSep2") }

  // DAFFODIL-1886
  @Test def test_manyAdjacentOptionals_01(): Unit = { runner.runOneTest("manyAdjacentOptionals_01") }

  // DAFFODIL-2263
  @Test def test_dfdl2263(): Unit = { runner.runOneTest("dfdl2263") }

}
