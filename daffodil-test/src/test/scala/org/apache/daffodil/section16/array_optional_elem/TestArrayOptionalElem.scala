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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestArrayOptionalElem extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section16/array_optional_elem/ArrayOptionalElem.tdml"
}

object TestBacktracking extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section16/array_optional_elem/backtracking.tdml"
}

object TestArrayComb extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section16/array_optional_elem/ArrayComb.tdml"
}

object TestFacets extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/facets/Facets.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestArrayOptionalElem extends TdmlTests {
  val tdmlSuite = TestArrayOptionalElem

  @Test def arrayExpressions01 = test
  @Test def arrayExpressions02 = test
  @Test def arrayExpressions02b = test
  @Test def arrayExpressions02c = test
  @Test def arrayExpressions02d = test
  @Test def arrayExpressions03 = test
  @Test def arrayExpressions04 = test
  @Test def arrayExpressions05 = test

  @Test def error01 = test
  @Test def postfixNoErr = test

  @Test def optionalElem = test
  @Test def optionalWithSeparators = test
  @Test def Lesson6_optional_element = test
  @Test def Lesson6_optional_element_01 = test
  @Test def Lesson6_fixed_array = test
  @Test def Lesson6_variable_array = test
  @Test def Lesson6_variable_array_01 = test
  @Test def Lesson6_variable_array_02 = test

  @Test def occursCountKindImplicitSeparators01a = test
  @Test def occursCountKindImplicitSeparators01b = test
  @Test def occursCountKindImplicitSeparators02 = test
  @Test def occursCountKindImplicitSeparators03 = test
  @Test def occursCountKindImplicitSeparators04 = test
  @Test def occursCountKindImplicitSeparators05 = test
  @Test def occursCountKindImplicitSeparators05Strict = test
  @Test def occursCountKindImplicitSeparatorsUnparser = test

  @Test def ambigSep1 = test
  @Test def ambigSep2 = test

  // DAFFODIL-1886
  @Test def manyAdjacentOptionals_01 = test

  // DAFFODIL-2263
  @Test def dfdl2263 = test
}

class TestBacktracking extends TdmlTests {
  val tdmlSuite = TestBacktracking

  @Test def backtrack1Text = test
}

class TestArrayComb extends TdmlTests {
  val tdmlSuite = TestArrayComb

  // DAFFODIL-1964
  @Test def arrayComb1 = test
  @Test def arrayComb2 = test
}

class TestFacets extends TdmlTests {
  val tdmlSuite = TestFacets

  @Test def leftOverData_Neg = test
}
