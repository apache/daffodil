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

import org.junit.Test

object TestUnparseArrayOptionalElemFixed extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section16/array_optional_elem/UnparseArrayFixedOptionalElem.tdml"
}

object TestUnparseArrayOptionalElemImplicit extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section16/array_optional_elem/UnparseArrayImplicitOptionalElem.tdml"
}

object TestUnparseArrayOptionalElemParsed extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section16/array_optional_elem/UnparseArrayParsedOptionalElem.tdml"
}

object TestUnparseArrayOptionalElemDelimited extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section16/array_optional_elem/UnparseArrayDelimitedOptionalElem.tdml"
}

object TestUnparseArrayExpressionConst extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section16/array_optional_elem/UnparseArrayExpressionConstant.tdml"
}

class TestUnparseArrayOptionalElemFixed extends TdmlTests {
  val tdmlSuite = TestUnparseArrayOptionalElemFixed

  @Test def fixedUnparseArrayTooManyElements01 = test
  @Test def fixedUnparseArrayTooFewElements01 = test
  @Test def fixedOptPresent = test
  @Test def fixedOptPresentArray = test
  @Test def fixedOptAbsentArray = test
  @Test def fixedOptTwoArrays = test
  @Test def fixedOptScalarThenArray = test
  @Test def fixedOptArrayThenScalar = test
}

class TestUnparseArrayOptionalElemImplicit extends TdmlTests {
  val tdmlSuite = TestUnparseArrayOptionalElemImplicit

  @Test def impOptScalarThenArray03 = test
  @Test def impOptArrayThenScalar03 = test

  @Test def impOptPresent = test
  @Test def impOptPresentArray = test
  @Test def impOptPresentArrayMax2 = test
  @Test def impOptAbsentArray = test
  @Test def impOptTwoArrays = test

  @Test def impOptScalarThenArray = test
  @Test def impOptScalarThenArray02 = test

  @Test def impOptArrayThenScalar = test
  @Test def impOptArrayThenScalar02 = test
  @Test def impOptArrayThenScalar02parse = test

  @Test def scalarThenImpOptArray01 = test
  @Test def scalarThenImpOptArray02 = test
  @Test def scalarThenImpOptArray03 = test
}

class TestUnparseArrayOptionalElemParsed extends TdmlTests {
  val tdmlSuite = TestUnparseArrayOptionalElemParsed

  @Test def parsedOptPresent = test
  @Test def parsedOptPresentArray = test
  @Test def parsedOptAbsentArray = test
  @Test def parsedOptTwoArrays = test

  @Test def parsedOptScalarThenArray = test
  @Test def parsedOptScalarThenArray02 = test
  @Test def parsedOptArrayThenScalar = test
  @Test def parsedOptArrayThenScalar02 = test
  @Test def parsedOptArrayThenScalar03 = test
}

class TestUnparseArrayOptionalElemDelimited extends TdmlTests {
  val tdmlSuite = TestUnparseArrayOptionalElemDelimited

  @Test def delimOptPresent = test
  @Test def delimOptPresentArray = test
  @Test def delimOptPresentArrayMax2 = test
  @Test def delimOptAbsentArray = test
  @Test def delimOptTwoArrays = test

  @Test def delimOptScalarThenArray = test
  @Test def delimOptScalarThenArray02 = test
  @Test def delimOptScalarThenArray03 = test
  @Test def delimOptArrayThenScalar = test
  @Test def delimOptArrayThenScalar02 = test
  @Test def delimOptArrayThenScalar03 = test
}

class TestUnparseArrayExpressionConst extends TdmlTests {
  val tdmlSuite = TestUnparseArrayExpressionConst

  @Test def exprOptPresent = test
  @Test def exprOptPresentArray = test
  @Test def exprOptAbsentArray = test
  @Test def exprOptTwoArrays = test
  @Test def exprOptScalarThenArray = test
  @Test def exprOptArrayThenScalar = test

  @Test def exprOptParsedData_01 = test
  @Test def exprOptParsedData_02 = test
  @Test def exprOptParsedData_03 = test
  @Test def exprOptParsedData_04 = test
  @Test def exprReqAbsentArray_01 = test
}
