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

package org.apache.daffodil.section24.regular_expressions

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestRegularExpressions extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section24/regular_expressions/RegularExpressions.tdml"
}

class TestRegularExpressions extends TdmlTests {

  val tdmlSuite = TestRegularExpressions

  @Test def entity_in_regex_fail = test
  @Test def entity_in_regex_fail_2 = test
  @Test def entity_in_regex_fail_3 = test
  @Test def entity_in_regex_fail_4 = test

  @Test def testRegEx_01 = test
  @Test def testRegEx_02 = test
  @Test def testRegEx_03 = test

  // DFDL-517
  // // Unsupported Java 7 features (should return Schema Definition Errors)
  @Ignore @Test def testRegEx_04 = test
  @Ignore @Test def testRegEx_05 = test
  @Ignore @Test def testRegEx_06 = test
  @Ignore @Test def testRegEx_07 = test

  // DFDL-922
  @Test def testDFDL_922 = test
  @Test def testDFDL_922_2 = test

  // DAFFODIL-809
  @Test def testAssertWithPattern1 = test
}
