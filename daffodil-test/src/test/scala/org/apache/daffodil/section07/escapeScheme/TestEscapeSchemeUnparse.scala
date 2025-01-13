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

package org.apache.daffodil.section07.escapeScheme

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestEscapeSchemeUnparse extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/escapeScheme/escapeSchemeUnparse.tdml"
}

class TestEscapeSchemeUnparse extends TdmlTests {
  val tdmlSuite = TestEscapeSchemeUnparse

  @Test def unparseDelimitedEscapedString01 = test
  @Test def unparseDelimitedEscapedString02 = test
  @Test def unparseDelimitedEscapedString03 = test
  @Test def unparseDelimitedEscapedString04 = test
  @Test def unparseDelimitedEscapedString05 = test
  @Test def unparseDelimitedEscapedString06 = test
  @Test def unparseDelimitedEscapedString07 = test
  @Test def unparseDelimitedEscapedString08 = test
  @Test def unparseDelimitedEscapedString09 = test
  @Test def unparseDelimitedEscapedString10 = test
  @Test def unparseDelimitedEscapedString12 = test
  @Test def unparseDelimitedEscapedString13 = test
  @Test def unparseDelimitedEscapedString14 = test

  /*
   * The following tests demonstrate that for extraEscapedCharacters during Unparsing that:
   *
   * 1. DFDL Character Class entities are not allowed
   * 2. DFDL raw byte entities are not allowed
   * 3. DFDL hex entities are allowed
   * 4. DFDL basic entities are allowed (like SP, VT, etc)
   * 5. DFDL decimal entities are allowed
   * 6. When an extra escaped character is not present, the text is not escaped.
   * */
  @Test def unparseDelimitedEscapedString15 = test
  @Test def unparseDelimitedEscapedString16 = test
  @Test def unparseDelimitedEscapedString17 = test
  @Test def unparseDelimitedEscapedString18 = test
  @Test def unparseDelimitedEscapedString19 = test
  @Test def unparseDelimitedEscapedString20 = test
  @Test def unparseDelimitedEscapedString21 = test
  @Test def unparseDelimitedEscapedString22 = test

  @Test def parseDelimitedEscapedString01 = test
  @Test def parseDelimitedEscapedString03 = test
  @Test def parseDelimitedEscapedString04 = test

  @Test def runtimeUnparseDelimiterEscapeConflict = test

  @Test def unparseInvalidExtraEscapedCharacters = test
}
