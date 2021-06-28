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

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestEscapeSchemeUnparse {
  val testDir = "/org/apache/daffodil/section07/escapeScheme/"
  val runner = Runner(testDir, "escapeSchemeUnparse.tdml")

  @AfterClass def tearDown(): Unit = {
    runner.reset
  }
}

class TestEscapeSchemeUnparse {

  import TestEscapeSchemeUnparse._

  @Test def test_unparseDelimitedEscapedString01(): Unit = { runner.runOneTest("unparseDelimitedEscapedString01") }
  @Test def test_unparseDelimitedEscapedString02(): Unit = { runner.runOneTest("unparseDelimitedEscapedString02") }
  @Test def test_unparseDelimitedEscapedString03(): Unit = { runner.runOneTest("unparseDelimitedEscapedString03") }
  @Test def test_unparseDelimitedEscapedString04(): Unit = { runner.runOneTest("unparseDelimitedEscapedString04") }
  @Test def test_unparseDelimitedEscapedString05(): Unit = { runner.runOneTest("unparseDelimitedEscapedString05") }
  @Test def test_unparseDelimitedEscapedString06(): Unit = { runner.runOneTest("unparseDelimitedEscapedString06") }
  @Test def test_unparseDelimitedEscapedString07(): Unit = { runner.runOneTest("unparseDelimitedEscapedString07") }
  @Test def test_unparseDelimitedEscapedString08(): Unit = { runner.runOneTest("unparseDelimitedEscapedString08") }
  @Test def test_unparseDelimitedEscapedString09(): Unit = { runner.runOneTest("unparseDelimitedEscapedString09") }
  @Test def test_unparseDelimitedEscapedString10(): Unit = { runner.runOneTest("unparseDelimitedEscapedString10") }
  @Test def test_unparseDelimitedEscapedString12(): Unit = { runner.runOneTest("unparseDelimitedEscapedString12") }
  @Test def test_unparseDelimitedEscapedString13(): Unit = { runner.runOneTest("unparseDelimitedEscapedString13") }
  @Test def test_unparseDelimitedEscapedString14(): Unit = { runner.runOneTest("unparseDelimitedEscapedString14") }

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
  @Test def test_unparseDelimitedEscapedString15(): Unit = { runner.runOneTest("unparseDelimitedEscapedString15") }
  @Test def test_unparseDelimitedEscapedString16(): Unit = { runner.runOneTest("unparseDelimitedEscapedString16") }
  @Test def test_unparseDelimitedEscapedString17(): Unit = { runner.runOneTest("unparseDelimitedEscapedString17") }
  @Test def test_unparseDelimitedEscapedString18(): Unit = { runner.runOneTest("unparseDelimitedEscapedString18") }
  @Test def test_unparseDelimitedEscapedString19(): Unit = { runner.runOneTest("unparseDelimitedEscapedString19") }
  @Test def test_unparseDelimitedEscapedString20(): Unit = { runner.runOneTest("unparseDelimitedEscapedString20") }
  @Test def test_unparseDelimitedEscapedString21(): Unit = { runner.runOneTest("unparseDelimitedEscapedString21") }
  @Test def test_unparseDelimitedEscapedString22(): Unit = { runner.runOneTest("unparseDelimitedEscapedString22") }

  @Test def test_parseDelimitedEscapedString01(): Unit = { runner.runOneTest("parseDelimitedEscapedString01") }
  @Test def test_parseDelimitedEscapedString03(): Unit = { runner.runOneTest("parseDelimitedEscapedString03") }
  @Test def test_parseDelimitedEscapedString04(): Unit = { runner.runOneTest("parseDelimitedEscapedString04") }

  @Test def test_runtimeUnparseDelimiterEscapeConflict(): Unit = { runner.runOneTest("runtimeUnparseDelimiterEscapeConflict") }

  @Test def test_unparseInvalidExtraEscapedCharacters(): Unit = { runner.runOneTest("unparseInvalidExtraEscapedCharacters") }
}
