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

package org.apache.daffodil.section06.namespaces

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestNamespacesDebug {
  val testDir = "/org/apache/daffodil/section06/namespaces/"

  val runner = Runner(testDir, "namespaces.tdml", validateTDMLFile = true, validateDFDLSchemas = false)
  val runner2 = Runner(testDir, "multiFile.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner3 = Runner(testDir, "includeImport.tdml")
  val runnerWithSchemaValidation = Runner(testDir, "multiFile.tdml", validateTDMLFile = true, validateDFDLSchemas = true)

  @AfterClass def shutDown {
    runner.reset
    runner2.reset
    runner3.reset
    runnerWithSchemaValidation.reset
  }
}

class TestNamespacesDebug {

  import TestNamespacesDebug._

  @Test def test_multi_encoding_04() { runner.runOneTest("multi_encoding_04") }
  @Test def test_multi_encoding_05() { runner.runOneTest("multi_encoding_05") }
  @Test def test_indexOutOfBounds_01() { runner.runOneTest("indexOutOfBounds_01") }

  // DFDL-1204 - this test no longer works. New loader won't accept character U+00B7 as a character
  // in a prefix name.
  @Test def test_namespaceSpecialChars() { runner.runOneTest("namespaceSpecialChars") }

  // DFDL-1663
  @Test def test_namespaceLimitParse() { runner.runOneTest("namespaceLimitParse") }
  @Test def test_namespaceLimitUnparse() { runner.runOneTest("namespaceLimitUnparse") }
}
