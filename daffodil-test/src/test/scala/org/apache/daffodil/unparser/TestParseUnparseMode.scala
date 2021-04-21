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

package org.apache.daffodil.unparser

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestParseUnparseMode {
  val testDir = "/org/apache/daffodil/unparser/"
  val runner = Runner(testDir, "parseUnparseModeTest.tdml",
    validateDFDLSchemas = false) // there are UPA errors in some test schemas

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestParseUnparseMode {

  import TestParseUnparseMode._

  @Test def test_parse1(): Unit = { runner.runOneTest("parse1") }

  @Test def test_unparse1(): Unit = { runner.runOneTest("unparse1") }

  @Test def test_unparse2(): Unit = { runner.runOneTest("unparse2") }

  @Test def test_unparse3(): Unit = { runner.runOneTest("unparse3") }
}
