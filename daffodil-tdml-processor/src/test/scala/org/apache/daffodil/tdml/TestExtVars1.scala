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

package org.apache.daffodil.tdml

import org.junit.Test
import org.junit.AfterClass

object TestExtVars1 {
  val testDir = "org/apache/daffodil/tdml/"
  val runner = Runner(testDir, "testExtVars1.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestExtVars1 {
  import TestExtVars1._

  @Test def test_configLittleEndian() = { runner.runOneTest("configLittleEndian") }
  @Test def test_unparseConfigLittleEndian() = { runner.runOneTest("unparseConfigLittleEndian") }
  @Test def test_unparseConfigBigEndian() = { runner.runOneTest("unparseConfigBigEndian") }
  @Test def test_configDefaultBigEndian() = { runner.runOneTest("configDefaultBigEndian") }
  @Test def test_unparseConfigDefaultBigEndian() = { runner.runOneTest("unparseConfigDefaultBigEndian") }

}

