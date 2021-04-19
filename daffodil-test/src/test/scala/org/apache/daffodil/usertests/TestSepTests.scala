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

package org.apache.daffodil.usertests

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSepTests {
  val testDir = "/org/apache/daffodil/usertests/"
  val runner = Runner(testDir, "SepTests.tdml")

  @AfterClass def shutDown: Unit = {
    runner.reset
  }

}

class TestSepTests {

  import TestSepTests._

  @Test def test_sep_trailing_1(): Unit = { runner.runOneTest("test_sep_trailing_1") }
  @Test def test_sep_anyEmpty_1(): Unit = { runner.runOneTest("test_sep_anyEmpty_1") }
  // DAFFODIL-2498 anyEmpty with minOccurs '0', and empty as first occurrence.
  @Test def test_sep_anyEmpty_2(): Unit = { runner.runOneTest("test_sep_anyEmpty_2") }

}
