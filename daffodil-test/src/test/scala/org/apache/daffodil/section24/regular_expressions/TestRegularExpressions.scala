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

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestRegularExpressions {
  val testDir = "/org/apache/daffodil/section24/regular_expressions/"
  val runner = Runner(testDir, "RegularExpressions.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestRegularExpressions {

  import TestRegularExpressions._

  @Test def test_entity_in_regex_fail(): Unit = { runner.runOneTest("entity_in_regex_fail") }
  @Test def test_entity_in_regex_fail_2(): Unit = { runner.runOneTest("entity_in_regex_fail_2") }
  @Test def test_entity_in_regex_fail_3(): Unit = { runner.runOneTest("entity_in_regex_fail_3") }
  @Test def test_entity_in_regex_fail_4(): Unit = { runner.runOneTest("entity_in_regex_fail_4") }

  @Test def test_testRegEx_01(): Unit = { runner.runOneTest("testRegEx_01") }
  @Test def test_testRegEx_02(): Unit = { runner.runOneTest("testRegEx_02") }
  @Test def test_testRegEx_03(): Unit = { runner.runOneTest("testRegEx_03") }

  // DFDL-517
  // // Unsupported Java 7 features (should return Schema Definition Errors)
  // @Test def test_testRegEx_04() { runner.runOneTest("testRegEx_04") }
  // @Test def test_testRegEx_05() { runner.runOneTest("testRegEx_05") }
  // @Test def test_testRegEx_06() { runner.runOneTest("testRegEx_06") }
  // @Test def test_testRegEx_07() { runner.runOneTest("testRegEx_07") }

  // DFDL-922
  @Test def test_testRegEx_08(): Unit = { runner.runOneTest("testDFDL-922") }
  @Test def test_testRegEx_09(): Unit = { runner.runOneTest("testDFDL-922_2") }

  // DAFFODIL-809
  @Test def test_assertWithPattern1(): Unit = { runner.runOneTest("testAssertWithPattern1") }
}
