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

package org.apache.daffodil.section07.assertions

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestAssertionsDebug {
  val testDir = "/org/apache/daffodil/section07/assertions/"
  val runner = Runner(testDir, "assert.tdml", validateTDMLFile = false)

  @AfterClass def tearDown {
    runner.reset
  }

}

class TestAssertionsDebug {

  import TestAssertionsDebug._

  //DFDL-474
  @Test def test_assertExpressionEmpty() { runner.runOneTest("assertExpressionEmpty") }

  // DFDL-1043
  @Test def test_assertFailShowsValue2() { runner.runOneTest("assertFailShowsValue2") }
  
  //DFDL-1210: This test should be removed, an assert failing will no longer show the value
  @Test def test_assertFailShowsValue() { runner.runOneTest("assertFailShowsValue") }

  //DFDL-2001
  @Test def test_testPatternX() { runner.runOneTest("testPatternX") }
  @Test def test_testPatternUnicode() { runner.runOneTest("testPatternUnicode") }

}
