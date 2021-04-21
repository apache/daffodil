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
package org.apache.daffodil.extensions

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestLookAhead {
  val testDir = "/org/apache/daffodil/extensions/lookAhead/"

  val runner = Runner(testDir, "lookAhead.tdml", validateTDMLFile = true)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestLookAhead {
  import TestLookAhead._

  @Test def test_lookAhead_01(): Unit = { runner.runOneTest("lookAhead_01") }
  @Test def test_lookAhead_02(): Unit = { runner.runOneTest("lookAhead_02") }
  @Test def test_lookAhead_03(): Unit = { runner.runOneTest("lookAhead_03") }
  @Test def test_lookAhead_04(): Unit = { runner.runOneTest("lookAhead_04") }
  @Test def test_lookAhead_05(): Unit = { runner.runOneTest("lookAhead_05") }
  @Test def test_lookAhead_06(): Unit = { runner.runOneTest("lookAhead_06") }
  @Test def test_lookAhead_tooFar_01(): Unit = { runner.runOneTest("lookAhead_tooFar_01") }
  @Test def test_lookAhead_tooFar_02(): Unit = { runner.runOneTest("lookAhead_tooFar_02") }
  @Test def test_lookAhead_tooFar_03(): Unit = { runner.runOneTest("lookAhead_tooFar_03") }
  @Test def test_lookAhead_negativeOffset_01(): Unit = { runner.runOneTest("lookAhead_negativeOffset_01") }
  @Test def test_lookAhead_negativeBitsize_01(): Unit = { runner.runOneTest("lookAhead_negativeBitsize_01") }
  @Test def test_lookAhead_zeroBitsize_01(): Unit = { runner.runOneTest("lookAhead_zeroBitsize_01") }
  @Test def test_lookAhead_newVariableInstance_01(): Unit = { runner.runOneTest("lookAhead_newVariableInstance_01") }
  @Test def test_lookAhead_setVariable_01(): Unit = { runner.runOneTest("lookAhead_setVariable_01") }
}
