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

package org.apache.daffodil.section07.variables

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestVariablesDebug {
  val testDir = "/org/apache/daffodil/section07/variables/"
  val runner = Runner(testDir, "variables.tdml")
  val runner_01 = Runner(testDir, "variables_01.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner_01.reset
  }

}

class TestVariablesDebug {

  import TestVariablesDebug._

  @Test def test_varInstance() { runner.runOneTest("varInstance") }

  // DFDL-1443 & DFDL-1448 - workaround for too-early variable evaluation
  @Test def test_setAfterReadErr_d() { runner_01.runOneTest("setAfterReadErr_d") }
  @Test def test_setAfterReadErr() { runner.runOneTest("setAfterReadErr") }

}
