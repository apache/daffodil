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

package org.apache.daffodil.section07.external_variables

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestExternalVariables {

  val testDir = "/org/apache/daffodil/section07/external_variables/"
  val runner = Runner(testDir, "external_variables.tdml")

  @AfterClass def tearDown() {
    runner.reset
  }

}

class TestExternalVariables {

  import TestExternalVariables._

  // It's important to note here that external variables
  // via the TDMLRunner are currently passed in during
  // compilation.
  @Test def test_override_define_vars_01() {
    runner.runOneTest("override_define_vars_01")
  }

  @Test def test_override_define_vars_02() {
    runner.runOneTest("override_define_vars_02")
  }

  @Test def test_override_define_vars_03() {
    runner.runOneTest("override_define_vars_03")
  }

  @Test def test_override_define_vars_04() {
    runner.runOneTest("override_define_vars_04")
  }

  @Test def test_override_define_vars_05() {
    runner.runOneTest("override_define_vars_05")
  }

  @Test def test_access_default_predefined_vars() {
    runner.runOneTest("access_default_predefined_vars")
  }

  @Test def test_set_a_predefined_var() {
    runner.runOneTest("set_predefined_var")
  }

}
