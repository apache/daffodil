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

  @AfterClass def tearDown(): Unit = {
    runner.reset
  }

}

class TestExternalVariables {

  import TestExternalVariables._

  @Test def test_override_define_vars_01(): Unit = {
    runner.runOneTest("override_define_vars_01")
  }

  @Test def test_override_define_vars_02(): Unit = {
    runner.runOneTest("override_define_vars_02")
  }

  @Test def test_override_define_vars_04(): Unit = {
    runner.runOneTest("override_define_vars_04")
  }

  @Test def test_override_define_vars_05(): Unit = {
    runner.runOneTest("override_define_vars_05")
  }

  @Test def test_override_define_vars_06(): Unit = {
    runner.runOneTest("override_define_vars_06")
  }

  @Test def test_override_define_vars_07(): Unit = {
    runner.runOneTest("override_define_vars_07")
  }

  @Test def test_access_default_predefined_vars(): Unit = {
    runner.runOneTest("access_default_predefined_vars")
  }

  @Test def test_set_predefined_var(): Unit = {
    runner.runOneTest("set_predefined_var")
  }

  // Tests that we can specify a file in the parser
  // test case.
  @Test def test_read_config_from_file(): Unit = {
    runner.runOneTest("read_config_from_file")
  }

  /**
   * Test that a default XMLNS binding isn't present. That would be broken
   * as this schema depends on unqualified child element names
   * (elementFormDefault unqualified, which is the default)
   */
  @Test def test_testNoRootUnnecessaryBinding(): Unit = {
    runner.runOneTest("testNoRootUnnecessaryBinding")
  }
}
