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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestExternalVariables extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/external_variables/external_variables.tdml"
}

class TestExternalVariables extends TdmlTests {
  val tdmlSuite = TestExternalVariables

  @Test def override_define_vars_01 = test
  @Test def override_define_vars_02 = test
  @Test def override_define_vars_04 = test
  @Test def override_define_vars_05 = test
  @Test def override_define_vars_06 = test
  @Test def override_define_vars_07 = test

  @Test def access_default_predefined_vars = test

  @Test def set_predefined_var = test

  // Tests that we can specify a file in the parser
  // test case.
  @Test def read_config_from_file = test

  /**
   * Test that a default XMLNS binding isn't present. That would be broken
   * as this schema depends on unqualified child element names
   * (elementFormDefault unqualified, which is the default)
   */
  @Test def testNoRootUnnecessaryBinding = test
}
