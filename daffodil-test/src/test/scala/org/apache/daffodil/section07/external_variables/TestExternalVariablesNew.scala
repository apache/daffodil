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

import org.apache.daffodil.tdml.DFDLTestSuite
import org.junit.Test
import org.apache.daffodil.util._

class TestExternalVariablesNew {

  val testDir = "/org/apache/daffodil/section07/external_variables/"
  val tdml = testDir + "external_variables.tdml"

  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  // It's important to note here that external variables
  // via the TDMLRunner are currently passed in during
  // compilation.

  // Tests that we can specify a file in the parser
  // test case.
  @Test def test_read_config_from_file() {
    runner.runOneTest("read_config_from_file")
  }

  /**
   * Test that a default XMLNS binding isn't present. That would be broken
   * as this schema depends on unqualified child element names
   * (elementFormDefault unqualified, which is the default)
   */
  @Test
  def test_testNoRootUnnecessaryBinding(): Unit = {
    runner.trace.runOneTest("testNoRootUnnecessaryBinding")
  }
}
