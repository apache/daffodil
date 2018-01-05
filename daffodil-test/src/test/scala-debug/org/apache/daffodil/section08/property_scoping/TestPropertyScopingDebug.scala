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

package org.apache.daffodil.section08.property_scoping

import org.junit.Test
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestPropertyScopingDebug {
  val testDir = "/org/apache/daffodil/section08/property_scoping/"
  val runner = Runner(testDir, "PropertyScoping.tdml")
  val runner_01 = Runner(testDir, "PropertyScoping_01.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner_01.reset
  }
}

class TestPropertyScopingDebug {

  import TestPropertyScopingDebug._

  //DFDL-1036 (was fixed) now DFDL-1159
  @Test def test_localAnnotation_05() { runner.runOneTest("localAnnotation_05") }

  val tdml = testDir + "PropertyScoping_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  // See DFDL-1337
  @Test def test_unparse_property_scoping_02() { runner_01.runOneTest("unparse_property_scoping_02") }
  // See DFDL-1342
  @Test def test_unparse_property_scoping_04() { runner_01.runOneTest("unparse_property_scoping_04") }
  @Test def test_unparse_property_scoping_05() { runner_01.runOneTest("unparse_property_scoping_05") }
}
