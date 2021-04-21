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
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestPropertyScoping {
  val testDir = "/org/apache/daffodil/section08/property_scoping/"
  val runner = Runner(testDir, "PropertyScoping.tdml")
  val runner_01 = Runner(testDir, "PropertyScoping_01.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner_01.reset
  }
}

class TestPropertyScoping {

  import TestPropertyScoping._

  @Test def test_defaultForm_01(): Unit = { runner.runOneTest("defaultForm_01") }
  @Test def test_defaultForm_02(): Unit = { runner.runOneTest("defaultForm_02") }
  @Test def test_defaultForm_03(): Unit = { runner.runOneTest("defaultForm_03") }
  @Test def test_defaultForm_04(): Unit = { runner.runOneTest("defaultForm_04") }

  @Test def test_localAnnotation_01(): Unit = { runner.runOneTest("localAnnotation_01") }
  @Test def test_localAnnotation_02(): Unit = { runner.runOneTest("localAnnotation_02") }
  @Test def test_localAnnotation_03(): Unit = { runner.runOneTest("localAnnotation_03") }
  @Test def test_localAnnotation_04(): Unit = { runner.runOneTest("localAnnotation_04") }
  @Test def test_localAnnotation_05(): Unit = { runner.runOneTest("localAnnotation_05") }

  @Test def test_property_scoping_01(): Unit = { runner.runOneTest("property_scoping_01") }
  @Test def test_unparse_property_scoping_01(): Unit = { runner.runOneTest("unparse_property_scoping_01") }
  @Test def test_property_scoping_06(): Unit = { runner.runOneTest("property_scoping_06") }
  @Test def test_unparse_property_scoping_06(): Unit = { runner.runOneTest("unparse_property_scoping_06") }
  @Test def test_group_ref(): Unit = { runner.runOneTest("group_ref") }
  @Test def test_multipleDefinition(): Unit = { runner.runOneTest("multipleDefinition") }
  @Test def test_multipleDefinition2(): Unit = { runner.runOneTest("multipleDefinition2") }
  @Test def test_multipleDefinition3(): Unit = { runner.runOneTest("multipleDefinition3") }

  @Test def test_format_nesting_01(): Unit = { runner.runOneTest("format_nesting_01") }

  @Test def test_property_scoping_02(): Unit = { runner_01.runOneTest("property_scoping_02") }
  @Test def test_unparse_property_scoping_02(): Unit = { runner_01.runOneTest("unparse_property_scoping_02") }
  @Test def test_property_scoping_03(): Unit = { runner_01.runOneTest("property_scoping_03") }
  @Test def test_unparse_property_scoping_03(): Unit = { runner_01.runOneTest("unparse_property_scoping_03") }
  @Test def test_property_scoping_04(): Unit = { runner_01.runOneTest("property_scoping_04") }
  @Test def test_property_scoping_05(): Unit = { runner_01.runOneTest("property_scoping_05") }
  @Test def test_unparse_property_scoping_04(): Unit = { runner_01.runOneTest("unparse_property_scoping_04") }
  @Test def test_unparse_property_scoping_05(): Unit = { runner_01.runOneTest("unparse_property_scoping_05") }
  @Test def test_property_scoping_07(): Unit = { runner_01.runOneTest("property_scoping_07") }
  @Test def test_unparse_property_scoping_07(): Unit = { runner_01.runOneTest("unparse_property_scoping_07") }
  @Test def test_property_scoping_08(): Unit = { runner_01.runOneTest("property_scoping_08") }
  @Test def test_unparse_property_scoping_08(): Unit = { runner_01.runOneTest("unparse_property_scoping_08") }
  @Test def test_property_scoping_09(): Unit = { runner_01.runOneTest("property_scoping_09") }
  @Test def test_unparse_property_scoping_09(): Unit = { runner_01.runOneTest("unparse_property_scoping_09") }
  @Test def test_property_scoping_10(): Unit = { runner_01.runOneTest("property_scoping_10") }
  @Test def test_unparse_property_scoping_10(): Unit = { runner_01.runOneTest("unparse_property_scoping_10") }
  @Test def test_property_scoping_11(): Unit = { runner_01.runOneTest("property_scoping_11") }
  @Test def test_unparse_property_scoping_11(): Unit = { runner_01.runOneTest("unparse_property_scoping_11") }
  @Test def test_unparse_property_scoping_12(): Unit = { runner_01.runOneTest("unparse_property_scoping_12") }
  @Test def test_NearestEnclosingSequenceElementRef(): Unit = { runner_01.runOneTest("NearestEnclosingSequenceElementRef") }

  @Test def test_property_refElementFormFail(): Unit = { runner_01.runOneTest("refElementFormFail") }

}
