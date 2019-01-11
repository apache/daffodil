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

package org.apache.daffodil.section02.schema_definition_errors

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSDE {

  val testDir = "/org/apache/daffodil/section02/schema_definition_errors/"
  val runner = Runner(testDir, "SchemaDefinitionErrors.tdml")

  @AfterClass def tearDown() {
    runner.reset
  }

}

class TestSDE {

  import TestSDE._

  @Test def test_AS000_rev() { runner.runOneTest("AS000_rev") }

  //  @Test def test_schema_component_err() { runner.runOneTest("schema_component_err") }

  @Test def test_schema_line_number() { runner.runOneTest("schema_line_number") }
  @Test def test_schema_warning() { runner.runOneTest("schema_warning") }
  @Test def test_missing_appinfo_source() { runner.runOneTest("missing_appinfo_source") }
  @Test def test_missing_closing_tag() { runner.runOneTest("missing_closing_tag") }
  @Test def test_ignoreAttributeFormDefault() { runner.runOneTest("ignoreAttributeFormDefault") }
}
