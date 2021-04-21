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

package org2

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestOrg2 {
  val runner = Runner("org2", "testSchemaFilesUnderSrcTest2.tdml")
  val runner2 = Runner("org2", "testEmbeddedSchema.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner2.reset
  }
}

class TestOrg2 {

  import TestOrg2._

  @Test def test_schemaFilesUnderSrcTest_02() = { runner.runOneTest("test_schemaFilesUnderSrcTest_02") }
  @Test def test_embeddedSchema_01() = { runner2.runOneTest("test_embeddedSchema_01") }

}
