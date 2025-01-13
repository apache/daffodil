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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestOrg2NonEmbedded extends TdmlSuite {
  val tdmlResource = "/org2/testSchemaFilesUnderSrcTest2.tdml"
}

object TestOrg2Embedded extends TdmlSuite {
  val tdmlResource = "/org2/testEmbeddedSchema.tdml"
}

class TestOrg2NonEmbedded extends TdmlTests {
  val tdmlSuite = TestOrg2NonEmbedded

  @Test def test_schemaFilesUnderSrcTest_02 = test
}

class TestOrg2Embedded extends TdmlTests {
  val tdmlSuite = TestOrg2Embedded

  @Test def test_embeddedSchema_01 = test
}
