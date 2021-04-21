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

package org.apache.daffodil.section02.processing_errors

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestProcessingErrorsUnparse {
  val testDir = "/org/apache/daffodil/section02/processing_errors/"

  val runner02 = Runner(testDir, "ProcessingErrorsUnparse.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner02Validate = Runner(testDir, "ProcessingErrorsUnparse.tdml", validateTDMLFile = true, validateDFDLSchemas = true,
    compileAllTopLevel = true)

  @AfterClass def shutDown(): Unit = {
    runner02.reset
    runner02Validate.reset
  }

}

class TestProcessingErrorsUnparse {

  import TestProcessingErrorsUnparse._

  @Test def test_roundTripErrorHalfwayThrough(): Unit = { runner02Validate.runOneTest("roundTripErrorHalfwayThrough") }

  @Test def test_upaInvalidSchemaUnparse(): Unit = { runner02Validate.runOneTest("upaInvalidSchemaUnparse") }
  @Test def test_upaInvalidSchemaUnparse2(): Unit = { runner02Validate.runOneTest("upaInvalidSchemaUnparse2") }
  @Test def test_missingNamespacePrefixUnparse(): Unit = { runner02.runOneTest("missingNamespacePrefixUnparse") }

  @Test def test_incorrectNamespaceUnparse(): Unit = { runner02.runOneTest("incorrectNamespaceUnparse") }

}
