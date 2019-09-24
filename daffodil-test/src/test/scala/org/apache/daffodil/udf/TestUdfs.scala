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
package org.apache.daffodil.udf

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestUdfs {
  val testDir = "/org/apache/daffodil/udf/"

  val runner = Runner(testDir, "udfs.tdml", validateTDMLFile = true)

  @AfterClass def shutDown {
    runner.reset
  }

}

class TestUdfs {
  import TestUdfs._

  @Test def test_udf_undeclaredNamespace() { runner.runOneTest("udf_undeclaredNamespace") }
  @Test def test_udf_fnNotFound() { runner.runOneTest("udf_fnNotFound") }

}