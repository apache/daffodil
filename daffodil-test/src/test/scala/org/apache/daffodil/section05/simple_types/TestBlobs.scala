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

package org.apache.daffodil.section05.simple_types

import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestBlobs {

  val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "Blobs.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestBlobs {
  import TestBlobs._

  @Test def test_blob_01(): Unit = { runner.runOneTest("blob_01") }
  @Test def test_blob_02(): Unit = { runner.runOneTest("blob_02") }
  @Test def test_blob_03(): Unit = { runner.runOneTest("blob_03") }
  @Test def test_blob_04(): Unit = { runner.runOneTest("blob_04") }
  @Test def test_blob_05(): Unit = { runner.runOneTest("blob_05") }
  @Test def test_blob_06(): Unit = { runner.runOneTest("blob_06") }
  @Test def test_blob_07(): Unit = { runner.runOneTest("blob_07") }
  @Test def test_blob_08(): Unit = { runner.runOneTest("blob_08") }
  @Test def test_blob_09(): Unit = { runner.runOneTest("blob_09") }
  @Test def test_blob_10(): Unit = { runner.runOneTest("blob_10") }
  @Test def test_blob_11(): Unit = { runner.runOneTest("blob_11") }
  @Test def test_blob_12(): Unit = { runner.runOneTest("blob_12") }
  @Test def test_blob_13(): Unit = { runner.runOneTest("blob_13") }
  @Test def test_blob_14(): Unit = { runner.runOneTest("blob_14") }
  @Test def test_blob_15(): Unit = { runner.runOneTest("blob_15") }

  @Test def test_blob_unparseError(): Unit = { runner.runOneTest("blob_unparseError") }

  @Test def test_clob_01(): Unit = { runner.runOneTest("clob_01") }

}
