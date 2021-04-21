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
package org.apache.daffodil.extensions

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestChoiceBranchKeyRanges {
  val testDir = "/org/apache/daffodil/extensions/choiceBranchRanges/"

  val runner = Runner(testDir, "choiceBranchKeyRanges.tdml", validateTDMLFile = true)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestChoiceBranchKeyRanges {
  import TestChoiceBranchKeyRanges._

  @Test def test_choiceBranchKeyRanges_01(): Unit = { runner.runOneTest("choiceBranchKeyRanges_01") }
  @Test def test_choiceBranchKeyRanges_overlap_01(): Unit = { runner.runOneTest("choiceBranchKeyRanges_overlap_01") }
  @Test def test_choiceBranchKeyRanges_overlap_02(): Unit = { runner.runOneTest("choiceBranchKeyRanges_overlap_02") }
  @Test def test_choiceBranchKeyRanges_overlap_03(): Unit = { runner.runOneTest("choiceBranchKeyRanges_overlap_03") }
  @Test def test_choiceBranchKeyRanges_oddLength_01(): Unit = { runner.runOneTest("choiceBranchKeyRanges_oddLength_01") }
  @Test def test_choiceBranchKeyRanges_badOrder_01(): Unit = { runner.runOneTest("choiceBranchKeyRanges_badOrder_01") }
  @Test def test_choiceBranchKeyRanges_nonintDispatch_01(): Unit = { runner.runOneTest("choiceBranchKeyRanges_nonintDispatch_01") }
  @Test def test_choiceBranchKeyRanges_nonintDispatch_02(): Unit = { runner.runOneTest("choiceBranchKeyRanges_nonintDispatch_02") }
}