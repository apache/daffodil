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

package org.apache.daffodil.section14.sequence_groups

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSequenceGroups3 {

  val testDir_01 = "/org/apache/daffodil/section14/sequence_groups/"

  val runner_01 = Runner(testDir_01, "SequenceGroupDelimiters.tdml")
  val runner_02 = Runner(testDir_01, "SequenceGroup.tdml", validateTDMLFile = false)

  @AfterClass def shutDown(): Unit = {
    runner_01.reset
    runner_02.reset
  }

}

class TestSequenceGroups3 {
  import TestSequenceGroups3._

  @Test def test_lastElts(): Unit = { runner_01.runOneTest("lastElts") }

  @Test def test_hiddenGroupSeqWithRequiredElements(): Unit = { runner_02.runOneTest("hiddenGroupSeqWithRequiredElements") }
  @Test def test_hiddenGroupChoiceWithAllRequiredBranches(): Unit = { runner_02.runOneTest("hiddenGroupChoiceWithAllRequiredBranches") }

  @Test def test_sequence_group_with_annotation_01(): Unit = { runner_02.runOneTest("sequence_group_with_annotation_01") }
  @Test def test_choice_group_with_annotation_01(): Unit = { runner_02.runOneTest("choice_group_with_annotation_01") }

  @Test def test_similar_model_groups_01(): Unit = { runner_02.runOneTest("similar_model_groups_01") }
}
