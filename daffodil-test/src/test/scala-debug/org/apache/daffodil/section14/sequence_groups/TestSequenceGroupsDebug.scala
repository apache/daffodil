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

object TestSequenceGroupsDebug {

  val testDir_01 = "/org/apache/daffodil/section14/sequence_groups/"

  val runner_01 = Runner(testDir_01, "SequenceGroupDelimiters.tdml")
  var runner_02 = Runner(testDir_01, "SequenceGroup.tdml", validateTDMLFile = false)

  @AfterClass def shutDown {
    runner_01.reset
    runner_02.reset
  }

}

class TestSequenceGroupsDebug {

  import TestSequenceGroupsDebug._

  //DFDL-284
  @Test def test_hiddenGroupLoop() { runner_02.runOneTest("hiddenGroupLoop") }

  @Test def test_emptySequenceSDE() { runner_02.runOneTest("emptySequenceSDE") }

}
