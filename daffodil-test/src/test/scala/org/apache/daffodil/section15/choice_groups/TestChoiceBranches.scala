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

package org.apache.daffodil.section15.choice_groups

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestChoiceBranches extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section15/choice_groups/ChoiceBranches.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestChoiceBranches extends TdmlTests {

  val tdmlSuite = TestChoiceBranches

  @Test def choiceBranch_e1(): Unit = test
  @Test def choiceBranch_e2(): Unit = test
  @Test def choiceBranch_e3(): Unit = test
  @Test def choiceBranch_e1_req(): Unit = test
  @Test def choiceBranch_e2_req(): Unit = test
  @Test def choiceBranch_e3_req(): Unit = test
  @Test def choiceBranch_e1_opt(): Unit = test
  @Test def choiceBranch_e2_opt(): Unit = test
  @Test def choiceBranch_e3_opt(): Unit = test
  @Test def choiceBranch_e1_reqElements(): Unit = test
  @Test def choiceBranch_e2_reqElements(): Unit = test
  @Test def choiceBranch_e3_reqElements(): Unit = test
}
