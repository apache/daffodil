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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestChoiceBranchKeyRanges extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/extensions/choiceBranchRanges/choiceBranchKeyRanges.tdml"
}

class TestChoiceBranchKeyRanges extends TdmlTests {
  val tdmlSuite = TestChoiceBranchKeyRanges

  @Test def choiceBranchKeyRanges_01 = test
  @Test def choiceBranchKeyRanges_overlap_01 = test
  @Test def choiceBranchKeyRanges_overlap_02 = test
  @Test def choiceBranchKeyRanges_overlap_03 = test
  @Test def choiceBranchKeyRanges_oddLength_01 = test
  @Test def choiceBranchKeyRanges_badOrder_01 = test
  @Test def choiceBranchKeyRanges_nonintDispatch_01 = test
  @Test def choiceBranchKeyRanges_nonintDispatch_02 = test
}
