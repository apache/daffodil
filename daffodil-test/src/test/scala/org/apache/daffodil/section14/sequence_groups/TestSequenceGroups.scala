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

object TestSequenceGroups {

  val testDir_01 = "/org/apache/daffodil/section14/sequence_groups/"

  val runner_01 = Runner(testDir_01, "SequenceGroupDelimiters.tdml")
  val runner_02 = Runner(testDir_01, "SequenceGroup.tdml", validateTDMLFile = false)

  @AfterClass def shutDown(): Unit = {
    runner_01.reset
    runner_02.reset
  }

}

class TestSequenceGroups {

  import TestSequenceGroups._




  @Test def test_SeqGrp_01(): Unit = { runner_01.runOneTest("SeqGrp_01") }
  @Test def test_SeqGrp_02(): Unit = { runner_01.runOneTest("SeqGrp_02") }
  @Test def test_SeqGrp_03(): Unit = { runner_01.runOneTest("SeqGrp_03") }
  @Test def test_SeqGrp_04(): Unit = { runner_01.runOneTest("SeqGrp_04") }
  @Test def test_prefix(): Unit = { runner_01.runOneTest("prefix") }
  @Test def test_prefix_01(): Unit = { runner_01.runOneTest("prefix_01") }
  @Test def test_NumSeq_02(): Unit = { runner_01.runOneTest("NumSeq_02") }
  @Test def test_groupRefInheritProps(): Unit = { runner_01.runOneTest("groupRefInheritProps") }
  @Test def test_sequenceWithinSequence(): Unit = { runner_01.runOneTest("sequenceWithinSequence") }
  @Test def test_delimitedByNextInitFail(): Unit = { runner_01.runOneTest("delimitedByNextInitFail") }

  // DAFFODIL-669
  //  @Test def test_emptySequenceSDE() { runner_02.runOneTest("emptySequenceSDE") }
  @Test def test_NadaParser(): Unit = { runner_02.runOneTest("nadaParser") }
  @Test def test_complexEmptyContent(): Unit = { runner_02.runOneTest("complexEmptyContent") }
  @Test def test_noContentComplexSDE(): Unit = { runner_02.runOneTest("noContentComplexSDE") }
  @Test def test_noContentAnnotatedComplexSDE(): Unit = { runner_02.runOneTest("noContentAnnotatedComplexSDE") }

  @Test def test_SeqGrp546(): Unit = { runner_02.runOneTest("SeqGrp546") }

  @Test def test_SeqGrp_05(): Unit = { runner_02.runOneTest("SeqGrp_05") }

  @Test def test_hiddenGroup1(): Unit = { runner_02.runOneTest("hiddenGroup1") }
  @Test def test_hiddenGroupSchemaFail(): Unit = { runner_02.runOneTest("hiddenGroupSchemaFail") }
  @Test def test_hiddenGroupWithAssert(): Unit = { runner_02.runOneTest("hiddenGroupWithAssert") }
  @Test def test_hiddenGroupWithAssert2(): Unit = { runner_02.runOneTest("hiddenGroupWithAssert2") }
  @Test def test_hiddenGroupNested(): Unit = { runner_02.runOneTest("hiddenGroupNested") }
  @Test def test_hiddenGroupNested2(): Unit = { runner_02.runOneTest("hiddenGroupNested2") }
  @Test def test_nestedGroupRefs(): Unit = { runner_02.runOneTest("nestedGroupRefs") }
  @Test def test_nestedGroupRefs2(): Unit = { runner_02.runOneTest("nestedGroupRefs2") }
  @Test def test_hiddenGroupChoice(): Unit = { runner_02.runOneTest("hiddenGroupChoice") }
  @Test def test_hiddenGroupChoice2(): Unit = { runner_02.runOneTest("hiddenGroupChoice2") }
  @Test def test_hiddenGroupIgnoredProps(): Unit = { runner_02.runOneTest("hiddenGroupIgnoredProps") }
  @Test def test_hiddenGroupAttributeNotation(): Unit = { runner_02.runOneTest("hiddenGroupAttributeNotation") }
  @Test def test_hiddenGroupElementNotation(): Unit = { runner_02.runOneTest("hiddenGroupElementNotation") }

  //DFDL-284
  // @Test def test_hiddenGroupLoop() { runner_02.runOneTest("hiddenGroupLoop") }

  //DFDL-598(related to, but this test does not say this is fixed)
  @Test def test_hiddenGroupRefEmptyString(): Unit = { runner_02.runOneTest("hiddenGroupRefEmptyString") }
  @Test def test_hiddenGroupRefDoesNotExist(): Unit = { runner_02.runOneTest("hiddenGroupRefDoesNotExist") }

  @Test def test_AC000(): Unit = { runner_02.runOneTest("AC000") }
  @Test def test_AD000(): Unit = { runner_02.runOneTest("AD000") }
  @Test def test_AS000(): Unit = { runner_02.runOneTest("AS000") }

  @Test def test_noDefaultSeqKind(): Unit = { runner_02.runOneTest("noDefaultSeqKind") }
  @Test def test_sequenceWithComplexType(): Unit = { runner_02.runOneTest("sequenceWithComplexType") }

  // DAFFODIL-2171
  @Test def test_delimiterScanning_01(): Unit = { runner_01.runOneTest("delimiterScanning_01") }
  @Test def test_delimiterScanning_02(): Unit = { runner_01.runOneTest("delimiterScanning_02") }
  //@Test def test_delimiterScanning_03() { runner_01.runOneTest("delimiterScanning_03") }
}
