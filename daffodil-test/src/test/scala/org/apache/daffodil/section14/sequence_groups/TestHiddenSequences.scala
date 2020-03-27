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

object TestHiddenSequences {

  val testDir = "/org/apache/daffodil/section14/sequence_groups/"

  val runner = Runner(testDir, "HiddenSequences.tdml", validateTDMLFile = true)

  @AfterClass def shutDown {
    runner.reset
  }

}

class TestHiddenSequences {

  import TestHiddenSequences._


  @Test def test_parseHiddenGroupRef() { runner.runOneTest("parseHiddenGroupRef") }
  @Test def test_parseRegularGroupRef() { runner.runOneTest("parseRegularGroupRef") }
  @Test def test_parseSeqOfHiddenAndRegularRef() { runner.runOneTest("parseSeqOfHiddenAndRegularRef") }
  @Test def test_parseNestedHiddenAndRegularRef() { runner.runOneTest("parseNestedHiddenAndRegularRef") }
  @Test def test_parseNestedRegularAndHiddenRef() { runner.runOneTest("parseNestedRegularAndHiddenRef") }
  @Test def test_parseNestedHiddenGroupRefs() { runner.runOneTest("parseNestedHiddenGroupRefs") }

  @Test def test_unparseHiddenGroupRef() { runner.runOneTest("unparseHiddenGroupRef") }
  @Test def test_unparseRegularGroupRef() { runner.runOneTest("unparseRegularGroupRef") }
  @Test def test_unparseSeqOfHiddenAndRegularRef() { runner.runOneTest("unparseSeqOfHiddenAndRegularRef") }
  @Test def test_unparseNestedHiddenAndRegularRef() { runner.runOneTest("unparseNestedHiddenAndRegularRef") }
  @Test def test_unparseNestedRegularAndHiddenRef() { runner.runOneTest("unparseNestedRegularAndHiddenRef") }
  @Test def test_unparseNestedHiddenGroupRefs() { runner.runOneTest("unparseNestedHiddenGroupRefs") }
  @Test def test_noOVCinHiddenContext() { runner.runOneTest("noOVCinHiddenContext") }
  @Test def test_nestedNoOVCinHiddenContext() { runner.runOneTest("nestedNoOVCinHiddenContext") }

  @Test def test_invalidGroupDefWithHiddenSequenceModelGroup() { runner.runOneTest("invalidGroupDefWithHiddenSequenceModelGroup") }
}
