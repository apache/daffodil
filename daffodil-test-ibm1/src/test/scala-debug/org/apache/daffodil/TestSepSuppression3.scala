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

package org.apache.daffodil

import org.apache.daffodil.tdml.Runner
import org.junit.Test

object TestSepSuppression3 {
  val testDir = "/test-suite/tresys-contributed/"
  lazy val runner = Runner(testDir, "sepSuppression.tdml")
  lazy val runner2 = Runner(testDir, "sepSuppression2.tdml")
}

class TestSepSuppression3 {
  import TestSepSuppression3._

  // These do not work on Daffodil
  // Unable to suppress separator before a potentiallyTrailingGroup that
  // is not present.
  //
  @Test def test_ptLax2u_daf() = { runner2.trace.runOneTest("ptLax2u_daf") }

  // These unparser tests also do not work on Daffodil
  //
  // Some due to lack of output of a separator for a
  // non-existing but positional element - the separator is required
  // to hold the place for it even though the element is not present
  // in the infoset.
  //
  // Others due to lack of ability to suppress separator for empty
  // potentially trailing group.
  //
  // Or both
  //
  @Test def test_ptg1_2u_daf() = { runner.trace.runOneTest("ptg1_2u_daf") }
  @Test def test_ptg1_3u_daf() = { runner.trace.runOneTest("ptg1_3u_daf") }
  @Test def test_ptg1_4u_daf() = { runner.trace.runOneTest("ptg1_4u_daf") }
  @Test def test_ptg1_5u_daf() = { runner.trace.runOneTest("ptg1_5u_daf") }
  @Test def test_ptg1_6u_daf() = { runner.trace.runOneTest("ptg1_6u_daf") }

  @Test def test_ptg2_1u_daf() = { runner.trace.runOneTest("ptg2_1u_daf") }

  @Test def test_ptg3_1u_daf() = { runner.trace.runOneTest("ptg3_1u_daf") }

  // IBM fails this test because its parser suppresses an empty string
  // in a case where the element is not optional. It is suppressing the
  // element for an empty string even though the element is not an optional/array.
  @Test def test_ptg3_1p_ibm() = { runner.trace.runOneTest("ptg3_1p_ibm") }

  // IBM fails this test because it cannot determine that a CHOICE
  // is a potentially trailing group.
  @Test def test_ptg2_1p_ibm() = { runner.trace.runOneTest("ptg2_1p_ibm") }

  // IBM fails this test because it cannot determine that a CHOICE
  // is a potentially trailing group, and therefore none of its contents
  // may be present. In general it cannot figure out because no element that
  // is within the choice is in the infoset, so it cannot resolve the choice.
  @Test def test_ptg2_1u_ibm() = { runner.trace.runOneTest("ptg2_1u_ibm") }
}
