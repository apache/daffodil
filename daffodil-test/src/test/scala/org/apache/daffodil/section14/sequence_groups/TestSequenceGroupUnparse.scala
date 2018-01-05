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
import org.apache.daffodil.util.Misc
import org.apache.daffodil.tdml.DFDLTestSuite
import org.junit.AfterClass

object TestSequenceGroupUnparse {
  val testDir = "org/apache/daffodil/section14/sequence_groups/"
  val aa = testDir + "SequenceGroupUnparse.tdml"
  val res = Misc.getRequiredResource(aa)
  var runner = new DFDLTestSuite(res)
  @AfterClass def shutDown {
    runner = null
  }
}
class TestSequenceGroupUnparse {
  import TestSequenceGroupUnparse._

  @Test def test_seqWithOptionals1() { runner.runOneTest("seqWithOptionals1") }
  @Test def test_seqWithOptionals2() { runner.runOneTest("seqWithOptionals2") }
  @Test def test_seqWithOptionals3() { runner.runOneTest("seqWithOptionals3") }
  @Test def test_seqWithOptionals4() { runner.runOneTest("seqWithOptionals4") }
  @Test def test_seqWithOptionals5() { runner.runOneTest("seqWithOptionals5") }

  @Test def test_seqWithHiddenGroupContainingComplex() { runner.runOneTest("seqWithHiddenGroupContainingComplex") }

}
