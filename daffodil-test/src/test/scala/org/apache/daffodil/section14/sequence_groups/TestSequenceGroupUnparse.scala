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

object TestSequenceGroupUnparse {
  val runner = Runner("/org/apache/daffodil/section14/sequence_groups/SequenceGroupUnparse.tdml")
  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}
class TestSequenceGroupUnparse {
  import TestSequenceGroupUnparse._

  @Test def test_seqWithOptionals1(): Unit = { runner.runOneTest("seqWithOptionals1") }
  @Test def test_seqWithOptionals2(): Unit = { runner.runOneTest("seqWithOptionals2") }
  @Test def test_seqWithOptionals3(): Unit = { runner.runOneTest("seqWithOptionals3") }
  @Test def test_seqWithOptionals4(): Unit = { runner.runOneTest("seqWithOptionals4") }
  @Test def test_seqWithOptionals5(): Unit = { runner.runOneTest("seqWithOptionals5") }

  @Test def test_seqWithHiddenGroupContainingComplex(): Unit = { runner.runOneTest("seqWithHiddenGroupContainingComplex") }

}
