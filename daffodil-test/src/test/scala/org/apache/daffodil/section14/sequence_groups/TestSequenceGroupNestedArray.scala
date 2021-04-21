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

object TestSequenceGroupNestedArray {

  val testDir_01 = "/org/apache/daffodil/section14/sequence_groups/"

  val runner = Runner(testDir_01, "SequenceGroupNestedArray.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestSequenceGroupNestedArray {

  import TestSequenceGroupNestedArray._

  @Test def test_csv_nohang_1(): Unit = { runner.runOneTest("csv_nohang_1") }
  // DAFFODIL-2487 hang when minOccurs="0"
  @Test def test_csv_hang_1(): Unit = { runner.runOneTest("csv_hang_1") }
  @Test def test_csv_hang_2(): Unit = { runner.runOneTest("csv_hang_2") }
  @Test def test_csv_hang_3(): Unit = { runner.runOneTest("csv_hang_3") }
}
