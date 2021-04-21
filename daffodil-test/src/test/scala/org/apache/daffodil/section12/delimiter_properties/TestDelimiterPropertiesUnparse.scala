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

package org.apache.daffodil.section12.delimiter_properties

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestDelimiterPropertiesUnparse {

  val testDir_02 = "/org/apache/daffodil/section12/delimiter_properties/"
  val runner = Runner(testDir_02, "DelimiterPropertiesUnparse.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestDelimiterPropertiesUnparse {

  import TestDelimiterPropertiesUnparse._

  @Test def test_unparseSeparatorLeadingSpace(): Unit = { runner.runOneTest("unparseSeparatorLeadingSpace") }

  //DFDL-1493, DFDL-1477
  //@Test def test_unparseMultipleInitiators04() { runner.runOneTest("unparseMultipleInitiators04") }
  //@Test def test_unparseMultipleInitiators06() { runner.runOneTest("unparseMultipleInitiators06") }

  @Test def test_unparseMultipleInitiators05(): Unit = { runner.runOneTest("unparseMultipleInitiators05") }
  @Test def test_unparseMultipleTerminators03(): Unit = { runner.runOneTest("unparseMultipleTerminators03") }

  @Test def test_unparseMultipleInitiators01(): Unit = { runner.runOneTest("unparseMultipleInitiators01") }
  @Test def test_unparseMultipleInitiators02(): Unit = { runner.runOneTest("unparseMultipleInitiators02") }
  @Test def test_unparseMultipleInitiators03(): Unit = { runner.runOneTest("unparseMultipleInitiators03") }
  @Test def test_unparseMultipleInitiators07(): Unit = { runner.runOneTest("unparseMultipleInitiators07") }

  @Test def test_unparseMultipleTerminators01(): Unit = { runner.runOneTest("unparseMultipleTerminators01") }
  @Test def test_unparseMultipleTerminators02(): Unit = { runner.runOneTest("unparseMultipleTerminators02") }
  @Test def test_unparseMultipleTerminators04(): Unit = { runner.runOneTest("unparseMultipleTerminators04") }
  @Test def test_unparseMultipleTerminators05(): Unit = { runner.runOneTest("unparseMultipleTerminators05") }

  @Test def test_unparseMultipleSeparators01(): Unit = { runner.runOneTest("unparseMultipleSeparators01") }
  @Test def test_unparseMultipleSeparators02(): Unit = { runner.runOneTest("unparseMultipleSeparators02") }
  @Test def test_unparseMultipleSeparators03(): Unit = { runner.runOneTest("unparseMultipleSeparators03") }

}
