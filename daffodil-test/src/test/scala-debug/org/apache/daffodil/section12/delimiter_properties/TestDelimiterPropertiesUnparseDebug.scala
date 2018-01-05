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
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite

object TestDelimiterPropertiesUnparseDebug {

  val testDir_02 = "/org/apache/daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterPropertiesUnparse.tdml"
  var runner = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  @AfterClass def shutDown {
    runner = null
  }

}

class TestDelimiterPropertiesUnparseDebug {

  import TestDelimiterPropertiesUnparseDebug._

  //DFDL-1287, DFDL-947
  @Test def test_unparseSeparatorLeadingSpace() { runner.runOneTest("unparseSeparatorLeadingSpace") }

  //DFDL-1213
  @Test def test_unparseMultipleInitiators04() { runner.runOneTest("unparseMultipleInitiators04") }
  @Test def test_unparseMultipleInitiators05() { runner.runOneTest("unparseMultipleInitiators05") }
  @Test def test_unparseMultipleInitiators06() { runner.runOneTest("unparseMultipleInitiators06") }
  @Test def test_unparseMultipleTerminators03() { runner.runOneTest("unparseMultipleTerminators03") }

}
