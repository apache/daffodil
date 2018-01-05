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

package org.apache.daffodil.section12.aligned_data

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestAlignedDataDebug {
  val testDir_01 = "/org/apache/daffodil/section12/aligned_data/"
  val runner1 = Runner(testDir_01, "Aligned_Data.tdml")
  val runner2 = Runner(testDir_01, "BinaryInput_01.tdml")

  @AfterClass def shutDown {
    runner1.reset
    runner2.reset
  }
}

class TestAlignedDataDebug {

  import TestAlignedDataDebug._

  // DFDL-1217
  @Test def test_alignmentOptionalElem03() = { runner1.runOneTest("alignmentOptionalElem03") }
}
