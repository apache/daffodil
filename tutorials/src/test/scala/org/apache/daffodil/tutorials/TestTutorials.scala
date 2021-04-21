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

package org.apache.daffodil.tutorials

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestTutorials {
  val runner1 = Runner("/", "bitorder.tutorial.tdml.xml")
  val runner4 = Runner("/", "tdmlTutorial.tdml.xml")
  val runner5 = Runner("/", "bugReportTemplate.tdml.xml")

  @AfterClass def shutDown(): Unit = {
    runner1.reset
    runner4.reset
    runner5.reset
  }

}
class TestTutorials {
  import TestTutorials._

  // removed for now. This will probably go back into this tutorial
  // @Test def test_MIL2045_47001D_1() { runner1.runOneTest("TestMIL2045_47001D_1") }
  @Test def test_leastSignificantBitFirst(): Unit = { runner1.runOneTest("leastSignificantBitFirst") }
  @Test def test_leastSignificantBitFirstRTL(): Unit = { runner1.runOneTest("leastSignificantBitFirstRTL") }
  @Test def test_mostSignificantBitFirst(): Unit = { runner1.runOneTest("mostSignificantBitFirst") }
  @Test def test_littleEndianLeastFirstLTR(): Unit = { runner1.runOneTest("littleEndianLeastFirstLTR") }
  @Test def test_littleEndianLeastFirstRTL(): Unit = { runner1.runOneTest("littleEndianLeastFirstRTL") }

  @Test def test_bugReportParse1(): Unit = { runner4.runOneTest("dateTimeTest") }
  @Test def test_bugReportUnparse1(): Unit = { runner4.runOneTest("unparseDateTimeTest") }

  @Test def test_bugReportTemplateParse1(): Unit = { runner5.runOneTest("dateTimeTest") }
  @Test def test_bugReportTemplateUnparse1(): Unit = { runner5.runOneTest("unparseDateTimeTest") }

}
