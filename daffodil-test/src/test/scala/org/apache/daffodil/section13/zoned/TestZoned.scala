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

package org.apache.daffodil.section13.zoned

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestZoned {
  val testDir = "/org/apache/daffodil/section13/zoned/"
  val runner = Runner(testDir, "zoned.tdml")

  @AfterClass def shutdown(): Unit = {
    runner.reset
  }

}

class TestZoned {
  import TestZoned._

  @Test def testZonedPatternFail01(): Unit = { runner.runOneTest("zoned_textNumberPattern_fail01") }
  @Test def testZonedPatternFail02(): Unit = { runner.runOneTest("zoned_textNumberPattern_fail02") }
  @Test def testZonedPatternFail03(): Unit = { runner.runOneTest("zoned_textNumberPattern_fail03") }
  @Test def testZonedPatternFail04(): Unit = { runner.runOneTest("zoned_textNumberPattern_fail04") }
  @Test def testZonedPatternFail05(): Unit = { runner.runOneTest("zoned_textNumberPattern_fail05") }
  @Test def testZonedPatternFail06(): Unit = { runner.runOneTest("zoned_textNumberPattern_fail06") }
  @Test def testZonedFloatFail01(): Unit = { runner.runOneTest("zoned_float_fail01") }
  @Test def testZonedDoubleFail01(): Unit = { runner.runOneTest("zoned_double_fail01") }

  @Test def testZonedStandard01(): Unit = { runner.runOneTest("ZonedStandard01") }
  @Test def testZonedStandard02(): Unit = { runner.runOneTest("ZonedStandard02") }
  @Test def testZonedStandard03(): Unit = { runner.runOneTest("ZonedStandard03") }
  @Test def testZonedStandard04(): Unit = { runner.runOneTest("ZonedStandard04") }
  @Test def testZonedStandard05(): Unit = { runner.runOneTest("ZonedStandard05") }

  @Test def testZonedTranslatedEBCDIC01(): Unit = { runner.runOneTest("ZonedTranslatedEBCDIC01") }
  @Test def testZonedTranslatedEBCDIC02(): Unit = { runner.runOneTest("ZonedTranslatedEBCDIC02") }
  @Test def testZonedTranslatedEBCDIC03(): Unit = { runner.runOneTest("ZonedTranslatedEBCDIC03") }
  @Test def testZonedTranslatedEBCDIC04(): Unit = { runner.runOneTest("ZonedTranslatedEBCDIC04") }
  @Test def testZonedTranslatedEBCDIC05(): Unit = { runner.runOneTest("ZonedTranslatedEBCDIC05") }

  @Test def testZonedCARealiaModified01(): Unit = { runner.runOneTest("ZonedCARealiaModified01") }
  @Test def testZonedCARealiaModified02(): Unit = { runner.runOneTest("ZonedCARealiaModified02") }
  @Test def testZonedCARealiaModified03(): Unit = { runner.runOneTest("ZonedCARealiaModified03") }
  @Test def testZonedCARealiaModified04(): Unit = { runner.runOneTest("ZonedCARealiaModified04") }
  @Test def testZonedCARealiaModified05(): Unit = { runner.runOneTest("ZonedCARealiaModified05") }

  @Test def testZonedTandemModified01(): Unit = { runner.runOneTest("ZonedTandemModified01") }
  @Test def testZonedTandemModified02(): Unit = { runner.runOneTest("ZonedTandemModified02") }
  @Test def testZonedTandemModified03(): Unit = { runner.runOneTest("ZonedTandemModified03") }
  @Test def testZonedTandemModified04(): Unit = { runner.runOneTest("ZonedTandemModified04") }
  @Test def testZonedTandemModified05(): Unit = { runner.runOneTest("ZonedTandemModified05") }
}
