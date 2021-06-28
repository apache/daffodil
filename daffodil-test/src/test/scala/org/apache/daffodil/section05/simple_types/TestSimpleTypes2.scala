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

package org.apache.daffodil.section05.simple_types

import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSimpleTypes2 {

  val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runnerAL = Runner(testDir, "AL.tdml")
  val runnerWS = Runner(testDir, "WhiteSpace.tdml")
  val runnerAJ = Runner(testDir, "AJ.tdml")
  val runnerAK = Runner(testDir, "AK.tdml")
  val runner1 = Runner(testDir, "BitOrder.tdml")
  val runnerST = Runner(testDir, "simple-type-bases.tdml")

  val runner = Runner(testDir, "SimpleTypes.tdml")
  val runner2 = Runner(testDir, "SimpleTypes2.tdml")

  @AfterClass def shutDown(): Unit = {
    runnerAL.reset
    runnerWS.reset
    runnerAJ.reset
    runnerAK.reset
    runner1.reset
    runnerST.reset
    runner.reset
    runner2.reset
  }
}

class TestSimpleTypes2 {
  import TestSimpleTypes2._

  @Test def test_AL000(): Unit = { runnerAL.runOneTest("AL000") }

  @Test def test_AJ000(): Unit = { runnerAJ.runOneTest("AJ000") }
  @Test def test_AJ001(): Unit = { runnerAJ.runOneTest("AJ001") }

  @Test def test_AK000(): Unit = { runnerAK.runOneTest("AK000") }
  @Test def test_AK001(): Unit = { runnerAK.runOneTest("AK001") }

  @Test def test_redefinedFormat(): Unit = { runnerWS.runOneTest("redefinedFormat") }
  @Test def test_whiteSpaceBeforeLax(): Unit = { runnerWS.runOneTest("whiteSpaceBeforeLax") }
  @Test def test_whiteSpaceAfterLax(): Unit = { runnerWS.runOneTest("whiteSpaceAfterLax") }
  @Test def test_whiteSpaceDuringLax(): Unit = { runnerWS.runOneTest("whiteSpaceDuringLax") }
  @Test def test_whiteSpaceBeforeStrict(): Unit = { runnerWS.runOneTest("whiteSpaceBeforeStrict") }
  @Test def test_whiteSpaceDuringStrict(): Unit = { runnerWS.runOneTest("whiteSpaceDuringStrict") }
  @Test def test_whiteSpaceAfterStrict(): Unit = { runnerWS.runOneTest("whiteSpaceAfterStrict") }

  // THIS TEST won't round trip until encoding for 7-bit ascii is implemented
  // Currently this is set to roundTrip="false"
  @Test def test_MIL2045_47001D_Page70_TableB_I_with_string(): Unit = { runner1.runOneTest("TestMIL2045_47001D_Page70_TableB_I_with_string") }
  @Test def test_MIL2045_47001D_1(): Unit = { runner1.runOneTest("TestMIL2045_47001D_1") }
  @Test def test_LSBFirstSpan3Bytes(): Unit = { runner1.runOneTest("TestLSBFirstSpan3Bytes") }
  @Test def test_leastSignificantBitFirst(): Unit = { runner1.runOneTest("leastSignificantBitFirst") }
  @Test def test_leastSigBitFirstFloat(): Unit = { runner1.runOneTest("leastSigBitFirstFloat") }
  @Test def test_leastSigBitFirstDouble(): Unit = { runner1.runOneTest("leastSigBitFirstDouble") }
  @Test def test_leastSignificantBitFirstRTL(): Unit = { runner1.runOneTest("leastSignificantBitFirstRTL") }
  @Test def test_mostSignificantBitFirst1(): Unit = { runner1.runOneTest("mostSignificantBitFirst1") }
  @Test def test_mostSigBitFirstFloat(): Unit = { runner1.runOneTest("mostSigBitFirstFloat") }
  @Test def test_mostSigBitFirstDouble(): Unit = { runner1.runOneTest("mostSigBitFirstDouble") }
  @Test def test_littleEndianLeastFirstLTR(): Unit = { runner1.runOneTest("littleEndianLeastFirstLTR") }
  @Test def test_littleEndianLeastFirstRTL(): Unit = { runner1.runOneTest("littleEndianLeastFirstRTL") }
  @Test def test_bitOrderChangeInvalid2(): Unit = { runner1.runOneTest("bitOrderChangeInvalid2") }
  @Test def test_bitOrderChangeInvalid2Unparser(): Unit = { runner1.runOneTest("bitOrderChangeInvalid2Unparser") }
  @Test def test_noByteOrder(): Unit = { runner1.runOneTest("noByteOrder") }

  // DAFFODIL-897
  //@Test def test_bitOrderChange() { runner1.runOneTest("bitOrderChange") }
  //@Test def test_bitOrderDocument() { runner1.runOneTest("bitOrderDocument") }
  //@Test def test_bitOrderTypeByte() { runner1.runOneTest("bitOrderTypeByte") }
  //@Test def test_bitOrderChangeInvalid3() { runner1.runOneTest("bitOrderChangeInvalid3") }
  //@Test def test_bitOrderChangeInvalid() { runner3.runOneTest("bitOrderChangeInvalid") }

  // DAFFODIL-1001 fixed.
  @Test def test_bigEndianLeastFirst(): Unit = { runner1.runOneTest("bigEndianLeastFirst") }


  @Test def test_simpleTypeDerivedFromPrimType(): Unit = { runnerST.runOneTest("simpleTypeDerivedFromPrimType") }
  @Test def test_simpleTypeChainedDerivations(): Unit = { runnerST.runOneTest("simpleTypeChainedDerivations") }
  @Test def test_simpleTypeOverlapPrimError(): Unit = { runnerST.runOneTest("simpleTypeOverlapPrimError") }
  @Test def test_simpleTypeOverlapSimpleTypeError(): Unit = { runnerST.runOneTest("simpleTypeOverlapSimpleTypeError") }

  @Test def test_mostSigBitFirstLEFloat(): Unit = { runner1.runOneTest("mostSigBitFirstLEFloat") }
  @Test def test_mostSigBitFirstLEDouble(): Unit = { runner1.runOneTest("mostSigBitFirstLEDouble") }

  @Test def test_hexBinary_Delimited_04(): Unit = { runner.runOneTest("hexBinary_Delimited_04") }

  @Test def test_restrictionBaseEmbeddedLeadlingSpace(): Unit = { runner.runOneTest("restrictionBaseEmbeddedLeadingSpace") }
  @Test def test_restrictionBaseEmbeddedTrailingSpace(): Unit = { runner.runOneTest("restrictionBaseEmbeddedTrailingSpace") }

  // DAFFODIL-2204
  @Test def test_terminatorErrorMessage(): Unit = { runner2.runOneTest("terminatorErrorMessage") }
}
