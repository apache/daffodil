/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.section05.simple_types

import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSimpleTypes2 {

  val testDir = "/org/apache/daffodil/section05/simple_types/"
  val testDir_01 = "/org/apache/daffodil/ibm-tests/"

  val runnerAL = Runner(testDir, "AL.tdml")
  val runner2 = Runner(testDir, "WhiteSpace.tdml")
  val runnerAJ = Runner(testDir, "AJ.tdml")
  val runnerAK = Runner(testDir, "AK.tdml")
  val runner1 = Runner(testDir, "BitOrder.tdml")
  val runnerST = Runner(testDir, "simple-type-bases.tdml")
  val runner_01 = Runner(testDir_01, "dpaext1.tdml")

  val runner = Runner(testDir, "SimpleTypes.tdml")

  @AfterClass def shutDown() {
    runnerAL.reset
    runner2.reset
    runnerAJ.reset
    runnerAK.reset
    runner_01.reset
    runner1.reset
    runnerST.reset
    runner.reset
  }
}

class TestSimpleTypes2 {
  import TestSimpleTypes2._

  @Test def test_AL000() { runnerAL.runOneTest("AL000") }

  @Test def test_AJ000() { runnerAJ.runOneTest("AJ000") }
  @Test def test_AJ001() { runnerAJ.runOneTest("AJ001") }

  @Test def test_AK000() { runnerAK.runOneTest("AK000") }
  @Test def test_AK001() { runnerAK.runOneTest("AK001") }

  @Test def test_schema_types_5_01() { runner_01.runOneTest("schema_types_5_01") }
  @Test def test_schema_types_5_02() { runner_01.runOneTest("schema_types_5_02") }
  @Test def test_schema_types_5_03() { runner_01.runOneTest("schema_types_5_03") }
  @Test def test_schema_types_5_04() { runner_01.runOneTest("schema_types_5_04") }
  @Test def test_schema_types_5_05() { runner_01.runOneTest("schema_types_5_05") }

  @Test def test_whiteSpaceBeforeLax() { runner2.runOneTest("whiteSpaceBeforeLax") }
  @Test def test_whiteSpaceDuringLax() { runner2.runOneTest("whiteSpaceDuringLax") }
  @Test def test_whiteSpaceBeforeStrict() { runner2.runOneTest("whiteSpaceBeforeStrict") }
  @Test def test_whiteSpaceDuringStrict() { runner2.runOneTest("whiteSpaceDuringStrict") }
  @Test def test_whiteSpaceAfterStrict() { runner2.runOneTest("whiteSpaceAfterStrict") }

  // THIS TEST won't round trip until encoding for 7-bit ascii is implemented
  // Currently this is set to roundTrip="false"
  @Test def test_MIL2045_47001D_Page70_TableB_I_with_string() { runner1.runOneTest("TestMIL2045_47001D_Page70_TableB_I_with_string") }
  @Test def test_MIL2045_47001D_1() { runner1.runOneTest("TestMIL2045_47001D_1") }
  @Test def test_LSBFirstSpan3Bytes() { runner1.runOneTest("TestLSBFirstSpan3Bytes") }
  @Test def test_leastSignificantBitFirst() { runner1.runOneTest("leastSignificantBitFirst") }
  @Test def test_leastSigBitFirstFloat() { runner1.runOneTest("leastSigBitFirstFloat") }
  @Test def test_leastSigBitFirstDouble() { runner1.runOneTest("leastSigBitFirstDouble") }
  @Test def test_leastSignificantBitFirstRTL() { runner1.runOneTest("leastSignificantBitFirstRTL") }
  @Test def test_mostSignificantBitFirst1() { runner1.runOneTest("mostSignificantBitFirst1") }
  @Test def test_mostSigBitFirstFloat() { runner1.runOneTest("mostSigBitFirstFloat") }
  @Test def test_mostSigBitFirstDouble() { runner1.runOneTest("mostSigBitFirstDouble") }
  @Test def test_littleEndianLeastFirstLTR() { runner1.runOneTest("littleEndianLeastFirstLTR") }
  @Test def test_littleEndianLeastFirstRTL() { runner1.runOneTest("littleEndianLeastFirstRTL") }
  @Test def test_bitOrderChangeInvalid2() { runner1.runOneTest("bitOrderChangeInvalid2") }
  @Test def test_bitOrderChangeInvalid2Unparser() { runner1.runOneTest("bitOrderChangeInvalid2Unparser") }
  @Test def test_noByteOrder() { runner1.runOneTest("noByteOrder") }

  @Test def test_simpleTypeDerivedFromPrimType() { runnerST.runOneTest("simpleTypeDerivedFromPrimType") }
  @Test def test_simpleTypeChainedDerivations() { runnerST.runOneTest("simpleTypeChainedDerivations") }
  @Test def test_simpleTypeOverlapPrimError() { runnerST.runOneTest("simpleTypeOverlapPrimError") }
  @Test def test_simpleTypeOverlapSimpleTypeError() { runnerST.runOneTest("simpleTypeOverlapSimpleTypeError") }

  @Test def test_mostSigBitFirstLEFloat() { runner1.runOneTest("mostSigBitFirstLEFloat") }
  @Test def test_mostSigBitFirstLEDouble() { runner1.runOneTest("mostSigBitFirstLEDouble") }

  @Test def test_hexBinary_Delimited_04() { runner.runOneTest("hexBinary_Delimited_04") }

}
