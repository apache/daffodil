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

package org.apache.daffodil.section12.length_properties

import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLengthProperties {
  val testDir_02 = "/org/apache/daffodil/section12/length_properties/"

  val runner_02 = Runner(testDir_02, "LengthProperties.tdml")

  @AfterClass def shutDown(): Unit = {
    runner_02.reset
  }

}

class TestLengthProperties {

  import TestLengthProperties._

  @Test def test_LengthProp_02(): Unit = { runner_02.runOneTest("LengthProp_02") }
  @Test def test_LengthProp_charVsBytes(): Unit = { runner_02.runOneTest("LengthProp_charVsBytes") }
  @Test def test_LengthProp_charVsBytes2(): Unit = { runner_02.runOneTest("LengthProp_charVsBytes2") }
  @Test def test_LengthProp_longByteLength(): Unit = { runner_02.runOneTest("LengthProp_longByteLength") }

  @Test def test_LengthProp_04(): Unit = { runner_02.runOneTest("LengthProp_04") }
  @Test def test_LengthProp_05(): Unit = { runner_02.runOneTest("LengthProp_05") }
  @Test def test_LengthProp_06(): Unit = { runner_02.runOneTest("LengthProp_06") }

  @Test def test_LengthProp_sequenceByLength(): Unit = { runner_02.runOneTest("LengthProp_sequenceByLength") }

  @Test def test_LengthProp_tooShortFailure(): Unit = { runner_02.runOneTest("LengthProp_tooShortFailure") }
  @Test def test_LengthProp_tooLongFailure(): Unit = { runner_02.runOneTest("LengthProp_tooLongFailure") }

  @Test def test_LengthProp_zeroLength(): Unit = { runner_02.runOneTest("LengthProp_zeroLength") }
  @Test def test_LengthProp_byteLength(): Unit = { runner_02.runOneTest("LengthProp_byteLength") }
  @Test def test_LengthProp_byteLength_UTF16(): Unit = { runner_02.runOneTest("LengthProp_byteLength_UTF16") }
  @Test def test_LengthProp_byteLength_UTF16fail(): Unit = { runner_02.runOneTest("LengthProp_byteLength_UTF16fail") }

  @Test def test_LengthProp_longTextLength(): Unit = { runner_02.runOneTest("LengthProp_longTextLength") }
  @Test def test_LengthProp_lengthExpression1(): Unit = { runner_02.runOneTest("LengthProp_lengthExpression1") }

  @Test def test_LengthProp_bits_01(): Unit = { runner_02.runOneTest("LengthProp_bits_01") }
  @Test def test_LengthProp_bits_02(): Unit = { runner_02.runOneTest("LengthProp_bits_02") }

  @Test def test_OneBitLeftOver(): Unit = { runner_02.runOneTest("OneBitLeftOver") }
  @Test def test_OneBit1(): Unit = { runner_02.runOneTest("OneBit1") }
  @Test def test_ThreeBit1(): Unit = { runner_02.runOneTest("ThreeBit1") }
  @Test def test_seqBit1(): Unit = { runner_02.runOneTest("seqBit1") }
  @Test def test_seqBitLeftOver(): Unit = { runner_02.runOneTest("seqBitLeftOver") }

  @Test def test_twoByteLittleEndian(): Unit = { runner_02.runOneTest("twoByteLittleEndian") }
  @Test def test_twoByteBigEndian(): Unit = { runner_02.runOneTest("twoByteBigEndian") }

  @Test def test_bit1(): Unit = { runner_02.runOneTest("bit1") }
  @Test def test_bit2(): Unit = { runner_02.runOneTest("bit2") }
  @Test def test_bit3(): Unit = { runner_02.runOneTest("bit3") }

  @Test def test_bitsRepresentedAsText1(): Unit = { runner_02.runOneTest("bitsRepresentedAsText1") }
  @Test def test_bitsRepresentedAsText2(): Unit = { runner_02.runOneTest("bitsRepresentedAsText2") }

  @Test def test_lengthGreaterThanEight1(): Unit = { runner_02.runOneTest("lengthGreaterThanEight1") }
  @Test def test_lengthGreaterThanEight2(): Unit = { runner_02.runOneTest("lengthGreaterThanEight2") }
  @Test def test_lengthGreaterThanEight3(): Unit = { runner_02.runOneTest("lengthGreaterThanEight3") }
  @Test def test_lengthGreaterThanEight4(): Unit = { runner_02.runOneTest("lengthGreaterThanEight4") }
  @Test def test_lengthGreaterThanEight5(): Unit = { runner_02.runOneTest("lengthGreaterThanEight5") }

  @Test def test_littleEndianBits1() = {
    import org.apache.daffodil.util._
    val b1 = Bits.littleEndianBitValue(1, 10)
    val b2 = Bits.littleEndianBitValue(10, 10)
    val res = b1 + b2
    assertEquals(384, res)
  }

  @Test def test_bitUnsignedLong(): Unit = { runner_02.runOneTest("bitUnsignedLong") }
  @Test def test_bitUnsignedLong2(): Unit = { runner_02.runOneTest("bitUnsignedLong2") }
  @Test def test_bitUnsignedLong3(): Unit = { runner_02.runOneTest("bitUnsignedLong3") }
  @Test def test_bitUnsignedLong4(): Unit = { runner_02.runOneTest("bitUnsignedLong4") }
  @Test def test_bitUnsignedLong5(): Unit = { runner_02.runOneTest("bitUnsignedLong5") }
  @Test def test_bitUCombo(): Unit = { runner_02.runOneTest("bitUCombo") }
  @Test def test_bitUCombo2(): Unit = { runner_02.runOneTest("bitUCombo2") }

  @Test def test_LengthProp_bits_bool(): Unit = { runner_02.runOneTest("LengthProp_bits_bool") }
  @Test def test_LengthProp_bits_bool_false(): Unit = { runner_02.runOneTest("LengthProp_bits_bool_false") }

  @Test def test_LengthProp_leftover1(): Unit = { runner_02.runOneTest("LengthProp_leftover1") }
  @Test def test_LengthProp_leftover2(): Unit = { runner_02.runOneTest("LengthProp_leftover2") }
  @Test def test_LengthProp_leftover3(): Unit = { runner_02.runOneTest("LengthProp_leftover3") }
  @Test def test_LengthProp_leftover4(): Unit = { runner_02.runOneTest("LengthProp_leftover4") }

  //DFDL-460
  @Test def test_LengthProp_floatBits(): Unit = { runner_02.runOneTest("LengthProp_floatBits") }

  @Test def test_bitShort(): Unit = { runner_02.runOneTest("bitShort") }
  @Test def test_bitShort2(): Unit = { runner_02.runOneTest("bitShort2") }
  @Test def test_bitShort3(): Unit = { runner_02.runOneTest("bitShort3") }
  @Test def test_bitShortImplicit(): Unit = { runner_02.runOneTest("bitShortImplicit") }
  @Test def test_bitInt(): Unit = { runner_02.runOneTest("bitInt") }
  @Test def test_bitIntImplicit(): Unit = { runner_02.runOneTest("bitIntImplicit") }
  @Test def test_bitInteger(): Unit = { runner_02.runOneTest("bitInteger") }
  @Test def test_bitLong(): Unit = { runner_02.runOneTest("bitLong") }
  @Test def test_bitByte(): Unit = { runner_02.runOneTest("bitByte") }
  @Test def test_bitSignedCombo(): Unit = { runner_02.runOneTest("bitSignedCombo") }

}
