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

  @AfterClass def shutDown {
    runner_02.reset
  }

}

class TestLengthProperties {

  import TestLengthProperties._

  @Test def test_LengthProp_02() { runner_02.runOneTest("LengthProp_02") }
  @Test def test_LengthProp_charVsBytes() { runner_02.runOneTest("LengthProp_charVsBytes") }
  @Test def test_LengthProp_charVsBytes2() { runner_02.runOneTest("LengthProp_charVsBytes2") }
  @Test def test_LengthProp_longByteLength() { runner_02.runOneTest("LengthProp_longByteLength") }

  @Test def test_LengthProp_04() { runner_02.runOneTest("LengthProp_04") }
  @Test def test_LengthProp_05() { runner_02.runOneTest("LengthProp_05") }
  @Test def test_LengthProp_06() { runner_02.runOneTest("LengthProp_06") }

  @Test def test_LengthProp_sequenceByLength() { runner_02.runOneTest("LengthProp_sequenceByLength") }

  @Test def test_LengthProp_tooShortFailure() { runner_02.runOneTest("LengthProp_tooShortFailure") }
  @Test def test_LengthProp_tooLongFailure() { runner_02.runOneTest("LengthProp_tooLongFailure") }

  @Test def test_LengthProp_zeroLength() { runner_02.runOneTest("LengthProp_zeroLength") }
  @Test def test_LengthProp_byteLength() { runner_02.runOneTest("LengthProp_byteLength") }
  @Test def test_LengthProp_byteLength_UTF16() { runner_02.runOneTest("LengthProp_byteLength_UTF16") }
  @Test def test_LengthProp_byteLength_UTF16fail() { runner_02.runOneTest("LengthProp_byteLength_UTF16fail") }

  @Test def test_LengthProp_longTextLength() { runner_02.runOneTest("LengthProp_longTextLength") }
  @Test def test_LengthProp_lengthExpression1() { runner_02.runOneTest("LengthProp_lengthExpression1") }

  @Test def test_LengthProp_bits_01() { runner_02.runOneTest("LengthProp_bits_01") }
  @Test def test_LengthProp_bits_02() { runner_02.runOneTest("LengthProp_bits_02") }

  @Test def test_OneBitLeftOver() { runner_02.runOneTest("OneBitLeftOver") }
  @Test def test_OneBit1() { runner_02.runOneTest("OneBit1") }
  @Test def test_ThreeBit1() { runner_02.runOneTest("ThreeBit1") }
  @Test def test_seqBit1() { runner_02.runOneTest("seqBit1") }
  @Test def test_seqBitLeftOver() { runner_02.runOneTest("seqBitLeftOver") }

  @Test def test_twoByteLittleEndian() { runner_02.runOneTest("twoByteLittleEndian") }
  @Test def test_twoByteBigEndian() { runner_02.runOneTest("twoByteBigEndian") }

  @Test def test_bit1() { runner_02.runOneTest("bit1") }
  @Test def test_bit2() { runner_02.runOneTest("bit2") }
  @Test def test_bit3() { runner_02.runOneTest("bit3") }

  @Test def test_bitsRepresentedAsText1() { runner_02.runOneTest("bitsRepresentedAsText1") }
  @Test def test_bitsRepresentedAsText2() { runner_02.runOneTest("bitsRepresentedAsText2") }

  @Test def test_lengthGreaterThanEight1() { runner_02.runOneTest("lengthGreaterThanEight1") }
  @Test def test_lengthGreaterThanEight2() { runner_02.runOneTest("lengthGreaterThanEight2") }
  @Test def test_lengthGreaterThanEight3() { runner_02.runOneTest("lengthGreaterThanEight3") }
  @Test def test_lengthGreaterThanEight4() { runner_02.runOneTest("lengthGreaterThanEight4") }
  @Test def test_lengthGreaterThanEight5() { runner_02.runOneTest("lengthGreaterThanEight5") }

  @Test def test_littleEndianBits1() = {
    import org.apache.daffodil.util._
    val b1 = Bits.littleEndianBitValue(1, 10)
    val b2 = Bits.littleEndianBitValue(10, 10)
    val res = b1 + b2
    assertEquals(384, res)
  }

  @Test def test_bitUnsignedLong() { runner_02.runOneTest("bitUnsignedLong") }
  @Test def test_bitUnsignedLong2() { runner_02.runOneTest("bitUnsignedLong2") }
  @Test def test_bitUnsignedLong3() { runner_02.runOneTest("bitUnsignedLong3") }
  @Test def test_bitUnsignedLong4() { runner_02.runOneTest("bitUnsignedLong4") }
  @Test def test_bitUnsignedLong5() { runner_02.runOneTest("bitUnsignedLong5") }
  @Test def test_bitUCombo() { runner_02.runOneTest("bitUCombo") }
  @Test def test_bitUCombo2() { runner_02.runOneTest("bitUCombo2") }

  @Test def test_LengthProp_bits_bool() { runner_02.runOneTest("LengthProp_bits_bool") }
  @Test def test_LengthProp_bits_bool_false() { runner_02.runOneTest("LengthProp_bits_bool_false") }

  @Test def test_LengthProp_leftover1() { runner_02.runOneTest("LengthProp_leftover1") }
  @Test def test_LengthProp_leftover2() { runner_02.runOneTest("LengthProp_leftover2") }
  @Test def test_LengthProp_leftover3() { runner_02.runOneTest("LengthProp_leftover3") }
  @Test def test_LengthProp_leftover4() { runner_02.runOneTest("LengthProp_leftover4") }

  //DFDL-460
  @Test def test_LengthProp_floatBits() { runner_02.runOneTest("LengthProp_floatBits") }

  @Test def test_bitShort() { runner_02.runOneTest("bitShort") }
  @Test def test_bitShort2() { runner_02.runOneTest("bitShort2") }
  @Test def test_bitShort3() { runner_02.runOneTest("bitShort3") }
  @Test def test_bitShortImplicit() { runner_02.runOneTest("bitShortImplicit") }
  @Test def test_bitInt() { runner_02.runOneTest("bitInt") }
  @Test def test_bitIntImplicit() { runner_02.runOneTest("bitIntImplicit") }
  @Test def test_bitInteger() { runner_02.runOneTest("bitInteger") }
  @Test def test_bitLong() { runner_02.runOneTest("bitLong") }
  @Test def test_bitByte() { runner_02.runOneTest("bitByte") }
  @Test def test_bitSignedCombo() { runner_02.runOneTest("bitSignedCombo") }

}
