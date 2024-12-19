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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Assert
import org.junit.Test

object TestLengthProperties extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/length_properties/LengthProperties.tdml"
}

class TestLengthProperties extends TdmlTests {
  val tdmlSuite = TestLengthProperties

  @Test def LengthProp_02 = test
  @Test def LengthProp_charVsBytes = test
  @Test def LengthProp_charVsBytes2 = test
  @Test def LengthProp_longByteLength = test

  @Test def LengthProp_04 = test
  @Test def LengthProp_05 = test
  @Test def LengthProp_06 = test

  @Test def LengthProp_sequenceByLength = test

  @Test def LengthProp_tooShortFailure = test
  @Test def LengthProp_tooLongFailure = test

  @Test def LengthProp_zeroLength = test
  @Test def LengthProp_byteLength = test
  @Test def LengthProp_byteLength_UTF16 = test
  @Test def LengthProp_byteLength_UTF16fail = test

  @Test def LengthProp_longTextLength = test
  @Test def LengthProp_lengthExpression1 = test

  @Test def LengthProp_bits_01 = test
  @Test def LengthProp_bits_02 = test

  @Test def OneBitLeftOver = test
  @Test def OneBit1 = test
  @Test def ThreeBit1 = test
  @Test def seqBit1 = test
  @Test def seqBitLeftOver = test

  @Test def twoByteLittleEndian = test
  @Test def twoByteBigEndian = test

  @Test def bit1 = test
  @Test def bit2 = test
  @Test def bit3 = test

  @Test def bitsRepresentedAsText1 = test
  @Test def bitsRepresentedAsText2 = test

  @Test def lengthGreaterThanEight1 = test
  @Test def lengthGreaterThanEight2 = test
  @Test def lengthGreaterThanEight3 = test
  @Test def lengthGreaterThanEight4 = test
  @Test def lengthGreaterThanEight5 = test

  @Test def bitUnsignedLong = test
  @Test def bitUnsignedLong2 = test
  @Test def bitUnsignedLong3 = test
  @Test def bitUnsignedLong4 = test
  @Test def bitUnsignedLong5 = test
  @Test def bitUCombo = test
  @Test def bitUCombo2 = test

  @Test def LengthProp_bits_bool = test
  @Test def LengthProp_bits_bool_false = test

  @Test def LengthProp_leftover1 = test
  @Test def LengthProp_leftover2 = test
  @Test def LengthProp_leftover3 = test
  @Test def LengthProp_leftover4 = test

  // DFDL-460
  @Test def LengthProp_floatBits = test

  @Test def bitShort = test
  @Test def bitShort2 = test
  @Test def bitShort3 = test
  @Test def bitShortImplicit = test
  @Test def bitInt = test
  @Test def bitIntImplicit = test
  @Test def bitInteger = test
  @Test def bitLong = test
  @Test def bitByte = test
  @Test def bitSignedCombo = test

  @Test def test_littleEndianBits1() = {
    import org.apache.daffodil.lib.util._
    val b1 = Bits.littleEndianBitValue(1, 10)
    val b2 = Bits.littleEndianBitValue(10, 10)
    val res = b1 + b2
    Assert.assertEquals(384, res)
  }
}
