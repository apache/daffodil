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

package org.apache.daffodil.section12.lengthKind

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestLengthKindExplicit extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/ExplicitTests.tdml"
}

class TestLengthKindExplicit extends TdmlTests {
  val tdmlSuite = TestLengthKindExplicit

  @Test def Lesson1_lengthKind_explicit = test
  @Test def test_ExplicitLengthBytesNotFixed = test
  @Test def ExplicitLengthBitsFixed = test
  @Test def test_ExplicitLengthBytesFixed = test
  @Test def ExplicitLengthBitsNotFixed = test
  @Test def ExplicitLengthCharsNotFixed = test
  @Test def ExplicitLengthCharsFixed = test
  @Test def ExplicitLengthBytesFixed50 = test

  @Test def test_lengthRuntimeIntNaN_PE = test
  @Test def test_lengthRuntimeDoubleNaN_PE = test
  @Test def test_lengthRuntimeIntNegative_SDE = test
  @Test def test_lengthRuntimeDoubleNegative_SDE = test

  @Test def test_ExplicitLengthBytesBroken = test

  @Test def test_ExplicitLengthBytesNotGiven = test

  @Test def test_ExplicitLengthBytesChoiceRef = test
  @Test def test_ExplicitLengthChildLengthLessParent_Chars = test
  @Test def test_ExplicitLengthChildLengthLessParent_Bytes = test
  @Test def test_ExplicitLengthChildLengthMoreParent_Chars = test

  @Test def explicitBytes_string_01 = test
  @Test def explicitBytes_int_01 = test
  @Test def explicitBytes_int_02 = test

  // Added for issue related to DFDL-1674
  @Test def denseBit_lengthKind_explicit = test

  // DFDL-2931
  @Test def lengthUnitsBitsForInteger_explicit = test
  @Test def lengthUnitsBitsForInteger_explicit2 = test
  @Test def lengthUnitsBitsForDecimal_explicit = test
  @Test def lengthUnitsBitsForFloat_explicit1 = test
  @Test def lengthUnitsBitsForFloat_explicit2 = test
  @Test def lengthUnitsBitsForFloat_explicit3 = test
  @Test def lengthUnitsBitsForDouble_explicit1 = test
  @Test def lengthUnitsBitsForDouble_explicit2 = test
  @Test def lengthUnitsBitsForDouble_explicit3 = test

  @Test def invalidUnsignedLongBitLength = test

  @Test def invalidUnsignedLongByteLength = test
  @Test def invalidUnsignedIntBitLength = test
  @Test def invalidUnsignedShortBitLength = test
  @Test def invalidUnsignedByteBitLength = test

  @Test def invalidLongBitLength = test
  @Test def invalidIntBitLength = test
  @Test def invalidShortBitLength = test
  @Test def invalidByteBitLength = test

  @Test def invalidLongBitLengthExpr = test

  @Test def invalidIntBitLengthExpr = test
  @Test def unparseInvalidIntBitLengthExpr = test

  @Test def invalidShortBitLengthExpr = test

  @Test def invalidByteBitLengthExpr = test

  @Test def insufficientBitsComplex = test

  @Test def insufficientBitsByte = test

  // DFDL-2297
  @Test def outOfRangeLengthBinaryInteger1 = test
  @Test def outOfRangeLengthBinaryInteger2 = test
  @Test def outOfRangeLengthBinaryInteger3 = test
  @Test def outOfRangeLengthBinaryInteger4 = test
  @Test def outOfRangeLengthBinaryInteger5 = test
  @Test def outOfRangeLengthBinaryInteger6 = test
  @Test def outOfRangeLengthBinaryInteger7 = test
  @Test def outOfRangeLengthBinaryInteger8 = test
  @Test def outOfRangeLengthBinaryInteger9 = test
  @Test def outOfRangeLengthBinaryInteger10 = test
  @Test def outOfRangeLengthBinaryInteger11 = test
  @Test def outOfRangeLengthBinaryInteger12 = test
  @Test def outOfRangeLengthBinaryInteger13 = test
  @Test def inRangeLengthBinaryInteger1 = test
  @Test def inRangeLengthBinaryInteger2 = test
  @Test def inRangeLengthBinaryInteger3 = test
  @Test def inRangeLengthBinaryInteger4 = test
  @Test def inRangeLengthBinaryInteger5 = test
  @Test def inRangeLengthBinaryInteger6 = test
  @Test def inRangeLengthBinaryInteger7 = test
  @Test def inRangeLengthBinaryInteger8 = test
  @Test def inRangeLengthBinaryInteger9 = test
  @Test def inRangeLengthBinaryInteger10 = test
  @Test def outOfRangeLengthBinaryDecimal1 = test
  @Test def outOfRangeLengthBinaryDecimal2 = test
  @Test def outOfRangeLengthBinaryDecimal3 = test
  @Test def outOfRangeLengthBinaryDecimal4 = test
  @Test def outOfRangeLengthBinaryDecimal5 = test
  @Test def outOfRangeLengthBinaryDecimal6 = test
  @Test def outOfRangeLengthBinaryDecimal7 = test
  @Test def outOfRangeLengthBinaryDecimal8 = test
  @Test def outOfRangeLengthBinaryDecimal9 = test
  @Test def outOfRangeLengthBinaryDecimal10 = test
}
