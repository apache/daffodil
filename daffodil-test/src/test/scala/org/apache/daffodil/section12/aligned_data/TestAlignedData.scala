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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestAlignedData extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/aligned_data/Aligned_Data.tdml"
}

object TestBinaryInput extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/aligned_data/BinaryInput_01.tdml"
}

class TestAlignedData extends TdmlTests {
  val tdmlSuite = TestAlignedData

  @Test def alignmentUnitsInvalid = test

  @Test def encodingAlignmentUtf32be = test

  @Test def leftAndRightFramingArray01 = test
  @Test def leftAndRightFramingArray02 = test
  @Test def leftAndRightFramingArray03 = test

  @Test def leftAndRightFramingChoice01 = test
  @Test def leftAndRightFramingChoice02 = test
  @Test def leftAndRightFramingChoice03 = test
  @Test def leftAndRightFramingChoice04 = test
  @Test def leftAndRightFramingChoice05 = test
  @Test def leftAndRightFramingChoice06 = test
  @Test def leftAndRightFramingChoice07 = test

  @Test def leftAndRightFramingNested01 = test
  @Test def leftAndRightFramingNested02 = test
  @Test def leftAndRightFramingNested03 = test
  @Test def leftAndRightFramingNested04 = test
  @Test def leftAndRightFramingNested05 = test

  @Test def leftFraming01 = test
  @Test def rightFraming01 = test
  @Test def leftAndRightFraming01 = test
  @Test def leftAndRightFraming02 = test

  @Test def alignmentStringErr = test
  @Test def alignmentStringBitSkip = test
  @Test def alignmentTerminatorBitSkip = test

  @Test def explicitAlignmentNoSkips01 = test
  @Test def explicitAlignmentNoSkips02 = test
  @Test def explicitAlignmentNoSkips03 = test
  @Test def explicitAlignmentNoSkips04 = test
  @Test def explicitAlignmentNoSkips05 = test

  @Test def trailingSkipDelimited01 = test
  @Test def trailingSkipDelimited02 = test
  @Test def trailingSkipDelimited03 = test
  @Test def trailingSkipDelimited04 = test

  @Test def alignmentOptionalElem = test
  @Test def alignmentOptionalElem02 = test
  // DFDL-1217
  @Ignore @Test def alignmentOptionalElem03 = test
  @Test def alignmentOptionalElem04 = test

  @Test def leadingSkip1 = test
  @Test def leadingSkip2 = test

  @Test def alignment01 = test
  @Test def alignment02 = test
  @Test def alignment03 = test

  @Test def impAlignmentHexBinary = test
  @Test def impAlignmentHexBinary2 = test

  @Test def implicitAlignmentUnsignedInt = test
  @Test def implicitAlignmentUnsignedShort = test

  @Test def implicitAlignmentInt = test
  @Test def implicitAlignmentInt2 = test

  @Test def implicitAlignmentShort = test
  @Test def implicitAlignmentShort2 = test

  @Test def implicitAlignmentLong = test
  @Test def implicitAlignmentLongT = test
  @Test def implicitAlignmentLongTBits = test
  @Test def implicitAlignmentByte = test
  @Test def implicitAlignmentByte2 = test
  @Test def implicitAlignmentUnsignedByte = test
  @Test def implicitAlignmentUnsignedByte2 = test

  @Test def implicitAlignmentUnsignedIntT = test

  @Test def implicitAlignmentUnsignedIntT2 = test
  @Test def implicitAlignmentUnsignedIntT2b = test
  @Test def implicitAlignmentUnsignedIntTBits = test
  @Test def implicitAlignmentUnsignedShortT2b = test
  @Test def implicitAlignmentUnsignedShortTBits = test

  @Test def implicitAlignmentUnsignedLong = test
  @Test def implicitAlignmentUnsignedLongT = test
  @Test def implicitAlignmentUnsignedLongT2 = test
  @Test def implicitAlignmentUnsignedLongTBits = test
  @Test def implicitAlignmentIntT = test
  @Test def implicitAlignmentShortT = test
  @Test def implicitAlignmentByteT = test
  @Test def implicitAlignmentByteT2 = test
  @Test def implicitAlignmentUnsignedByteT = test
  @Test def implicitAlignmentUnsignedByteT2 = test

  @Test def implicitAlignmentDateT = test
  @Test def implicitAlignmentDateT2 = test
  @Test def implicitAlignmentTimeT = test
  @Test def implicitAlignmentDateTimeT = test

  @Test def implicitAlignmentFloatT = test
  @Test def implicitAlignmentFloatT2 = test
  @Test def implicitAlignmentFloatT_Fail = test
  @Test def implicitAlignmentFloat = test
  @Test def implicitAlignmentFloat2 = test
  @Test def implicitAlignmentDouble = test
  @Test def implicitAlignmentDouble2 = test
  @Test def implicitAlignmentDoubleT = test
  @Test def implicitAlignmentDoubleT2 = test
  @Test def implicitAlignmentDoubleT_Fail = test

  @Test def implicitAlignmentString1 = test
  @Test def implicitAlignmentString2 = test

  @Test def impAlignmentNonNegativeInteger = test
  @Test def impAlignmentNonNegativeInteger2 = test
  @Test def impAlignmentNonNegativeInteger3 = test

  @Test def impAlignmentInteger1 = test
  @Test def impAlignmentInteger2 = test
  @Test def impAlignmentInteger3 = test

  @Test def implicitAlignmentNonNegativeIntegerT = test
  @Test def implicitAlignmentNonNegativeIntegerT2 = test
  @Test def implicitAlignmentNonNegativeIntegerT3 = test
  @Test def implicitAlignmentNonNegativeIntegerT_Fail = test

  @Test def implicitAlignmentIntegerT = test
  @Test def implicitAlignmentIntegerT2 = test
  @Test def implicitAlignmentIntegerT_Fail = test

  @Test def implicitAlignmentDecimalT = test
  @Test def implicitAlignmentDecimalT2 = test
  @Test def implicitAlignmentDecimalT_Fail = test
  @Test def implicitAlignmentDecimal = test
  @Test def implicitAlignmentDecimal2 = test

  @Test def alignmentLSBFirst = test
  @Test def alignmentMSBFirst = test

  @Test def alignmentArray = test

  @Test def fillByte_01 = test
  @Test def fillByte_02 = test
  @Test def fillByte_03 = test
  @Test def fillByte_04 = test
  @Test def fillByte_05 = test
  @Test def fillByte_06 = test

  @Test def alignmentFillByteNotDefined = test
  @Test def alignmentFillByteDefined = test

  @Test def separatorMTA_01 = test
}

class TestBinaryInput extends TdmlTests {
  val tdmlSuite = TestBinaryInput

  @Test def LeadingSkipBytes = test
  @Test def LeadingSkipBits = test
  @Test def TrailingSkipBytes = test
  @Test def TrailingSkipBits = test
  @Test def AligningSkipBytes = test
  @Test def AligningSkipBytes2 = test
  @Test def AligningSkipBits = test
  @Test def AligningSkipBits2 = test
}
