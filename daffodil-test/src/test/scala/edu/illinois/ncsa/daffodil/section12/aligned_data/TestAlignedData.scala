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

package edu.illinois.ncsa.daffodil.section12.aligned_data

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestAlignedData {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/section12/aligned_data/"
  val runner1 = Runner(testDir_01, "Aligned_Data.tdml")
  val runner2 = Runner(testDir_01, "BinaryInput_01.tdml")

  @AfterClass def shutDown {
    runner1.reset
    runner2.reset
  }
}

class TestAlignedData {
  import TestAlignedData._

  @Test def test_alignmentUnitsInvalid() = { runner1.runOneTest("alignmentUnitsInvalid") }

  @Test def test_encodingAlignmentUtf32be() = { runner1.runOneTest("encodingAlignmentUtf32be") }

  @Test def test_leftAndRightFramingArray01() = { runner1.runOneTest("leftAndRightFramingArray01") }
  @Test def test_leftAndRightFramingArray02() = { runner1.runOneTest("leftAndRightFramingArray02") }
  @Test def test_leftAndRightFramingArray03() = { runner1.runOneTest("leftAndRightFramingArray03") }

  @Test def test_leftAndRightFramingChoice01() = { runner1.runOneTest("leftAndRightFramingChoice01") }
  @Test def test_leftAndRightFramingChoice02() = { runner1.runOneTest("leftAndRightFramingChoice02") }
  @Test def test_leftAndRightFramingChoice03() = { runner1.runOneTest("leftAndRightFramingChoice03") }
  @Test def test_leftAndRightFramingChoice04() = { runner1.runOneTest("leftAndRightFramingChoice04") }
  @Test def test_leftAndRightFramingChoice05() = { runner1.runOneTest("leftAndRightFramingChoice05") }
  @Test def test_leftAndRightFramingChoice06() = { runner1.runOneTest("leftAndRightFramingChoice06") }
  @Test def test_leftAndRightFramingChoice07() = { runner1.runOneTest("leftAndRightFramingChoice07") }

  @Test def test_leftAndRightFramingNested01() = { runner1.runOneTest("leftAndRightFramingNested01") }
  @Test def test_leftAndRightFramingNested02() = { runner1.runOneTest("leftAndRightFramingNested02") }
  @Test def test_leftAndRightFramingNested03() = { runner1.runOneTest("leftAndRightFramingNested03") }
  @Test def test_leftAndRightFramingNested04() = { runner1.runOneTest("leftAndRightFramingNested04") }
  @Test def test_leftAndRightFramingNested05() = { runner1.runOneTest("leftAndRightFramingNested05") }

  @Test def test_leftFraming01() = { runner1.runOneTest("leftFraming01") }
  @Test def test_rightFraming01() = { runner1.runOneTest("rightFraming01") }
  @Test def test_leftAndRightFraming01() = { runner1.runOneTest("leftAndRightFraming01") }
  @Test def test_leftAndRightFraming02() = { runner1.runOneTest("leftAndRightFraming02") }

  @Test def test_alignmentStringErr() = { runner1.runOneTest("alignmentStringErr") }
  //  @Test def test_alignmentStringBitSkip() = { runner1.runOneTest("alignmentStringBitSkip") }
  //  @Test def test_alignmentTerminatorBitSkip() = { runner1.runOneTest("alignmentTerminatorBitSkip") }

  @Test def test_explicitAlignmentNoSkips01() = { runner1.runOneTest("explicitAlignmentNoSkips01") }
  @Test def test_explicitAlignmentNoSkips02() = { runner1.runOneTest("explicitAlignmentNoSkips02") }
  @Test def test_explicitAlignmentNoSkips03() = { runner1.runOneTest("explicitAlignmentNoSkips03") }
  @Test def test_explicitAlignmentNoSkips04() = { runner1.runOneTest("explicitAlignmentNoSkips04") }
  @Test def test_explicitAlignmentNoSkips05() = { runner1.runOneTest("explicitAlignmentNoSkips05") }

  @Test def test_trailingSkipDelimited01() = { runner1.runOneTest("trailingSkipDelimited01") }
  @Test def test_trailingSkipDelimited02() = { runner1.runOneTest("trailingSkipDelimited02") }
  @Test def test_trailingSkipDelimited03() = { runner1.runOneTest("trailingSkipDelimited03") }
  @Test def test_trailingSkipDelimited04() = { runner1.runOneTest("trailingSkipDelimited04") }

  @Test def test_alignmentOptionalElem() = { runner1.runOneTest("alignmentOptionalElem") }
  @Test def test_alignmentOptionalElem02() = { runner1.runOneTest("alignmentOptionalElem02") }
  // DFDL-1217
  //  @Test def test_alignmentOptionalElem03() = { runner1.runOneTest("alignmentOptionalElem03") }
  @Test def test_alignmentOptionalElem04() = { runner1.runOneTest("alignmentOptionalElem04") }

  @Test def test_leadingSkip1() = { runner1.runOneTest("leadingSkip1") }
  @Test def test_leadingSkip2() = { runner1.runOneTest("leadingSkip2") }

  @Test def test_alignment01() = { runner1.runOneTest("alignment01") }
  @Test def test_alignment02() = { runner1.runOneTest("alignment02") }
  @Test def test_alignment03() = { runner1.runOneTest("alignment03") }

  @Test def test_impAlignmentHexBinary() = { runner1.runOneTest("impAlignmentHexBinary") }
  @Test def test_impAlignmentHexBinary2() = { runner1.runOneTest("impAlignmentHexBinary2") }

  @Test def test_implicitAlignmentUnsignedInt() = { runner1.runOneTest("implicitAlignmentUnsignedInt") }
  @Test def test_implicitAlignmentUnsignedShort() = { runner1.runOneTest("implicitAlignmentUnsignedShort") }

  @Test def test_implicitAlignmentInt() = { runner1.runOneTest("implicitAlignmentInt") }
  @Test def test_implicitAlignmentInt2() = { runner1.runOneTest("implicitAlignmentInt2") }

  @Test def test_implicitAlignmentShort() = { runner1.runOneTest("implicitAlignmentShort") }
  @Test def test_implicitAlignmentShort2() = { runner1.runOneTest("implicitAlignmentShort2") }

  @Test def test_implicitAlignmentLong() = { runner1.runOneTest("implicitAlignmentLong") }
  @Test def test_implicitAlignmentLongT() = { runner1.runOneTest("implicitAlignmentLongT") }
  @Test def test_implicitAlignmentLongTBits() = { runner1.runOneTest("implicitAlignmentLongTBits") }
  @Test def test_implicitAlignmentByte() = { runner1.runOneTest("implicitAlignmentByte") }
  @Test def test_implicitAlignmentByte2() = { runner1.runOneTest("implicitAlignmentByte2") }
  @Test def test_implicitAlignmentUnsignedByte() = { runner1.runOneTest("implicitAlignmentUnsignedByte") }
  @Test def test_implicitAlignmentUnsignedByte2() = { runner1.runOneTest("implicitAlignmentUnsignedByte2") }

  @Test def test_implicitAlignmentUnsignedIntT() = { runner1.runOneTest("implicitAlignmentUnsignedIntT") }

  @Test def test_implicitAlignmentUnsignedIntT2() = { runner1.runOneTest("implicitAlignmentUnsignedIntT2") }
  @Test def test_implicitAlignmentUnsignedIntT2b() = { runner1.runOneTest("implicitAlignmentUnsignedIntT2b") }
  @Test def test_implicitAlignmentUnsignedIntTBits() = { runner1.runOneTest("implicitAlignmentUnsignedIntTBits") }
  @Test def test_implicitAlignmentUnsignedShortT2b() = { runner1.runOneTest("implicitAlignmentUnsignedShortT2b") }
  @Test def test_implicitAlignmentUnsignedShortTBits() = { runner1.runOneTest("implicitAlignmentUnsignedShortTBits") }

  @Test def test_implicitAlignmentUnsignedLong() = { runner1.runOneTest("implicitAlignmentUnsignedLong") }
  @Test def test_implicitAlignmentUnsignedLongT() = { runner1.runOneTest("implicitAlignmentUnsignedLongT") }
  @Test def test_implicitAlignmentUnsignedLongT2() = { runner1.runOneTest("implicitAlignmentUnsignedLongT2") }
  @Test def test_implicitAlignmentUnsignedLongTBits() = { runner1.runOneTest("implicitAlignmentUnsignedLongTBits") }
  @Test def test_implicitAlignmentIntT() = { runner1.runOneTest("implicitAlignmentIntT") }
  @Test def test_implicitAlignmentShortT() = { runner1.runOneTest("implicitAlignmentShortT") }
  @Test def test_implicitAlignmentByteT() = { runner1.runOneTest("implicitAlignmentByteT") }
  @Test def test_implicitAlignmentByteT2() = { runner1.runOneTest("implicitAlignmentByteT2") }
  @Test def test_implicitAlignmentUnsignedByteT() = { runner1.runOneTest("implicitAlignmentUnsignedByteT") }
  @Test def test_implicitAlignmentUnsignedByteT2() = { runner1.runOneTest("implicitAlignmentUnsignedByteT2") }

  @Test def test_implicitAlignmentDateT() = { runner1.runOneTest("implicitAlignmentDateT") }
  @Test def test_implicitAlignmentDateT2() = { runner1.runOneTest("implicitAlignmentDateT2") }
  @Test def test_implicitAlignmentTimeT() = { runner1.runOneTest("implicitAlignmentTimeT") }
  @Test def test_implicitAlignmentDateTimeT() = { runner1.runOneTest("implicitAlignmentDateTimeT") }

  @Test def test_implicitAlignmentFloatT() = { runner1.runOneTest("implicitAlignmentFloatT") }
  @Test def test_implicitAlignmentFloatT2() = { runner1.runOneTest("implicitAlignmentFloatT2") }
  @Test def test_implicitAlignmentFloatT_Fail() = { runner1.runOneTest("implicitAlignmentFloatT_Fail") }
  @Test def test_implicitAlignmentFloat() = { runner1.runOneTest("implicitAlignmentFloat") }
  @Test def test_implicitAlignmentFloat2() = { runner1.runOneTest("implicitAlignmentFloat2") }
  @Test def test_implicitAlignmentDouble() = { runner1.runOneTest("implicitAlignmentDouble") }
  @Test def test_implicitAlignmentDouble2() = { runner1.runOneTest("implicitAlignmentDouble2") }
  @Test def test_implicitAlignmentDoubleT() = { runner1.runOneTest("implicitAlignmentDoubleT") }
  @Test def test_implicitAlignmentDoubleT2() = { runner1.runOneTest("implicitAlignmentDoubleT2") }
  @Test def test_implicitAlignmentDoubleT_Fail() = { runner1.runOneTest("implicitAlignmentDoubleT_Fail") }

  @Test def test_implicitAlignmentString1() = { runner1.runOneTest("implicitAlignmentString1") }
  @Test def test_implicitAlignmentString2() = { runner1.runOneTest("implicitAlignmentString2") }

  @Test def test_impAlignmentNonNegativeInteger() = { runner1.runOneTest("impAlignmentNonNegativeInteger") }
  @Test def test_impAlignmentNonNegativeInteger2() = { runner1.runOneTest("impAlignmentNonNegativeInteger2") }
  @Test def test_impAlignmentNonNegativeInteger3() = { runner1.runOneTest("impAlignmentNonNegativeInteger3") }

  @Test def test_impAlignmentInteger1() = { runner1.runOneTest("impAlignmentInteger1") }
  @Test def test_impAlignmentInteger2() = { runner1.runOneTest("impAlignmentInteger2") }
  @Test def test_impAlignmentInteger3() = { runner1.runOneTest("impAlignmentInteger3") }

  @Test def test_implicitAlignmentNonNegativeIntegerT() = { runner1.runOneTest("implicitAlignmentNonNegativeIntegerT") }
  @Test def test_implicitAlignmentNonNegativeIntegerT2() = { runner1.runOneTest("implicitAlignmentNonNegativeIntegerT2") }
  @Test def test_implicitAlignmentNonNegativeIntegerT3() = { runner1.runOneTest("implicitAlignmentNonNegativeIntegerT3") }
  @Test def test_implicitAlignmentNonNegativeIntegerT_Fail() = { runner1.runOneTest("implicitAlignmentNonNegativeIntegerT_Fail") }

  @Test def test_implicitAlignmentIntegerT() = { runner1.runOneTest("implicitAlignmentIntegerT") }
  @Test def test_implicitAlignmentIntegerT2() = { runner1.runOneTest("implicitAlignmentIntegerT2") }
  @Test def test_implicitAlignmentIntegerT_Fail() = { runner1.runOneTest("implicitAlignmentIntegerT_Fail") }

  @Test def test_implicitAlignmentDecimalT() = { runner1.runOneTest("implicitAlignmentDecimalT") }
  @Test def test_implicitAlignmentDecimalT2() = { runner1.runOneTest("implicitAlignmentDecimalT2") }
  @Test def test_implicitAlignmentDecimalT_Fail() = { runner1.runOneTest("implicitAlignmentDecimalT_Fail") }
  @Test def test_implicitAlignmentDecimal() = { runner1.runOneTest("implicitAlignmentDecimal") }
  @Test def test_implicitAlignmentDecimal2() = { runner1.runOneTest("implicitAlignmentDecimal2") }

  @Test def test_alignmentLSBFirst() = { runner1.runOneTest("alignmentLSBFirst") }
  @Test def test_alignmentMSBFirst() = { runner1.runOneTest("alignmentMSBFirst") }

  @Test def test_LeadingSkipBytes() { runner2.runOneTest("LeadingSkipBytes") }
  @Test def test_LeadingSkipBits() { runner2.runOneTest("LeadingSkipBits") }
  @Test def test_TrailingSkipBytes() { runner2.runOneTest("TrailingSkipBytes") }
  @Test def test_TrailingSkipBits() { runner2.runOneTest("TrailingSkipBits") }
  @Test def test_AligningSkipBytes() { runner2.runOneTest("AligningSkipBytes") }
  @Test def test_AligningSkipBytes2() { runner2.runOneTest("AligningSkipBytes2") }
  @Test def test_AligningSkipBits() { runner2.runOneTest("AligningSkipBits") }
  @Test def test_AligningSkipBits2() { runner2.runOneTest("AligningSkipBits2") }

  @Test def test_alignmentArray() = { runner1.runOneTest("alignmentArray") }

  @Test def test_fillByte_01() = { runner1.runOneTest("fillByte_01") }
  @Test def test_fillByte_02() = { runner1.runOneTest("fillByte_02") }
  @Test def test_fillByte_03() = { runner1.runOneTest("fillByte_03") }
  @Test def test_fillByte_04() = { runner1.runOneTest("fillByte_04") }
  @Test def test_fillByte_05() = { runner1.runOneTest("fillByte_05") }
  @Test def test_fillByte_06() = { runner1.runOneTest("fillByte_06") }
}
