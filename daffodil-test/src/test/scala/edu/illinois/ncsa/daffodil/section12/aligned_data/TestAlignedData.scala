package edu.illinois.ncsa.daffodil.section12.aligned_data

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestAlignedData {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/section12/aligned_data/"
  val tdml1 = testDir_01 + "Aligned_Data.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1), validateTDMLFile = true, validateDFDLSchemas = false)
  def dbg = {
    Debugger.withTracing(false)
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  }

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
  @Test def test_alignmentOptionalElem03() = { runner1.runOneTest("alignmentOptionalElem03") }
  @Test def test_alignmentOptionalElem04() = { runner1.runOneTest("alignmentOptionalElem04") }

  @Test def test_leadingSkip1() = { runner1.runOneTest("leadingSkip1") }
  @Test def test_leadingSkip2() = { runner1.runOneTest("leadingSkip2") }

  @Test def test_alignment01() = { runner1.runOneTest("alignment01") }
  @Test def test_alignment02() = { runner1.runOneTest("alignment02") }
  @Test def test_alignment03() = { runner1.runOneTest("alignment03") }

  @Test def test_impAlignmentHexBinary() = { runner1.runOneTest("impAlignmentHexBinary") }
  @Test def test_impAlignmentHexBinary2() = { runner1.runOneTest("impAlignmentHexBinary2") }

  @Test def test_implicitAlignmentUInt() = { runner1.runOneTest("implicitAlignmentUInt") }
  @Test def test_implicitAlignmentUShort() = { runner1.runOneTest("implicitAlignmentUShort") }

  @Test def test_implicitAlignmentInt() = { runner1.runOneTest("implicitAlignmentInt") }
  @Test def test_implicitAlignmentInt2() = { runner1.runOneTest("implicitAlignmentInt2") }

  @Test def test_implicitAlignmentShort() = { runner1.runOneTest("implicitAlignmentShort") }
  @Test def test_implicitAlignmentShort2() = { runner1.runOneTest("implicitAlignmentShort2") }

  @Test def test_implicitAlignmentLong() = { runner1.runOneTest("implicitAlignmentLong") }
  @Test def test_implicitAlignmentLongT() = { runner1.runOneTest("implicitAlignmentLongT") }
  @Test def test_implicitAlignmentLongTBits() = { runner1.runOneTest("implicitAlignmentLongTBits") }
  @Test def test_implicitAlignmentByte() = { runner1.runOneTest("implicitAlignmentByte") }
  @Test def test_implicitAlignmentByte2() = { runner1.runOneTest("implicitAlignmentByte2") }
  @Test def test_implicitAlignmentUByte() = { runner1.runOneTest("implicitAlignmentUByte") }
  @Test def test_implicitAlignmentUByte2() = { runner1.runOneTest("implicitAlignmentUByte2") }

  @Test def test_implicitAlignmentUIntT() = { runner1.runOneTest("implicitAlignmentUIntT") }

  @Test def test_implicitAlignmentUIntT2() = { runner1.runOneTest("implicitAlignmentUIntT2") }
  @Test def test_implicitAlignmentUIntT2b() = { runner1.runOneTest("implicitAlignmentUIntT2b") }
  @Test def test_implicitAlignmentUIntTBits() = { runner1.runOneTest("implicitAlignmentUIntTBits") }
  @Test def test_implicitAlignmentUShortT2b() = { runner1.runOneTest("implicitAlignmentUShortT2b") }
  @Test def test_implicitAlignmentUShortTBits() = { runner1.runOneTest("implicitAlignmentUShortTBits") }

  @Test def test_implicitAlignmentULong() = { runner1.runOneTest("implicitAlignmentULong") }
  @Test def test_implicitAlignmentULongT() = { runner1.runOneTest("implicitAlignmentULongT") }
  @Test def test_implicitAlignmentULongT2() = { runner1.runOneTest("implicitAlignmentULongT2") }
  @Test def test_implicitAlignmentULongTBits() = { runner1.runOneTest("implicitAlignmentULongTBits") }
  @Test def test_implicitAlignmentIntT() = { runner1.runOneTest("implicitAlignmentIntT") }
  @Test def test_implicitAlignmentShortT() = { runner1.runOneTest("implicitAlignmentShortT") }
  @Test def test_implicitAlignmentByteT() = { runner1.runOneTest("implicitAlignmentByteT") }
  @Test def test_implicitAlignmentByteT2() = { runner1.runOneTest("implicitAlignmentByteT2") }
  @Test def test_implicitAlignmentUByteT() = { runner1.runOneTest("implicitAlignmentUByteT") }
  @Test def test_implicitAlignmentUByteT2() = { runner1.runOneTest("implicitAlignmentUByteT2") }

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

  val tdml2 = testDir_01 + "BinaryInput_01.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_LeadingSkipBytes() { runner2.runOneTest("LeadingSkipBytes") }
  @Test def test_LeadingSkipBits() { runner2.runOneTest("LeadingSkipBits") }
  @Test def test_TrailingSkipBytes() { runner2.runOneTest("TrailingSkipBytes") }
  @Test def test_TrailingSkipBits() { runner2.runOneTest("TrailingSkipBits") }
  @Test def test_AligningSkipBytes() { runner2.runOneTest("AligningSkipBytes") }
  @Test def test_AligningSkipBytes2() { runner2.runOneTest("AligningSkipBytes2") }
  @Test def test_AligningSkipBits() { runner2.runOneTest("AligningSkipBits") }
  @Test def test_AligningSkipBits2() { runner2.runOneTest("AligningSkipBits2") }

}
