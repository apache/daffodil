package edu.illinois.ncsa.daffodil.section12.length_properties

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

class TestLengthProperties {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_length_explicit_12_01() { runner_01.runOneTest("length_explicit_12_01") }
  @Test def test_length_explicit_12_02() { runner_01.runOneTest("length_explicit_12_02") }
  @Test def test_length_delimited_12_06() { runner_01.runOneTest("length_delimited_12_06") }

  val testDir_02 = "/edu/illinois/ncsa/daffodil/section12/length_properties/"
  val tdml_02 = testDir_02 + "LengthProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  @Test def test_LengthProp_02() { runner_02.runOneTest("LengthProp_02") }
  @Test def test_LengthProp_04() { runner_02.runOneTest("LengthProp_04") }
  @Test def test_LengthProp_05() { runner_02.runOneTest("LengthProp_05") }
  //  @Test def test_LengthProp_06() { runner_02.runOneTest("LengthProp_06") }

  @Test def test_LengthProp_sequenceByLength() { runner_02.runOneTest("LengthProp_sequenceByLength") }
  @Test def test_LengthProp_charVsBytes() { runner_02.runOneTest("LengthProp_charVsBytes") }
  @Test def test_LengthProp_charVsBytes2() { runner_02.runOneTest("LengthProp_charVsBytes2") }
  @Test def test_LengthProp_tooShortFailure() { runner_02.runOneTest("LengthProp_tooShortFailure") }
  @Test def test_LengthProp_tooLongFailure() { runner_02.runOneTest("LengthProp_tooLongFailure") }

  @Test def test_LengthProp_zeroLength() { runner_02.runOneTest("LengthProp_zeroLength") }
  @Test def test_LengthProp_byteLength() { runner_02.runOneTest("LengthProp_byteLength") }
  @Test def test_LengthProp_byteLength_UTF16() { runner_02.runOneTest("LengthProp_byteLength_UTF16") }
  @Test def test_LengthProp_byteLength_UTF16fail() { runner_02.runOneTest("LengthProp_byteLength_UTF16fail") }
  @Test def test_LengthProp_longByteLength() { runner_02.runOneTest("LengthProp_longByteLength") }
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
 
  @Test def test_bitULong() { runner_02.runOneTest("bitULong") }
  @Test def test_bitULong2() { runner_02.runOneTest("bitULong2") }
  @Test def test_bitULong3() { runner_02.runOneTest("bitULong3") }
  @Test def test_bitULong4() { runner_02.runOneTest("bitULong4") }
  @Test def test_bitULong5() { runner_02.runOneTest("bitULong5") }
  @Test def test_bitUCombo() { runner_02.runOneTest("bitUCombo") }
  @Test def test_bitUCombo2() { runner_02.runOneTest("bitUCombo2") }

//  @Test def test_LengthProp_bits_bool() { runner_02.runOneTest("LengthProp_bits_bool") }

  @Test def test_LengthProp_leftover1() { runner_02.runOneTest("LengthProp_leftover1") }
  @Test def test_LengthProp_leftover2() { runner_02.runOneTest("LengthProp_leftover2") }
  @Test def test_LengthProp_leftover3() { runner_02.runOneTest("LengthProp_leftover3") }
  @Test def test_LengthProp_leftover4() { runner_02.runOneTest("LengthProp_leftover4") }
  
//  @Test def test_LengthProp_floatBits() { runner_02.runOneTest("LengthProp_floatBits") }
 
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
