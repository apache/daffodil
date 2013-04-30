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
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestAlignedData extends JUnitSuite {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/section12/aligned_data/"
  val tdml1 = testDir_01 + "Aligned_Data.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))

  @Test def test_leadingSkip_1() = { runner1.runOneTest("leadingSkip1") }
  @Test def test_leadingSkip_2() = { runner1.runOneTest("leadingSkip2") }
  
  @Test def test_alignment01() = { runner1.runOneTest("alignment01") }
  @Test def test_alignment02() = { runner1.runOneTest("alignment02") }
  @Test def test_alignment03() = { runner1.runOneTest("alignment03") }
  
  //@Test def test_impAlignmentHexBinary() = { runner1.runOneTest("impAlignmentHexBinary") }
  
 /*
    2.95. Section 12.1. State that if representation is text or type is string, then alignment is
          determined by character set encoding.
 */

//  @Test def test_implicitAlignmentString1() = { runner1.runOneTest("implicitAlignmentString1") }
  @Test def test_implicitAlignmentString2() = { runner1.runOneTest("implicitAlignmentString2") }

//  @Test def test_implicitAlignmentUInt() = { runner1.runOneTest("implicitAlignmentUInt") }
  @Test def test_implicitAlignmentUIntb() = { runner1.runOneTest("implicitAlignmentUIntb") }
  
//  @Test def test_implicitAlignmentUShort() = { runner1.runOneTest("implicitAlignmentUShort") }
  @Test def test_implicitAlignmentUShortb() = { runner1.runOneTest("implicitAlignmentUShortb") }
  
  val tdml2 = testDir_01 + "BinaryInput_01.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_leading_skip_byte(){ runner2.runOneTest("LeadingSkipBytes")}
  @Test def test_leading_skip_bit(){ runner2.runOneTest("LeadingSkipBits")}
  @Test def test_trailing_skip_byte(){ runner2.runOneTest("TrailingSkipBytes")}
  @Test def test_trailing_skip_bit(){ runner2.runOneTest("TrailingSkipBits")}
  @Test def test_AligningSkipBytes(){ runner2.runOneTest("AligningSkipBytes")}
  @Test def test_AligningSkipBytes2(){ runner2.runOneTest("AligningSkipBytes2")}
  @Test def test_AligningSkipBits(){ runner2.runOneTest("AligningSkipBits")}
  @Test def test_AligningSkipBits2(){ runner2.runOneTest("AligningSkipBits2")}

}
