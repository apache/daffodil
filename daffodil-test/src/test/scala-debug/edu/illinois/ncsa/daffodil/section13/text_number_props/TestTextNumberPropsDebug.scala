package edu.illinois.ncsa.daffodil.section13.text_number_props

/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

class TestTextNumberPropsDebug {
  val testDir = "/edu/illinois/ncsa/daffodil/section13/text_number_props/"
  val aa = testDir + "TextNumberProps.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

// DFDL-845
  @Test def test_textNumberCheckPolicy_lax01() { runner.runOneTest("textNumberCheckPolicy_lax01") }
  @Test def test_textNumberCheckPolicy_lax05() { runner.runOneTest("textNumberCheckPolicy_lax05") }
  @Test def test_textNumberCheckPolicy_lax04() { runner.runOneTest("textNumberCheckPolicy_lax04") }
  @Test def test_textNumberCheckPolicy_lax10() { runner.runOneTest("textNumberCheckPolicy_lax10") }
  @Test def test_textNumberCheckPolicy_lax11() { runner.runOneTest("textNumberCheckPolicy_lax11") }
  @Test def test_textNumberCheckPolicy_lax12() { runner.runOneTest("textNumberCheckPolicy_lax12") }
  @Test def test_textNumberCheckPolicy_lax13() { runner.runOneTest("textNumberCheckPolicy_lax13") }
  @Test def test_textNumberCheckPolicy_lax14() { runner.runOneTest("textNumberCheckPolicy_lax14") }
  @Test def test_textNumberCheckPolicy_lax15() { runner.runOneTest("textNumberCheckPolicy_lax15") }
  @Test def test_textNumberCheckPolicy_lax16() { runner.runOneTest("textNumberCheckPolicy_lax16") }

// DFDL-843
  @Test def test_textStandardGroupingSeparator03() { runner.runOneTest("textStandardGroupingSeparator03") }

// DFDL-847
  @Test def test_textStandardDecimalSeparator10() { runner.runOneTest("textStandardDecimalSeparator10") }
  @Test def test_textStandardDecimalSeparator11() { runner.runOneTest("textStandardDecimalSeparator11") }

// DFDL-851
  @Test def test_textStandardGroupingSeparator12() { runner.runOneTest("textStandardGroupingSeparator12") }
  @Test def test_textStandardDecimalSeparator17() { runner.runOneTest("textStandardDecimalSeparator17") }
  @Test def test_standardZeroRep07() { runner.runOneTest("standardZeroRep07") }

// DFDL-853
  @Test def test_textNumberPattern_pSymbol01() { runner.runOneTest("textNumberPattern_pSymbol01") }
  @Test def test_textNumberPattern_pSymbol02() { runner.runOneTest("textNumberPattern_pSymbol02") }

  @Test def test_textNumberPattern_scientificNotation02() { runner.runOneTest("textNumberPattern_scientificNotation02") }

// DFDL-859
  @Test def test_decimalPadding01() { runner.runOneTest("decimalPadding01") }
  @Test def test_decimalPadding04() { runner.runOneTest("decimalPadding04") }
  @Test def test_nonNegIntPadding01() { runner.runOneTest("nonNegIntPadding01") }
  @Test def test_nonNegIntPadding03() { runner.runOneTest("nonNegIntPadding03") }
  
//DFDL-191
  @Test def test_infnanCaseInsensitive() { runner.runOneTest("infnanCaseInsensitive") }
  @Test def test_expCaseSensitive() { runner.runOneTest("expCaseSensitive") }

// DFDL-865
  @Test def test_textNumberPattern_facets01() { runner.runOneTest("textNumberPattern_facets01") }
  @Test def test_textNumberPattern_facets02() { runner.runOneTest("textNumberPattern_facets02") }

// DFDL-861
  @Test def test_standardZeroRep03() { runner.runOneTest("standardZeroRep03") }

// DFDL-868
  @Test def test_standardZeroRep09() { runner.runOneTest("standardZeroRep09") }

}
