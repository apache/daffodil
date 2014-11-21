package edu.illinois.ncsa.daffodil.section11.content_framing_properties

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

class TestContentFramingProperties {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val tdml1 = testDir_01 + "dpaext1.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))

  @Test def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }
  @Test def test_encoding_11_02() { runner1.runOneTest("encoding_11_02") }
  @Test def test_encoding_11_03() { runner1.runOneTest("encoding_11_03") }

  val testDir_02 = "/edu/illinois/ncsa/daffodil/section11/content_framing_properties/"
  val tdml2 = testDir_02 + "ContentFramingProps.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
  
  // Commented out due to 4byte char decode issue when implementing DFDL-951 - DFDL-965
  // @Test def test_xml_utf8_4byte_chars_01() { runner2.runOneTest("xml_utf8_4byte_chars_01") }
  // @Test def test_xml_utf8_4byte_chars() { runner2.runOneTest("xml_utf8_4byte_chars") }

  @Test def test_UTF_16_01() { runner2.runOneTest("UTF_16_01") }
  @Test def test_xml_illegal_chars_01() { runner2.runOneTest("xml_illegal_chars_01") }
  @Test def test_xml_illegal_chars_02() { runner2.runOneTest("xml_illegal_chars_02") }
  @Test def test_xml_illegal_chars() { runner2.runOneTest("xml_illegal_chars") }

  @Test def test_alignmentPacked7BitASCII() { runner2.runOneTest("alignmentPacked7BitASCII") }
  @Test def test_alignmentPacked7BitASCII_03() { runner2.runOneTest("alignmentPacked7BitASCII_03") }
  @Test def test_alignmentPacked7BitASCII_04() { runner2.runOneTest("alignmentPacked7BitASCII_04") }
  //  DFDL-751 - 7-bit ASCII alignment should be 1 bit, complains that it needs to be 8 bits
  //  @Test def test_alignmentPacked7BitASCII_02() { runner2.runOneTest("alignmentPacked7BitASCII_02") } 
  //  @Test def test_alignmentPacked7BitASCII_05() { runner2.runOneTest("alignmentPacked7BitASCII_05") } 

  /*** DFDL-379 US-ASCII-7-bit-packed text ***/
  @Test def test_packed7BitASCII1() { runner2.runOneTest("packed7BitASCII1") }
  @Test def test_packed7BitASCII2() { runner2.runOneTest("packed7BitASCII2") }
  @Test def test_packed7BitASCII3() = { runner2.runOneTest("packed7BitASCII3") }
  @Test def test_packed7BitASCII4() = { runner2.runOneTest("packed7BitASCII4") }
  @Test def test_packed7BitASCII5() = { runner2.runOneTest("packed7BitASCII5") }
  @Test def test_packed7BitASCII6() = { runner2.runOneTest("packed7BitASCII6") }

  @Test def test_packed7BitASCII7() = { runner2.runOneTest("packed7BitASCII7") }
  @Test def test_packed7BitASCII8() = { runner2.runOneTest("packed7BitASCII8") }
  @Test def test_packed7BitASCII9() = { runner2.runOneTest("packed7BitASCII9") }

  @Test def test_encoding_iso_8859_1() = { runner2.runOneTest("encoding_iso-8859-1") }

  @Test def test_encodingErrorReplace() { runner2.runOneTest("encodingErrorReplace") }
}
