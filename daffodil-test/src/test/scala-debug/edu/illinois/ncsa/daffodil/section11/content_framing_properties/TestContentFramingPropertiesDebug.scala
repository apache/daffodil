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

package edu.illinois.ncsa.daffodil.section11.content_framing_properties

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestContentFramingPropertiesDebug {
  private val testDir_01 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  lazy val runner1 = Runner(testDir_01, "dpaext1.tdml")

  private val testDir_02 = "/edu/illinois/ncsa/daffodil/section11/content_framing_properties/"
  lazy val runner2 = Runner(testDir_02, "ContentFramingProps.tdml")

  @AfterClass def shutdown {
    runner1.reset
    runner2.reset
  }
}

class TestContentFramingPropertiesDebug {

  import TestContentFramingPropertiesDebug._

  //  DFDL-751 - 7-bit ASCII alignment should be 1 bit, complains that it needs to be 8 bits
  @Test def test_alignmentPacked7BitASCII_02() { runner2.runOneTest("alignmentPacked7BitASCII_02") }
  @Test def test_alignmentPacked7BitASCII_05() { runner2.runOneTest("alignmentPacked7BitASCII_05") }

  // DFDL-1620: IllegalArgumentException with invalid character with 7-Bit encoding
  @Test def test_packed7BitASCII_unparse2() { runner2.runOneTest("packed7BitASCII_unparse2") }

  // Implementing DFDL-951 caused regression - see DFDL-965
  @Test def test_xml_utf8_4byte_chars() { runner2.runOneTest("xml_utf8_4byte_chars") }
  @Test def test_xml_utf8_4byte_chars_01() { runner2.runOneTest("xml_utf8_4byte_chars_01") }

  // JIRA Ticket DFDL-1386 - 4-byte utf-8 characters/surrogate-pair issue.
  @Test def test_encodingNoError() { runner2.runOneTest("encodingNoError") }
}
