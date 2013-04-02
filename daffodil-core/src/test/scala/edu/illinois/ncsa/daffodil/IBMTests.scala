package edu.illinois.ncsa.daffodil

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

import java.io.File
import org.scalatest.junit.JUnitSuite
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import org.junit.Test

class IBMTestsThatPass extends JUnitSuite {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
  
//  @Test def test_simple_type_properties_text_calendar_13_01() { runner2.runOneTest("simple_type_properties_text_calendar_13_01") } // DFDL-622 - date/time/dateTime infoset output is incorrect
//  @Test def test_simple_type_properties_text_calendar_13_02() { runner2.runOneTest("simple_type_properties_text_calendar_13_02") } // DFDL-622 - date/time/dateTime infoset output is incorrect
//  @Test def test_simple_type_properties_text_calendar_13_03() { runner2.runOneTest("simple_type_properties_text_calendar_13_03") } // DFDL-620 - Implicit time calendarPattern should use ZZZ, not zzz.
//  @Test def test_simple_type_properties_text_calendar_13_04() { runner2.runOneTest("simple_type_properties_text_calendar_13_04") } // DFDL-622 - date/time/dateTime infoset output is incorrect

  @Test def test_introduction_1_01() { runner1.runOneTest("introduction_1_01") }

  @Test def test_property_syntax_7_04() { runner1.runOneTest("property_syntax_7_04") }

  // Used to work, but now we get NYI for attributeFormDefault='qualified'
  // @Test def test_scoping_default_format_8_01() { runner1.runOneTest("scoping_default_format_8_01") }
  // @Test def test_scoping_define_format_8_01() { runner1.runOneTest("scoping_define_format_8_01") }

  @Test def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }

  @Test def test_length_implicit_12_01() { runner1.runOneTest("length_implicit_12_01") }

  @Test def test_length_explicit_12_03() { runner1.runOneTest("length_explicit_12_03") }

  @Test def test_simple_type_properties_pad_trim_13_01() { runner2.runOneTest("simple_type_properties_pad_trim_13_01") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_02() { runner2.runOneTest("simple_type_properties_pad_trim_13_02") } // xs:integer type
  @Test def test_simple_type_properties_pad_trim_13_03() { runner2.runOneTest("simple_type_properties_pad_trim_13_03") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_04() { runner2.runOneTest("simple_type_properties_pad_trim_13_04") } // pad/trim

  @Test def test_simple_type_properties_binary_number_13_02() { runner2.runOneTest("simple_type_properties_binary_number_13_02") }

  @Test def test_sequences_separated_14_01() { runner2.runOneTest("sequences_separated_14_01") }
  @Test def test_sequences_separated_14_02() { runner2.runOneTest("sequences_separated_14_02") }
  @Test def test_sequences_separated_14_07() { runner2.runOneTest("sequences_separated_14_07") }
  @Test def test_sequences_separated_14_08() { runner2.runOneTest("sequences_separated_14_08") }
}

