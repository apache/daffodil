package edu.illinois.ncsa.daffodil

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

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import org.junit.Test

/**
 * Delete this class once these regressions have been addressed.
 */
//class IBMTestRegressions {
//  
//  val testDir = "/test-suite/ibm-contributed/"
//  val tdml1 = testDir + "dpaext1.tdml"
//  val tdml2 = testDir + "dpaext2.tdml"
//  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
//  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
//
//  // Nothing here! Great!
//}

class IBMTestsThatThrow {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_scoping_define_format_8_02() { runner1.runOneTest("scoping_define_format_8_02") } //DFDL-565 - attributeFormDefault='qualified', packed
  @Test def test_scoping_define_format_8_03() { runner1.runOneTest("scoping_define_format_8_03") } //DFDL-565 - attributeFormDefault='qualified', packed
  @Test def test_scoping_define_format_8_04() { runner1.runOneTest("scoping_define_format_8_04") } //DFDL-565 - attributeFormDefault='qualified'

  @Test def test_scoping_default_format_8_01() { runner1.runOneTest("scoping_default_format_8_01") } // DFDL-565 attributeFormDefault='qualified'
  @Test def test_scoping_define_format_8_01() { runner1.runOneTest("scoping_define_format_8_01") } // DFDL-565 attributeFormDefault='qualified'

  @Test def test_alignment_bytes_12_04() { runner1.runOneTest("alignment_bytes_12_04") } //DFDL-461 binary boolean
  @Test def test_alignment_bytes_12_05() { runner1.runOneTest("alignment_bytes_12_05") } //DFDL-99 binary dateTime
  @Test def test_delimiter_12_02() { runner1.runOneTest("delimiter_12_02") } //ignoreCase='yes'

  @Test def test_length_implicit_12_02() { runner1.runOneTest("length_implicit_12_02") } // implicit length string - bug in IBM test (doesn't have minLength - both are required)

  @Test def test_length_delimited_12_05() { runner1.runOneTest("length_delimited_12_05") } // DFDL-629 binaryNumberRep="bcd"

  @Test def test_simple_type_properties_text_number_13_01() { runner2.runOneTest("simple_type_properties_text_number_13_01") } // DFDL-452 text number advanced props
  @Test def test_simple_type_properties_text_number_13_02() { runner2.runOneTest("simple_type_properties_text_number_13_02") } // DFDL-452 textStandardInfinityRep, textStandardZeroRep, textNumberPattern
  @Test def test_simple_type_properties_text_number_13_03() { runner2.runOneTest("simple_type_properties_text_number_13_03") } // DFDL-452 textStandardBase (base 16)
  @Test def test_simple_type_properties_binary_number_13_01() { runner2.runOneTest("simple_type_properties_binary_number_13_01") } // DFDL-629 binaryNumberRep="bcd"
  
  @Test def test_simple_type_properties_text_boolean_13_01() { runner2.runOneTest("simple_type_properties_text_boolean_13_01") } // DFDL-462 boolean type
  @Test def test_simple_type_properties_text_boolean_13_02() { runner2.runOneTest("simple_type_properties_text_boolean_13_02") } // DFDL-462 boolean type
  @Test def test_simple_type_properties_text_boolean_13_03() { runner2.runOneTest("simple_type_properties_text_boolean_13_03") } // DFDL-462 boolean type
  @Test def test_simple_type_properties_bin_boolean_13_01() { runner2.runOneTest("simple_type_properties_bin_boolean_13_01") } // DFDL-461 boolean type
  @Test def test_simple_type_properties_bin_calendar_13_01() { runner2.runOneTest("simple_type_properties_bin_calendar_13_01") } // DFDL-99 dateTime
  @Test def test_simple_type_properties_bin_calendar_13_02() { runner2.runOneTest("simple_type_properties_bin_calendar_13_02") } // DFDL-99 dateTime

  @Test def test_sequences_separated_14_04() { runner2.runOneTest("sequences_separated_14_04") } // left over data

}
