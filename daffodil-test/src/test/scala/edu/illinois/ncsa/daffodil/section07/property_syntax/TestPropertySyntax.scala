/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section07.property_syntax

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File

class TestPropertySyntax {
  val testDir = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val tdml = testDir + "dpaext1.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_property_syntax_7_01() { runner.runOneTest("property_syntax_7_01") }
  @Test def test_property_syntax_7_02() { runner.runOneTest("property_syntax_7_02") }
  @Test def test_property_syntax_7_03() { runner.runOneTest("property_syntax_7_03") }

  val testDir1 = "/edu/illinois/ncsa/daffodil/section07/property_syntax/"
  val aa = testDir1 + "PropertySyntax.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(aa), false, false)

  @Test def test_ShortAndLongForm() { runner1.runOneTest("ShortAndLongForm") }
  @Test def test_ShortAnnotationAndElementForm() { runner1.runOneTest("ShortAnnotationAndElementForm") }
  @Test def test_AnnotationAndElementForm() { runner1.runOneTest("AnnotationAndElementForm") }
  @Test def test_ShortAndElementForm() { runner1.runOneTest("ShortAndElementForm") }
  @Test def test_Lesson3_attribute_form() { runner1.runOneTest("Lesson3_attribute_form") }
  @Test def test_Lesson3_element_form() { runner1.runOneTest("Lesson3_element_form") }
  @Test def test_Lesson3_short_form() { runner1.runOneTest("Lesson3_short_form") }
  @Test def test_encodingEmptyFail() { runner1.runOneTest("encodingEmptyFail") }

}
