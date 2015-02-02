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

package edu.illinois.ncsa.daffodil.section12.length_properties

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

class TestLengthPropertiesDebug {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  // uses lengthUnits bytes with lengthKind explicit and utf-8
  @Test def test_length_explicit_12_01() { runner_01.runOneTest("length_explicit_12_01") }

  val testDir_02 = "/edu/illinois/ncsa/daffodil/section12/length_properties/"
  val tdml_02 = testDir_02 + "LengthProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  // DFDL-931 Uses lengthUnits bytes with utf-8 encoding and explicit lengthKind
  @Test def test_LengthProp_02() { runner_02.runOneTest("LengthProp_02") }
  @Test def test_LengthProp_charVsBytes() { runner_02.runOneTest("LengthProp_charVsBytes") }
  @Test def test_LengthProp_charVsBytes2() { runner_02.runOneTest("LengthProp_charVsBytes2") }
  @Test def test_LengthProp_longByteLength() { runner_02.runOneTest("LengthProp_longByteLength") }

  // should just skip the 2 excess bytes
  @Test def test_LengthProp_06() { runner_02.runOneTest("LengthProp_06") }

  //DFDL-460
  @Test def test_LengthProp_floatBits() { runner_02.runOneTest("LengthProp_floatBits") }

  @Test def test_LengthProp_bits_bool() { runner_02.runOneTest("LengthProp_bits_bool") }

}
