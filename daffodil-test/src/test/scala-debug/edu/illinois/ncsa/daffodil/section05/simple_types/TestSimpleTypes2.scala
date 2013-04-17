package edu.illinois.ncsa.daffodil.section05.simple_types

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

class TestSimpleTypes2 extends JUnitSuite {
  val testDir = "/edu/illinois/ncsa/daffodil/section05/simple_types/"
  val aa = testDir + "SimpleTypes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_warning_exercise() {
    val exc = intercept[Exception] {
      runner.runOneTest("warning_exercise")
    }
    assertTrue(exc.getMessage().contains("Did not find"))
  }
  //  @Test def test_Long3() {runner.runOneTest("Long3")}
  //  @Test def test_Long4() {runner.runOneTest("Long4")}

///////////////////////// DFDL-105, DFDL-102, DFDL-100 /////////////////////////////

  @Test def test_timeImplicitPattern() { runner.runOneTest("timeImplicitPattern") }
  @Test def test_timeLaxCheckPolicy01() { runner.runOneTest("timeLaxCheckPolicy01") }
  @Test def test_timeLaxCheckPolicy02() { runner.runOneTest("timeLaxCheckPolicy02") }
  @Test def test_timeLaxCheckPolicy03() { runner.runOneTest("timeLaxCheckPolicy03") }

  @Test def test_dateTimeLaxCheckPolicy01() { runner.runOneTest("dateTimeLaxCheckPolicy01") }

  @Test def test_datePattern04() { runner.runOneTest("datePattern04") }

//////////////////////// DFDL-529 /////////////////////////////

  @Test def test_dateCalendarCenturyStart() { runner.runOneTest("dateCalendarCenturyStart") }
  @Test def test_dateCalendarCenturyStart2() { runner.runOneTest("dateCalendarCenturyStart2") }

///////////////////////////////////////////////////////////////

  @Test def test_whiteSpaceBeforeValidInt() { runner.runOneTest("whiteSpaceBeforeValidInt") }
  @Test def test_whiteSpaceDuringValidInt() { runner.runOneTest("whiteSpaceDuringValidInt") }

  @Test def test_whiteSpaceBeforeValidLong() { runner.runOneTest("whiteSpaceBeforeValidLong") }
  @Test def test_whiteSpaceDuringValidLong() { runner.runOneTest("whiteSpaceDuringValidLong") }

  @Test def test_whiteSpaceBeforeValidShort() { runner.runOneTest("whiteSpaceBeforeValidShort") }
  @Test def test_whiteSpaceDuringValidShort() { runner.runOneTest("whiteSpaceDuringValidShort") }

  @Test def test_whiteSpaceBeforeValidUnsignedInt() { runner.runOneTest("whiteSpaceBeforeValidUnsignedInt") }
  @Test def test_whiteSpaceDuringValidUnsignedInt() { runner.runOneTest("whiteSpaceDuringValidUnsignedInt") }

  @Test def test_whiteSpaceBeforeValidUnsignedShort() { runner.runOneTest("whiteSpaceBeforeValidUnsignedShort") }
  @Test def test_whiteSpaceDuringValidUnsignedShort() { runner.runOneTest("whiteSpaceDuringValidUnsignedShort") }

  @Test def test_whiteSpaceBeforeValidUnsignedByte() { runner.runOneTest("whiteSpaceBeforeValidUnsignedByte") }
  @Test def test_whiteSpaceDuringValidUnsignedByte() { runner.runOneTest("whiteSpaceDuringValidUnsignedByte") }

  @Test def test_whiteSpaceBeforeValidValue() { runner.runOneTest("whiteSpaceBeforeValidValue") }
  @Test def test_whiteSpaceDuringValidValue() { runner.runOneTest("whiteSpaceDuringValidValue") }
  
  @Test def test_posinteger_binary_01() { runner.runOneTest("nonNegInt_binary_01") }

  @Test def test_nonNegativeInteger_text() { runner.runOneTest("nonNegativeInteger_text") }
  @Test def test_nonNegativeInteger_bin() { runner.runOneTest("nonNegativeInteger_bin") }

}
