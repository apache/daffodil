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

package edu.illinois.ncsa.daffodil.section05.simple_types

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
import edu.illinois.ncsa.daffodil.Implicits._

class TestSimpleTypes2 {
  val testDir = "/edu/illinois/ncsa/daffodil/section05/simple_types/"
  val aa = testDir + "SimpleTypes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_warning_exercise() {
    val exc = intercept[Exception] {
      runner.runOneTest("warning_exercise")
    }
    assertTrue(exc.getMessage().contains("Did not find"))
  }

  //////////////////////// DFDL-529 /////////////////////////////

  @Test def test_dateCalendarCenturyStart() { runner.runOneTest("dateCalendarCenturyStart") }
  @Test def test_dateCalendarCenturyStart2() { runner.runOneTest("dateCalendarCenturyStart2") }

  //////////////////////// DFDL-845 ////////////////////////////
  @Test def test_whiteSpaceAfterValidInt() { runner.runOneTest("whiteSpaceAfterValidInt") }
  @Test def test_whiteSpaceAfterValidLong() { runner.runOneTest("whiteSpaceAfterValidLong") }
  @Test def test_whiteSpaceAfterValidShort() { runner.runOneTest("whiteSpaceAfterValidShort") }
  @Test def test_whiteSpaceAfterValidUnsignedInt() { runner.runOneTest("whiteSpaceAfterValidUnsignedInt") }
  @Test def test_whiteSpaceAfterValidUnsignedShort() { runner.runOneTest("whiteSpaceAfterValidUnsignedShort") }
  @Test def test_whiteSpaceAfterValidUnsignedByte() { runner.runOneTest("whiteSpaceAfterValidUnsignedByte") }
  @Test def test_whiteSpaceAfterValidByte() { runner.runOneTest("whiteSpaceAfterValidByte") }
  @Test def test_whiteSpaceAfterValidUnsignedLong() { runner.runOneTest("whiteSpaceAfterValidUnsignedLong") }
  @Test def test_whiteSpaceAfterValidInteger() { runner.runOneTest("whiteSpaceAfterValidInteger") }

  @Test def test_whiteSpaceDuringValidShort() { runner.runOneTest("whiteSpaceDuringValidShort") }
  @Test def test_whiteSpaceDuringValidInteger() { runner.runOneTest("whiteSpaceDuringValidInteger") }
  @Test def test_whiteSpaceDuringValidUnsignedLong() { runner.runOneTest("whiteSpaceDuringValidUnsignedLong") }
  @Test def test_whiteSpaceDuringValidUnsignedShort() { runner.runOneTest("whiteSpaceDuringValidUnsignedShort") }
  @Test def test_whiteSpaceDuringValidUnsignedByte() { runner.runOneTest("whiteSpaceDuringValidUnsignedByte") }
  @Test def test_whiteSpaceDuringValidUnsignedInt() { runner.runOneTest("whiteSpaceDuringValidUnsignedInt") }
  @Test def test_whiteSpaceDuringValidInt() { runner.runOneTest("whiteSpaceDuringValidInt") }
  @Test def test_whiteSpaceDuringValidLong() { runner.runOneTest("whiteSpaceDuringValidLong") }
  @Test def test_whiteSpaceDuringValidByte() { runner.runOneTest("whiteSpaceDuringValidByte") }

  /////////////////////// DFDL-1042 /////////////////////////////////////////////

  @Test def test_dateStrictCheckPolicy01() { runner.runOneTest("dateStrictCheckPolicy01") }
  @Test def test_timeStrictCheckPolicy01() { runner.runOneTest("timeStrictCheckPolicy01") }
  @Test def test_timeStrictCheckPolicy02() { runner.runOneTest("timeStrictCheckPolicy02") }
  @Test def test_timeFormatting5() { runner.runOneTest("timeFormatting5") }
  
  ///////////////////////////////////////////////////////////////////////////////////

  @Test def test_posinteger_binary_01() { runner.runOneTest("nonNegInt_binary_01") }

  val bb = testDir + "WhiteSpace.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(bb))
  @Test def test_whiteSpaceAfterLax() { runner2.runOneTest("whiteSpaceAfterLax") }
  @Test def test_redefinedFormat() { runner2.runOneTest("redefinedFormat") }

  val cc = testDir + "BitOrder.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(cc))
  @Test def test_bigEndianLeastFirst() { runner1.runOneTest("bigEndianLeastFirst") }
  @Test def test_bitOrderChange() { runner1.runOneTest("bitOrderChange") }
  @Test def test_bitOrderDocument() { runner1.runOneTest("bitOrderDocument") }
  @Test def test_bitOrderTypeByte() { runner1.runOneTest("bitOrderTypeByte") }
  @Test def test_bitOrderChangeInvalid3() { runner1.runOneTest("bitOrderChangeInvalid3") }

  val dd = testDir + "BitOrderInvalid.tdml"
  lazy val runner3 = new DFDLTestSuite(Misc.getRequiredResource(dd))
  @Test def test_bitOrderChangeInvalid() { runner3.runOneTest("bitOrderChangeInvalid") }
}
