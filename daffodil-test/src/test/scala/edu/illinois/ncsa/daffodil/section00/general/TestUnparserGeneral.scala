/* Copyright (c) 2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section00.general

import junit.framework.Assert._
import org.junit.Test
import org.junit.AfterClass
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

object TestUnparserGeneral {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val aa = testDir + "testUnparserGeneral.tdml"
  var runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @AfterClass def shutDown {
    runner = null
  }
}
class TestUnparserGeneral {

  import TestUnparserGeneral._

  @Test def test_unparseFixedLengthString01() { runner.runOneTest("unparseFixedLengthString01") }
  @Test def test_unparseFixedLengthString02() { runner.runOneTest("unparseFixedLengthString02") }

  //DFDL-1306
  @Test def test_unparseFixedLengthString03() { runner.runOneTest("unparseFixedLengthString03") }

  @Test def test_negativeUnparseTest01() { runner.runOneTest("negativeUnparseTest01") }
  @Test def test_negativeUnparseTest02() { runner.runOneTest("negativeUnparseTest02") }
  @Test def test_negativeUnparseTest05() { runner.runOneTest("negativeUnparseTest05") }

  //DFDL-1294
  @Test def test_negativeUnparseTest03() { runner.runOneTest("negativeUnparseTest03") }
  @Test def test_negativeUnparseTest04() { runner.runOneTest("negativeUnparseTest04") }

  @Test def test_unparseDelimitedString01() { runner.runOneTest("unparseDelimitedString01") }
  @Test def test_unparseDelimitedPaddedString01() { runner.runOneTest("unparseDelimitedPaddedString01") }
  @Test def test_unparseDelimitedEscapedString01() { runner.runOneTest("unparseDelimitedEscapedString01") }
  @Test def test_unparseDelimitedEscapedString02() { runner.runOneTest("unparseDelimitedEscapedString02") }

  @Test def test_parseDelimitedString01() { runner.runOneTest("parseDelimitedString01") }
  @Test def test_parseDelimitedPaddedString01() { runner.runOneTest("parseDelimitedPaddedString01") }
  @Test def test_parseDelimitedEscapedString01() { runner.runOneTest("parseDelimitedEscapedString01") }

}

