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

package edu.illinois.ncsa.daffodil.section13.nillable

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
import org.junit._

object TestNillableUnparse {
  val testDir = "/edu/illinois/ncsa/daffodil/section13/nillable/"
  val ln = testDir + "literal-value-nils-unparse.tdml"
  var runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))
  
  /**
   * Avoid memory leak of adding more and more test suites to static objects as we run more and more test suites.
   */
  @AfterClass def tearDown() { 
    runnerLN = null 
  }
}

class TestNillableUnparse {

  import TestNillableUnparse._
  
  @Test def test_text_complex_nil() { runnerLN.runOneTest("text_complex_nil") }
  @Test def test_text_complex_nil2() { runnerLN.runOneTest("text_complex_nil2") }
  @Test def test_text_complex_nil3() { runnerLN.runOneTest("text_complex_nil3") }

  //DFDL-1304
  //@Test def test_text_nil_only6() { runnerLN.runOneTest("text_nil_only6") }
  //@Test def test_text_nil_only8() { runnerLN.runOneTest("text_nil_only8") }
  //@Test def test_text_nil_only10() { runnerLN.runOneTest("text_nil_only10") }
  //@Test def test_text_nil_only15() { runnerLN.runOneTest("text_nil_only15") }

  @Test def test_text_nil_only1() { runnerLN.runOneTest("text_nil_only1") }
  @Test def test_text_nil_only2() { runnerLN.runOneTest("text_nil_only2") }
  @Test def test_text_nil_only3() { runnerLN.runOneTest("text_nil_only3") }
  @Test def test_text_nil_only4() { runnerLN.runOneTest("text_nil_only4") }
  @Test def test_text_nil_only5() { runnerLN.runOneTest("text_nil_only5") }
  @Test def test_text_nil_only7() { runnerLN.runOneTest("text_nil_only7") }
  @Test def test_text_nil_only9() { runnerLN.runOneTest("text_nil_only9") }
  @Test def test_text_nil_only11() { runnerLN.runOneTest("text_nil_only11") }
  @Test def test_text_nil_only12() { runnerLN.runOneTest("text_nil_only12") }
  @Test def test_text_nil_only13() { runnerLN.runOneTest("text_nil_only13") }
  @Test def test_text_nil_only14() { runnerLN.runOneTest("text_nil_only14") }
  @Test def test_text_nil_only16() { runnerLN.runOneTest("text_nil_only16") }
  @Test def test_text_nil_only17() { runnerLN.runOneTest("text_nil_only17") }

  @Test def test_text_nil_characterClass_01() { runnerLN.runOneTest("text_nil_characterClass_01") }
  @Test def test_text_nil_characterClass_02() { runnerLN.runOneTest("text_nil_characterClass_02") }
  @Test def test_text_nil_characterClass_03() { runnerLN.runOneTest("text_nil_characterClass_03") }
  @Test def test_text_nil_characterClass_04() { runnerLN.runOneTest("text_nil_characterClass_04") }
  @Test def test_text_nil_characterClass_05() { runnerLN.runOneTest("text_nil_characterClass_05") }
  @Test def test_text_nil_characterClass_06() { runnerLN.runOneTest("text_nil_characterClass_06") }

}
