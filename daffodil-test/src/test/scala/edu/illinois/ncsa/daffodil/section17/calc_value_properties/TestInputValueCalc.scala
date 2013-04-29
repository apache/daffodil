package edu.illinois.ncsa.daffodil.section17.calc_value_properties

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
import org.junit.Test
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.LoggingDefaults
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestInputValueCalc extends JUnitSuite {
  val testDir = "/edu/illinois/ncsa/daffodil/section17/calc_value_properties/"
  val ar = testDir + "AR.tdml"
  lazy val runnerAR = new DFDLTestSuite(Misc.getRequiredResource(ar))

  @Test def test_AR000() { runnerAR.runOneTest("AR000") }

  val aq = testDir + "AQ.tdml"
  lazy val runnerAQ = new DFDLTestSuite(Misc.getRequiredResource(aq))

  @Test def test_AQ000() { runnerAQ.runOneTest("AQ000") }

  val aa = testDir + "AA.tdml"

  lazy val runnerAA = { new DFDLTestSuite(Misc.getRequiredResource(aa)) }

  @Test def test_AA000() { runnerAA.runOneTest("AA000") }
  @Test def test_inputValueCalcErrorDiagnostic1() { runnerAA.runOneTest("inputValueCalcErrorDiagnostic1") }
  @Test def test_inputValueCalcErrorDiagnostic2() { runnerAA.runOneTest("inputValueCalcErrorDiagnostic2") }
  @Test def test_inputValueCalcAbsolutePath() { runnerAA.runOneTest("inputValueCalcAbsolutePath") }

  val tdml = testDir + "inputValueCalc.tdml"

  lazy val runner = { new DFDLTestSuite(Misc.getRequiredResource(tdml)) }

  @Test def test_InputValueCalc_01() { runner.runOneTest("InputValueCalc_01") }
  @Test def test_InputValueCalc_02() { runner.runOneTest("InputValueCalc_02") }
  @Test def test_InputValueCalc_03() { runner.runOneTest("InputValueCalc_03") }
  @Test def test_InputValueCalc_04() { runner.runOneTest("InputValueCalc_04") }
  @Test def test_InputValueCalc_05() { runner.runOneTest("InputValueCalc_05") }
  @Test def test_InputValueCalc_06() { runner.runOneTest("InputValueCalc_06") }
  //@Test def test_InputValueCalc_07() { runner.runOneTest("InputValueCalc_07")}
  @Test def test_InputValueCalc_08() { runner.runOneTest("InputValueCalc_08") }
  @Test def test_InputValueCalc_09() { runner.runOneTest("InputValueCalc_09") }
  @Test def test_InputValueCalc_10() { runner.runOneTest("InputValueCalc_10") }
  @Test def test_InputValueCalc_11() { runner.runOneTest("InputValueCalc_11") }

  @Test def test_InputValueCalc_Int_Range_Min() { runner.runOneTest("InputValueCalc_Int_Range_Min") }
  @Test def test_InputValueCalc_Int_Range_Max() { runner.runOneTest("InputValueCalc_Int_Range_Max") }
}
