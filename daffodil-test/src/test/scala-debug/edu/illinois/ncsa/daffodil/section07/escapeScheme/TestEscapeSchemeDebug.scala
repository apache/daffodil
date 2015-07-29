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

package edu.illinois.ncsa.daffodil.section07.escapeScheme

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
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestEscapeSchemeDebug {
  val testDir = "/edu/illinois/ncsa/daffodil/section07/escapeScheme/"
  val runner = Runner(testDir, "escapeScheme.tdml", validateTDMLFile = false)
  val runnerNeg = Runner(testDir, "escapeSchemeNeg.tdml", validateTDMLFile = false)
  val runner2 = Runner(testDir, "escapeScenarios.tdml", validateTDMLFile = false)

  @AfterClass def shutDown() {
    runner.reset
    runnerNeg.reset
    runner2.reset
  }
}

class TestEscapeSchemeDebug {

  import TestEscapeSchemeDebug._

  @Test def test_escapeSchemeNonUnique() { runner.runOneTest("escapeSchemeNonUnique") }

  val bb = testDir + "escapeScenarios.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(bb), validateTDMLFile = false)

  //DFDL-961
  @Test def test_scenario3_11_postfix() { runner2.runOneTest("scenario3_11_postfix") }
}
