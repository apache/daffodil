package edu.illinois.ncsa.daffodil.section00.general

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

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

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
import edu.illinois.ncsa.daffodil.Implicits.intercept

class TestGeneral {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val aa = testDir + "general.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_check_no_namespace_message() { runner.runOneTest("check_no_namespace_message") }

  @Test def test_capitalization() { runner.runOneTest("capitalization") }

  @Test def test_litNil1() { runner.runOneTest("litNil1") }
  @Test def test_litNil1FullPath() { runner.runOneTest("litNil1FullPath") }
  @Test def test_referentialIntegrity() { runner.runOneTest("referentialIntegrity") }

  // Test commented out until DFDL-577 is resolved
  // @Test def test_litNil2() { runner.runOneTest("litNil2") }

  // Test causes exception as the file is not found
  // @Test def test_fileDNE() { runner.runOneTest("fileDNE") }

  val bb = testDir + "largeInput.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(bb))

  @Test def test_largeInput_01() { runner1.runOneTest("largeInput_01") }

  val testDir2 = "/test space/"

  val a_b = testDir2 + "A BTinyData.tdml.dat"
  lazy val runnerA_B = new DFDLTestSuite(Misc.getRequiredResource(a_b))

  @Test def test_dir_and_file_with_spaces() {
    val e = intercept[Exception] {
      runnerA_B.runOneTest("AB006")
    }
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("required resource"))
    assertTrue(m.contains("/test%20space/A%20BTinyData.tdml.dat"))
    assertTrue(m.toLowerCase.contains("not found"))

  }

  val testDir3 = "/test space/test 1/"
  val cc = testDir3 + "namespaces.tdml"
  lazy val runner_ns = new DFDLTestSuite(Misc.getRequiredResource(cc))

  @Test def test_no_namespace_02() {
    val e = intercept[Exception] {
      runner_ns.runOneTest("no_namespace_02")
    }
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("required resource"))
    assertTrue(m.contains("/test%20space/test%201/namespaces.tdml"))
    assertTrue(m.toLowerCase.contains("not found"))
  }

  @Test def test_1530_scala_version() {
    val versionStr = scala.util.Properties.versionNumberString
    assertTrue("The scala version must begin with 2.10. The current version is: " + versionStr, versionStr.startsWith("2.10."))
  }
}
