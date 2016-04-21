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

package edu.illinois.ncsa.daffodil.section00.general

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits.intercept
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestGeneral {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val runner = Runner(testDir, "general.tdml")

  val runner1 = Runner(testDir, "largeInput.tdml")

  val testDir2 = "/test space/"
  val runnerA_B = Runner(testDir2, "A BTinyData.tdml.dat")

  val testDir3 = "/test space/test 1/"
  val runner_ns = Runner(testDir3, "namespaces.tdml")

  @AfterClass def shutDown() {
    runner.reset
    runner1.reset
    runnerA_B.reset
    runner_ns.reset
  }
}

class TestGeneral {

  import TestGeneral._

  @Test def test_check_no_namespace_message() { runner.runOneTest("check_no_namespace_message") }

  @Test def test_capitalization() { runner.runOneTest("capitalization") }

  @Test def test_litNil1() { runner.runOneTest("litNil1") }
  @Test def test_litNil1FullPath() { runner.runOneTest("litNil1FullPath") }
  @Test def test_referentialIntegrity() { runner.runOneTest("referentialIntegrity") }

  // Test commented out until DFDL-577 is resolved
  // @Test def test_litNil2() { runner.runOneTest("litNil2") }

  // Test causes exception as the file is not found
  // @Test def test_fileDNE() { runner.runOneTest("fileDNE") }

  @Test def test_largeInput_01() { runner1.runOneTest("largeInput_01") }

  @Test def test_dir_and_file_with_spaces() {
    val e = intercept[Exception] {
      runnerA_B.runOneTest("AB006")
    }
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("required resource"))
    assertTrue(m.contains("/test%20space/A%20BTinyData.tdml.dat"))
    assertTrue(m.toLowerCase.contains("not found"))

  }

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
    assertTrue("The scala version must begin with 2.11. The current version is: " + versionStr, versionStr.startsWith("2.11."))
  }
}
