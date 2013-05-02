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

class TestGeneral {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val aa = testDir + "general.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

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

  @Test def test_dir_and_file_with_spaces() { runnerA_B.runOneTest("AB006") }

}
