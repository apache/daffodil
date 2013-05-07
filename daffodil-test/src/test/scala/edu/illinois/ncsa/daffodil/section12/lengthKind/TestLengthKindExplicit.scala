package edu.illinois.ncsa.daffodil.section12.lengthKind

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
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger.withDebugger
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestLengthKindExplicit {
  val testDir = "/edu/illinois/ncsa/daffodil/section12/lengthKind/"
  val aa = testDir + "ExplicitTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_Lesson1_lengthKind_explicit() { runner.runOneTest("Lesson1_lengthKind_explicit") }
  @Test def test_ExplicitLengthBytesNotFixed() = { runner.runOneTest("test_ExplicitLengthBytesNotFixed") }
  @Test def test_ExplicitLengthBytesFixed() = { runner.runOneTest("test_ExplicitLengthBytesFixed") }
  @Test def test_lengthRuntimeSDE() = { runner.runOneTest("test_lengthRuntimeSDE") }
  @Test def test_ExplicitLengthBytesBroken() = { runner.runOneTest("test_ExplicitLengthBytesBroken") }
  @Test def test_ExplicitLengthBytesNotGiven() = { runner.runOneTest("test_ExplicitLengthBytesNotGiven") }
  @Test def test_ExplicitLengthBytesChoiceRef() = { runner.runOneTest("test_ExplicitLengthBytesChoiceRef") }
  @Test def test_ExplicitLengthChildLengthLessParent_Chars() = { runner.runOneTest("test_ExplicitLengthChildLengthLessParent_Chars") }
  @Test def test_ExplicitLengthChildLengthLessParent_Bits() = { runner.runOneTest("test_ExplicitLengthChildLengthLessParent_Bytes") }
  @Test def test_ExplicitLengthChildLengthMoreParent_Chars() = { runner.runOneTest("test_ExplicitLengthChildLengthMoreParent_Chars") }

}
