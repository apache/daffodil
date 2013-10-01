package edu.illinois.ncsa.daffodil.parsing

/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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
import edu.illinois.ncsa.daffodil.CLI.Util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import junit.framework.Assert.assertEquals
import java.io.File
import edu.illinois.ncsa.daffodil.CLI.Util

class TestCLIparsingDebug {

  val output1 = Util.getExpectedString("output1.txt")
  val output2 = Util.getExpectedString("output2.txt")
  val output4 = Util.getExpectedString("output4.txt")
  val output5 = Util.getExpectedString("output5.txt")
  val output6 = Util.getExpectedString("output6.txt")
  val output8 = Util.getExpectedString("output8.txt")
  val output9 = Util.getExpectedString("output9.txt")
  val output10 = Util.getExpectedString("output10.txt")
  val output11 = Util.getExpectedString("output11.txt")
  val output12 = Util.getExpectedString("output12.txt")

  // DFDL-400: Fails due to validation error in schemas.  Is this intentional? If so this test will need to be rewritten.
  // if not, the schema(s) will need to be corrected.
  //
  @Test def test_1267_CLI_Parsing_MultifileSchema_basicTest() {
    val cmd = "echo random,data,should,work| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_08.dfdl.xsd --root base -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_A_08.dfdl.xsd\n"
    val shell = Util.start(cmd)
    shell.expect(output5)

    shell.send("exit\n")
    shell.expectClose()
  }
}
