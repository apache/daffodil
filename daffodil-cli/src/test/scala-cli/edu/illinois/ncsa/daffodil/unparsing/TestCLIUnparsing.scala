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

package edu.illinois.ncsa.daffodil.unparsing

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.CLI.Util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import junit.framework.Assert.assertEquals
import java.io.File
import edu.illinois.ncsa.daffodil.CLI.Util
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLIunparsing {

  @Test def test_3525_CLI_Unparsing_SimpleUnparse_inFile() {

    val schemaFile = "daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section00/general/generalSchema.dfdl.xsd"
    val inputFile = "daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input12.txt"
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      var cmd = String.format("%s unparse -s %s --root e1 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("Hello"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }
  
  @Test def test_3526_CLI_Unparsing_SimpleUnparse_inFile2() {

    val schemaFile = "daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section00/general/generalSchema.dfdl.xsd"
    val inputFile = "daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input13.txt"
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      var cmd = String.format("%s unparse -s %s --root e3 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("[1,2]"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

}
