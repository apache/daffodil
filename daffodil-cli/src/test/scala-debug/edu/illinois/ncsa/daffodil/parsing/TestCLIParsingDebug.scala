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

  val output9 = Util.getExpectedString("output9.txt")
  val output10 = Util.getExpectedString("output10.txt")

  // These tests are being moved to debug because of ticket DFDL-714.
  // We changed the error handling to escalate Xerces parser warning to SDE errors.
  // Because of an imported schema that has errors in it for testing, all of these tests are now failing.
  // They will need to be updated to import only the schemas necessary to properly test each case
  // and not include those schemas that they do not use but are causing parser errors.

  @Test def test_1585_CLI_Parsing_MultifileSchema_methodImportSameDir() {
    var cmd = "echo test| daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_14.dfdl.xsd\n"
    var shell = Util.start(cmd)

    shell.expect(output9)
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1586_CLI_Parsing_MultifileSchema_methodIncludeSameDir() {
    var cmd = "echo test| daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_15.dfdl.xsd\n"
    var shell = Util.start(cmd)

    shell.expect(output10)
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1971_CLI_Parsing_traceMode01() {
    val cmd = "echo test| daffodil-cli/target/start -t parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_15.dfdl.xsd\n"
    val shell = Util.start(cmd)
    shell.expect("parser: <ElementEnd name='rabbitHole'/>")

    shell.send("exit\n")
    shell.expectClose()
    assert(shell.getExitValue() == 0)
  }

  @Test def test_1972_CLI_Parsing_traceMode02() {
    // DFDL-400: Again this was failing due to an error in one of the schemas.
    // it would appear that this test is only testing the --trace option.  So
    // it should not matter what schema we give it.  Using schemas that work.
    //
    //    val cmd = "echo random,data,should,work| daffodil-cli/target/start --trace parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_08.dfdl.xsd --root base -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_A_08.dfdl.xsd\n"
    //    val shell = Util.start(cmd)
    //    shell.expect("parser: <ElementEnd name='base'/>")
    val cmd = "echo test| daffodil-cli/target/start --trace parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_15.dfdl.xsd\n"
    val shell = Util.start(cmd)
    shell.expect("parser: <ElementEnd name='rabbitHole'/>")

    shell.send("exit\n")
    shell.expectClose()
    assert(shell.getExitValue() == 0)
  }

  @Test def test_2358_CLI_Parsing_SimpleParse_stdOut_extVars() {

    //var cmd = "echo 0,1,2| daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/external_variables/external_variables.dfdl.xsd -r row -X daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/external_variables/external_variables.xml\n"
    var cmd = "echo 0,1,2| daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/external_variables/external_variables.dfdl.xsd -r row -D\"{http://example.com}var1=99\" -c daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/external_variables/daffodil_config_cli_test.xml\n"

    val shell = Util.start(cmd)

    shell.expect(output11)

    shell.send("exit\n")
    shell.expectClose()
  }
}
