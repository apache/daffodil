package edu.illinois.ncsa.daffodil.CLI.parsing

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

class TestCLIparsing {

  val output1 = Util.getExpectedString("output1.txt")
  val output2 = Util.getExpectedString("output2.txt")
  val output4 = Util.getExpectedString("output4.txt")
  val output5 = Util.getExpectedString("output5.txt")
  val output6 = Util.getExpectedString("output6.txt")
  val output8 = Util.getExpectedString("output8.txt")
  val output9 = Util.getExpectedString("output9.txt")
  val output10 = Util.getExpectedString("output10.txt")

  @Test def test_1593_CLI_Parsing_MultifileSchema_noGlobalElem() {
    val tmp_filename: String = (System.currentTimeMillis / 1000).toString()
    var cmd = "daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/TopLevel.xsd -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/DefaultProperties.xsd -o " + tmp_filename + " daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/02nine_headers.txt\n"
    var shell = Util.start(cmd)

    val err = shell.getCurrentStandardErrContents()
    assertEquals(err, "")
    shell.send("exit\n")
    shell.expectClose()

    //Remove the temporary output file
    val file = new File(tmp_filename)
    assertTrue("Failed to remove temporary file: %s".format(file), file.delete)
  }

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

  @Test def test_1587_CLI_Parsing_MultifileSchema_methodImportSameDir2() {
    var cmd = "echo test| daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_16.dfdl.xsd\n"
    var shell = Util.start(cmd)

    shell.expect(output10)
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1317_IBMCompatibility_ABC_test_ibm_abc_cli() {

    var cmd = "echo abcabcabc| daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/ABC_IBM.xsd -r ABC\n"
    val shell = Util.start(cmd)

    shell.expect(output8)
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_977_CLI_Parsing_SimpleParse_stdOut() {

    var cmd = "echo 0,1,2| daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix\n"
    val shell = Util.start(cmd)

    shell.expect(output1)
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_978_CLI_Parsing_SimpleParse_outFile() {
    val tmp_filename: String = (System.currentTimeMillis / 1000).toString()
    val cmd = "echo 0,1,2| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -o " + tmp_filename + "\n"
    val shell = Util.start(cmd)
    shell.send("exit\n")
    shell.expectClose()

    val linuxOpen = "cat " + tmp_filename + "\n"
    val windowsOpen = "type " + tmp_filename + "\n"

    val openCmd = if (Util.isWindows) windowsOpen else linuxOpen
    val shell2 = Util.start(openCmd)
    shell2.expect("<tns:cell>2</tns:cell>")

    shell2.send("exit\n")
    shell2.expectClose()

    //Remove the temporary output file
    val file = new File(tmp_filename)
    assertTrue("Failed to remove temporary file: %s".format(file), file.delete)
  }

  @Test def test_979_CLI_Parsing_SimpleParse_inFile() {
    val cmd = "./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(output1)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_980_CLI_Parsing_SimpleParse_stOutDash() {
    val cmd = "./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -o - daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(output1)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_981_CLI_Parsing_SimpleParse_stdInDash() {
    val cmd = "echo 0,1,2,3| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -\n"
    val shell = Util.start(cmd)
    shell.expect(output2)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_983_CLI_Parsing_SimpleParse_verboseMode() {

    var cmd = "echo 0,1| ./daffodil-cli/target/start -v parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -\n"
    var shell = Util.start(cmd)
    shell.expectErr("[info]")
    shell.send("exit\n")
    shell.expectClose()

    cmd = "echo 0,1| ./daffodil-cli/target/start -vv parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -\n"
    shell = Util.start(cmd)
    shell.expectErr("[compile]")
    shell.send("exit\n")
    shell.expectClose()

    cmd = "echo 0,1| ./daffodil-cli/target/start -vvv parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -\n"
    shell = Util.start(cmd)
    shell.expectErr("[debug]")
    shell.send("exit\n")
    shell.expectClose()

    cmd = "echo 0,1| ./daffodil-cli/target/start -vvvv parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -\n"
    shell = Util.start(cmd)
    shell.expectErr("[oolagdebug]")
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_984_CLI_Parsing_negativeTest() {
    val cmd = "echo 0,1,2,3| ./daffodil-cli/target/start parse\n"
    val shell = Util.start(cmd)
    shell.expectErr("one of --schema or --parser must be defined")

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_985_CLI_Parsing_SimpleParse_defaultRoot() {
    val cmd = "echo 0,1,2,3| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd\n"
    val shell = Util.start(cmd)
    shell.expect(output2)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_988_CLI_Parsing_SimpleParse_specifiedRoot() {
    val expected = """<tns:hcp xmlns:tns="http://www.example.org/example1/">12</tns:hcp>"""
    val cmd = "echo 12| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r hcp\n"
    val shell = Util.start(cmd)
    shell.expect(expected)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_989_CLI_Parsing_negativeTest02() {
    val cmd = "echo 12| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -n 'http://www.example.org/example1/'\n"
    val shell = Util.start(cmd)
    shell.expectErr("--root must be defined if --namespace is defined")

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_996_CLI_Parsing_negativeTest04() {
    val cmd = "echo 12| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r unknown\n"
    val shell = Util.start(cmd)
    shell.expectErr("No root element found for unknown in any available namespace")

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_997_CLI_Parsing_multSchemas() {
    val expected = """<tns:hcp xmlns:tns="http://www.example.org/example1/">12</tns:hcp>"""
    val cmd = "echo 12| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r hcp\n"
    val shell = Util.start(cmd)
    shell.expect(expected)

    shell.send("exit\n")
    shell.expectClose()

    val cmd2 = "./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r address daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"
    val shell2 = Util.start(cmd2)
    shell2.expect(output4)

    shell2.send("exit\n")
    shell2.expectClose()
  }

  @Test def test_1002_CLI_Parsing_negativeTest03() {
    val cmd = "echo 0,1,2| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -P parserThatDoesNotExist\n"
    val shell = Util.start(cmd)
    shell.expectErr("only one of --parser and --schema may be defined")

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1003_CLI_Parsing_SimpleParse_emptyNamespace() {
    //This is one of those rare cases where we cannot make a command work for both Windows and Linux
    val cmdWin = """./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r address -n "" daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"""
    val cmdLin = "./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r address -n '' daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"
    val cmd = if (Util.isWindows) cmdWin else cmdLin
    val shell = Util.start(cmd)
    shell.expect(output4)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1004_CLI_Parsing_SimpleParse_namespaceUsed() {
    val cmd = "./daffodil-cli/target/start parse -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd -r matrix -n target daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt\n"
    val shell = Util.start(cmd)
    shell.expect(output6)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1005_CLI_Parsing_SimpleParse_rootPath() {
    val expected = """<tns:hcp xmlns:tns="http://www.example.org/example1/">12</tns:hcp>"""
    val cmdLinux = "echo -ne '12' | ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r hcp -p /\n"
    val cmdWindows = """echo 12| daffodil-cli\target\start parse -s daffodil-test\src\test\resources\edu\illinois\ncsa\daffodil\section06\entities\charClassEntities.dfdl.xsd -r hcp -p /"""

    val cmd = if (Util.isWindows) cmdWindows else cmdLinux

    val shell = Util.startNoConvert(cmd)
    shell.expect(expected)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1015_CLI_Parsing_SimpleParse_defaultRootMultSchema() {
    val cmd = "./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"
    val shell = Util.start(cmd)
    shell.expect(output4)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1267_CLI_Parsing_MultifileSchema_basicTest() {
    val cmd = "echo random,data,should,work| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_08.dfdl.xsd --root base -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_A_08.dfdl.xsd\n"
    val shell = Util.start(cmd)
    shell.expect(output5)

    shell.send("exit\n")
    shell.expectClose()
  }
  /*
  //On hold until I implement a way to set the classpath before executing
  @Test def test_1313_CLI_Parsing_assertionFailure() {
    val cmd = "echo unacceptable| ./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_B_08.dfdl.xsd -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_C_08.dfdl.xsd --root bElem2\n"
    val shell = Util.start(cmd)
    shell.expectErr("Parse Error: Assertion failed. Assertion failed for dfdl:checkConstraints(.)")
    
    shell.send("exit\n")
    shell.expectClose()
  }
*/
  @Test def test_1319_CLI_Parsing_invalidElementSDE() {
    val cmd = "echo ababababbaacccccb| ./daffodil-cli/target/start parse -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/ABC_IBM_invalid.xsd -r ABC\n"
    val shell = Util.start(cmd)
    shell.expectErr("The value 'fixed' of attribute 'maxOccurs' on element 'xsd:element' is not valid with respect to its type")

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1346_CLI_Parsing_SimpleParse_defaultRootMultSchemaMultiple() {
    var x = 0
    for (x <- 1 to 10) {
      println("Run " + x + " of 10")
      val cmd = "./daffodil-cli/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"
      val shell = Util.start(cmd)
      shell.expect(output4)

      shell.send("exit\n")
      shell.expectClose()
    }
  }

  @Test def test_1386_CLI_Parsing_negativeTest05() {
    val cmd = "echo 12| ./daffodil-cli/target/start\n"
    val shell = Util.start(cmd)
    shell.expectErr("subcommand")

    shell.send("exit\n")
    shell.expectClose()
  }

}
