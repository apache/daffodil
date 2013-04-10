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
import java.io.File
import expectj.ExpectJ
import expectj.Spawn

class TestCLIparsing {
    
  val output1 = Util.getExpectedString("output1.txt")
  val output2 = Util.getExpectedString("output2.txt")
  val output4 = Util.getExpectedString("output4.txt")
  val output5 = Util.getExpectedString("output5.txt")
  val output6 = Util.getExpectedString("output6.txt")

  @Test def test_977_CLI_Parsing_SimpleParse_stdOut() {

    var cmd = "echo 0,1,2| daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix\n"
    val shell = Util.getShell(cmd)
  
    shell.expect(output1)
    shell.send("exit\n")
    shell.expectClose()
  }
/*  
  @Test def test_978_CLI_Parsing_SimpleParse_outFile() {

    val tmp_filename: Long = System.currentTimeMillis / 1000
//    val cmd = "echo 0,1,2| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -o " + tmp_filename + "\n"
    val shell = Util.getShell(cmd)

//    shell.send("cat " + tmp_filename + "\n")
//    shell.expect(output1)
//    shell.send("rm -f " + tmp_filename + "\n")
    shell.send("exit\n")
    shell.expectClose()
  }
*/
  @Test def test_979_CLI_Parsing_SimpleParse_inFile() {
    val cmd = "./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.getShell(cmd) 
    shell.expect(output1)

    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_980_CLI_Parsing_SimpleParse_stOutDash() {
    val cmd = "./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -o - daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.getShell(cmd)
    shell.expect(output1)
    
    shell.send("exit\n")
    shell.expectClose()
  }
 
  @Test def test_981_CLI_Parsing_SimpleParse_stdInDash() {
    val cmd = "echo 0,1,2,3| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -\n"
    val shell = Util.getShell(cmd)
    shell.expect(output2)
    
    shell.send("exit\n")
    shell.expectClose()
  }
/* 
  @Test def test_983_CLI_Parsing_SimpleParse_verboseMode() {

    val cmd = "echo 0,1,2,3| ./daffodil-core/target/start -vvv parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -\n" 
    val shell = Util.getShell(cmd)
    shell.expectErr("element.row")
    
    shell.send("exit\n")
    shell.expectClose()
  }
*/
  
  @Test def test_984_CLI_Parsing_negativeTest() {
    val cmd = "echo 0,1,2,3| ./daffodil-core/target/start parse\n"
    val shell = Util.getShell(cmd)
    shell.expectErr("one of --schema or --parser must be defined")
    
    shell.send("exit\n")
    shell.expectClose()
  }
  
  @Test def test_985_CLI_Parsing_SimpleParse_defaultRoot() {
    val cmd = "echo 0,1,2,3| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd\n"
    val shell = Util.getShell(cmd)
    shell.expect(output2)
    
    shell.send("exit\n")
    shell.expectClose()
  }
 
  @Test def test_988_CLI_Parsing_SimpleParse_specifiedRoot() {
    val expected = """<tns:hcp xmlns:tns="http://www.example.org/example1/">12</tns:hcp>"""
    val cmd = "echo 12| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r hcp\n"
    val shell = Util.getShell(cmd)
    shell.expect(expected)
    
    shell.send("exit\n")
    shell.expectClose()
  }
 
  @Test def test_989_CLI_Parsing_negativeTest02() {
    val cmd = "echo 12| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -n 'http://www.example.org/example1/'\n"
    val shell = Util.getShell(cmd)
    shell.expectErr("--root must be defined if --namespace is defined")
    
    shell.send("exit\n")
    shell.expectClose()
  }
  
  @Test def test_996_CLI_Parsing_negativeTest04() {
    val cmd = "echo 12| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r unknown\n"
    val shell = Util.getShell(cmd)
    shell.expectErr("No root element found for unknown in any available namespace")
    
    shell.send("exit\n")
    shell.expectClose()
  }
 /* 
  @Test def test_997_CLI_Parsing_multSchemas() {
    val expected = """<tns:hcp xmlns:tns="http://www.example.org/example1/">12</tns:hcp>"""
    val cmd = "echo 12| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r hcp\n"
    val shell = Util.getShell(cmd)
    shell.expect(expected)
    
    shell.send("exit\n")
    shell.expectClose()

    val cmd2 = "echo 118*Ridgewood Circle*Rochester*NY| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r address\n"
    val shell2 = Util.getShell(cmd2)
    shell2.expect(output4)
    
    shell2.send("exit\n")
    shell2.expectClose()
  }
  */
 
  @Test def test_1002_CLI_Parsing_negativeTest03() {
    val cmd  = "echo 0,1,2| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -P parserThatDoesNotExist\n"
    val shell = Util.getShell(cmd)
    shell.expectErr("only one of --parser and --schema may be defined")
    
    shell.send("exit\n")
    shell.expectClose()
  }
 
  @Test def test_1003_CLI_Parsing_SimpleParse_emptyNamespace() {
    //This is one of those rare cases where we cannot make a command work for both Windows and Linux
    val cmdWin = """./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r address -n "" daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"""
    val cmdLin = "./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -r address -n '' daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"
    val cmd = if (Util.isWindows) cmdWin else cmdLin
    val shell = Util.getShell(cmd)
    shell.expect(output4)
    
    shell.send("exit\n")
    shell.expectClose()
  }
 
  @Test def test_1004_CLI_Parsing_SimpleParse_namespaceUsed() {
    val cmd = "./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd -r matrix -n target daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt\n"
    val shell = Util.getShell(cmd)
    shell.expect(output6)
    
    shell.send("exit\n")
    shell.expectClose()
  }
/* 
  @Test def test_1005_CLI_Parsing_SimpleParse_rootPath() {
    val expected = """<tns:hcp xmlns:tns="http://www.example.org/example1/">12</tns:hcp>"""
    val cmd = "echo 12| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r hcp -p /\n"
    val shell = Util.getShell(cmd)
    shell.expect(expected)
    
    shell.send("exit\n")
    shell.expectClose()
  }
*/  
  @Test def test_1015_CLI_Parsing_SimpleParse_defaultRootMultSchema() {
    val cmd = "./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n" 
    val shell = Util.getShell(cmd)
    shell.expect(output4)
    
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1267_CLI_Parsing_MultifileSchema_basicTest() {
    val cmd = "echo random,data,should,work| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_08.dfdl.xsd --root base -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_A_08.dfdl.xsd\n"
    val shell = Util.getShell(cmd)
    shell.expect(output5)
    
    shell.send("exit\n")
    shell.expectClose()
  }
/* 
  @Test def test_1313_CLI_Parsing_assertionFailure() {
    val cmd = "echo unacceptable| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_B_08.dfdl.xsd -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_C_08.dfdl.xsd --root bElem2\n"
    val shell = Util.getShell(cmd)
    shell.expectErr("Parse Error: Assertion failed. Assertion failed for dfdl:checkConstraints(.)")
    
    shell.send("exit\n")
    shell.expectClose()
  }
*/
  @Test def test_1319_CLI_Parsing_invalidElementSDE() {
    val cmd = "echo ababababbaacccccb| ./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/ABC_IBM_invalid.xsd -r ABC\n"
    val shell = Util.getShell(cmd)
    shell.expectErr("The value 'fixed' of attribute 'maxOccurs' on element 'xsd:element' is not valid with respect to its type")
    
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1346_CLI_Parsing_SimpleParse_defaultRootMultSchemaMultiple() {
    var x = 0
    for (x <- 1 to 10){
      println("Run " + x + " of 10") 
      val cmd = "./daffodil-core/target/start parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input7.txt\n"
      val shell = Util.getShell(cmd)
      shell.expect(output4)
    
      shell.send("exit\n")
      shell.expectClose()
    }
  }
  
  @Test def test_1386_CLI_Parsing_negativeTest05() {
    val cmd = "echo 12| ./daffodil-core/target/start\n"
    val shell = Util.getShell(cmd)
    shell.expectErr("subcommand")
    
    shell.send("exit\n")
    shell.expectClose()
  }

}
