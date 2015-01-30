package edu.illinois.ncsa.daffodil.saving

/* Copyright (c) 2014 Tresys Technology, LLC. All rights reserved.
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
import org.junit.Before
import org.junit.After
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
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof
import java.nio.file.Files

class TestCLISaveParser {

  val output1 = Util.getExpectedString("output1.txt")
  val output4 = Util.getExpectedString("output4.txt")
  val output6 = Util.getExpectedString("output6.txt")
  val output12 = Util.getExpectedString("output12.txt")
  val savedParserFile = new File("savedParser.xml")

  @Before def before() {
    savedParserFile.delete
  }

  @After def after() {
    savedParserFile.delete
  }

  @Test def test_3017_CLI_Saving_SaveParser_simple() {

    var cmd = Util.binPath + " save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix savedParser.xml\n"
    val shell = Util.start(cmd)

    var cmd2 = "echo 0,1,2| " + Util.binPath + " parse --parser savedParser.xml\n"
    shell.send(cmd2)

    shell.expect(contains("<tns:matrix"))
    shell.expect(contains(output1))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3018_CLI_Saving_SaveParser_stdout() {

    val shell = Util.startIncludeErrors("")
    var savedParser = "external_variables.dfdl.xsd.bin"
    val parserFile = new File(savedParser)
    val schemaFile = "daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/external_variables/external_variables.dfdl.xsd"
    var saveCmd = String.format("%s -v save-parser -s %s > %s\n",
      Util.binPath,
      (if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile),
      savedParser)

    try {
      shell.send(saveCmd)
      shell.expectIn(1, (contains("[info] Time")))
      assertTrue("save-parser failed", parserFile.exists())
      var parseCmd = String.format("echo 0,1,2| %s parse --parser %s\n", Util.binPath, savedParser)
      shell.send(parseCmd)
      shell.expect(contains("<tns:row"))
      shell.expect(contains("<cell>0</cell>"))
      shell.expect(contains("<cell>-1</cell>"))
      shell.expect(contains("<cell>-2</cell>"))
      shell.expect(contains("</tns:row>"))
      shell.send("exit\n")
      shell.expect(eof())
    } finally {
      shell.close()
      if (parserFile.exists()) parserFile.delete()
    }
  }

  @Test def test_3019_CLI_Saving_SaveParser_withConfig() {

    var cmd = Util.binPath + " save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/external_variables/external_variables.dfdl.xsd -r row2 -c daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/external_variables/daffodil_config_cli_test.xml savedParser.xml\n"
    val shell = Util.start(cmd)

    var cmd2 = "echo 0,1,2| " + Util.binPath + " parse --parser savedParser.xml\n"
    shell.send(cmd2)

    shell.expect(contains("<tns:row2"))
    shell.expect(contains(output12))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3020_CLI_Saving_SaveParser_namespaceUsed() {

    val cmd = Util.binPath + " save-parser -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd -r {target}matrix savedParser.xml\n"
    val shell = Util.start(cmd)

    var cmd2 = Util.binPath + " parse --parser savedParser.xml daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt\n"
    shell.send(cmd2)
    shell.expect(contains("<tns:matrix"))
    shell.expect(contains(output6))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3021_CLI_Saving_SaveParser_path() {

    val cmdLinux = Util.binPath + " save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix -p / savedParser.xml\n"
    val cmdWindows = Util.binPath + """ save-parser -s daffodil-test\src\test\resources\edu\illinois\ncsa\daffodil\section06\entities\charClassEntities.dfdl.xsd -r matrix -p / savedParser.xml"""
    val cmd = if (Util.isWindows) cmdWindows else cmdLinux
    val shell = Util.startNoConvert(cmd)

    val cmdLinux2 = "echo 0,1,2| " + Util.binPath + " parse --parser savedParser.xml\n"
    val cmdWindows2 = """echo 0,1,2| """ + Util.binPath + """ parse --parser savedParser.xml"""
    val cmd2 = if (Util.isWindows) cmdWindows2 else cmdLinux2
    shell.send(cmd2)

    shell.expect(contains("<tns:matrix"))
    shell.expect(contains(output1))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3022_CLI_Saving_SaveParser_MultSchema() {

    var cmd = Util.binPath + " save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/defineFormat/defineFormat.dfdl.xsd -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd savedParser.xml\n"
    val shell = Util.start(cmd, true)

    shell.expect(contains("Bad arguments for option 'schema'"))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3023_CLI_Saving_SaveParser_verboseMode() {

    var cmd = Util.binPath + " -v save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix\n"
    var shell = Util.start(cmd, true)
    shell.expect(contains("[info]"))
    shell.send("exit\n")
    shell.expect(eof())
    shell.close()

    cmd = Util.binPath + " -vv save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix\n"
    shell = Util.start(cmd, true)
    shell.expect(contains("[compile]"))
    shell.send("exit\n")
    shell.expect(eof())
    shell.close()

    cmd = Util.binPath + " -vvv save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix\n"
    shell = Util.start(cmd, true)
    shell.expect(contains("[debug]"))
    shell.send("exit\n")
    shell.expect(eof())
    shell.close()

    cmd = Util.binPath + " -vvvv save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix\n"
    shell = Util.start(cmd, true)
    shell.expect(contains("[oolagdebug]"))
    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  // See DFDL-1016
  /*@Test def test_3038_CLI_Saving_SaveParser_namespaceNoRoot() {
    
    var cmd = Util.binPath + " save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r {http://www.example.org/example1/} savedParser.xml\n"
    val shell = Util.start(cmd, true)
    
    // Error message needs to be updated with actual message
    shell.expect(contains("Error - Root is required if namespace is given"))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }*/

  @Test def test_3039_CLI_Saving_SaveParser_emptyNamespace() {

    val cmdLinux = Util.binPath + " save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r {}matrix -p / savedParser.xml\n"
    val cmdWindows = Util.binPath + """ save-parser -s daffodil-test\src\test\resources\edu\illinois\ncsa\daffodil\section06\entities\charClassEntities.dfdl.xsd -r {}matrix -p / savedParser.xml"""
    val cmd = if (Util.isWindows) cmdWindows else cmdLinux
    val shell = Util.startNoConvert(cmd)

    val cmdLinux2 = "echo 0,1,2| " + Util.binPath + " parse --parser savedParser.xml\n"
    val cmdWindows2 = """echo 0,1,2| """ + Util.binPath + """ parse --parser savedParser.xml"""
    val cmd2 = if (Util.isWindows) cmdWindows2 else cmdLinux2
    shell.send(cmd2)

    shell.expect(contains("<tns:matrix"))
    shell.expect(contains(output1))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }
  
  @Test def test_DFDL_1205_CLI_FullValidation_SavedParser_Incompatible() {

    val cmd = Util.binPath + " save-parser -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/charClassEntities.dfdl.xsd -r {target}matrix savedParser.xml\n"
    val shell = Util.start(cmd, true)

    var cmd2 = Util.binPath + " parse --parser savedParser.xml --validate daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt\n"
    shell.send(cmd2)
    
    shell.expect(contains("[error]"))
    shell.expect(contains("The validation mode 'Full' is invalid when using a saved parser."))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  // See DFDL-1147
  /*@Test def test_3063_CLI_Saving_SaveParser_validate() {

    var cmd = Util.binPath + " save-parser --validate on -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)
    
    var cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)
    shell.expect(contains("[warning] Validation Error: validation_check: cvc-pattern-valid"))
    shell.expect(contains("[warning] Validation Error: element.validation_check failed"))

    cmd = Util.binPath + " save-parser --validate -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)
    
    cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)
    shell.expect(contains("[warning] Validation Error: validation_check: cvc-pattern-valid"))
    shell.expect(contains("[warning] Validation Error: element.validation_check failed"))

    cmd = Util.binPath + " save-parser --validate limited -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)
    
    cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)
    shell.expect(contains("[warning] Validation Error: element.validation_check failed"))

    cmd = Util.binPath + " save-parser --validate off -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)
    
    cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }*/

  // See DFDL-1141
  /*@Test def test_3036_CLI_Saving_SaveParser_debug() {

    var cmd = Util.binPath + " -d save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix savedParser.xml\n"
    val shell = Util.start(cmd)
    
    val cmd2 = Util.binPath + " parse --parser savedParser.xml daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    shell.send(cmd2)
    shell.expect(contains("(debug)"))
    shell.send("continue\n")
    shell.send("quit\n")

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3037_CLI_Saving_SaveParser_trace() {

    var cmd = Util.binPath + " -t save-parser -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/namespaces/multi_base_15.dfdl.xsd savedParser.xml\n"
    val shell = Util.start(cmd)
    
    val cmd2 = "echo test| " + Util.binPath + " parse --parser savedParser.xml\n"
    shell.send(cmd2)
    shell.expect(contains("parser: <Element name='rabbitHole'><ComplexType>...</ComplexType></Element name='rabbitHole'>"))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }*/
}
