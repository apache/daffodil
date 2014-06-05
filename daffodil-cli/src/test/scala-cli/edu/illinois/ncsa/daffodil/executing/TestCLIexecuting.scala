package edu.illinois.ncsa.daffodil.CLI.executing

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
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.CLI.Util._
import edu.illinois.ncsa.daffodil.CLI.Util._
import java.io.File
import java.io.IOException
import java.lang.AssertionError
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.CLI.Util
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.anyString
import net.sf.expectit.matcher.Matchers.regexp

class TestCLIexecuting {

  val testDir = "daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/"
  val output3 = Util.getExpectedString("output3.txt")
  val output13 = Util.getExpectedString("output13.txt")
  val output14 = Util.getExpectedString("output14.txt")
  val output15 = Util.getExpectedString("output15.txt")
  val output16 = Util.getExpectedString("output16.txt")

  @Test def test_995_CLI_Executing_Listing_negativeTest01() {
    val cmd = "./daffodil-cli/target/start test daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml escape_entry1 escape_entry2-11 escape_entry1-5 escape_entry4_3\n"
    val shell = Util.start(cmd)
    shell.expect(contains(output3))
    shell.send("exit\n")
    shell.close()
  }

  @Test def test_1001_CLI_Executing_Listing_execRegex01() {
    val cmd = "./daffodil-cli/target/start test --regex daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml 'escape_entry4_\\d'\n"
    val shell = Util.start(cmd)
    shell.expect(contains(output13))
    shell.send("exit\n")
    shell.close()
  }
  
  @Test def test_1000_CLI_Executing_Listing_listRegex02() {
    val cmd = "./daffodil-cli/target/start test -l --regex daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml 'escape_entryb-\\d+'\n"
    val shell = Util.start(cmd)
    try {
      shell.expect(5000, anyString())
    } catch {
      case ex: AssertionError => {
        //Didn't find a string which is what we wanted
	shell.send("exit\n")
	shell.close()
	return
      }
    }
    shell.send("exit\n")
    shell.close()
    fail("Output was found when none was expected.")
  }
  
  @Test def test_999_CLI_Executing_Listing_listRegex01() {
    val cmd = "./daffodil-cli/target/start test -l --regex daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml 'escape_entry4_\\d+'\n"
    val shell = Util.start(cmd)
    shell.expect(contains(output14))
    shell.send("exit\n")
    shell.close()
  }
  
  @Test def test_994_CLI_Executing_Listing_execAll() {
    val cmd = "./daffodil-cli/target/start test daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml\n"
    val shell = Util.start(cmd)
    shell.expect(contains(output15))
    shell.send("exit\n")
    shell.close()
  }
  
  @Test def test_993_CLI_Executing_Listing_listAll() {
    val cmd = "./daffodil-cli/target/start test -l daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml\n"
    val shell = Util.start(cmd)
    shell.expect(contains(output16))

    shell.send("./daffodil-cli/target/start test -l daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml | wc -l\n")
    val result = shell.expect(regexp("[0-9]{2}"))
    val numTests = result.group()

    shell.send("grep -c \"parserTestCase>\" daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml\n")
    val result2 = shell.expect(regexp("[0-9]{2}"))
    val numFile = result2.group()

    assertTrue("Number of tests run should match the number of tests in the file.", numTests == numFile)

    shell.send("exit\n")
    shell.close()
  }
  
  @Test def test_990_CLI_Executing_Listing_singleTest() {
    val cmd = "./daffodil-cli/target/start test daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml byte_entities_6_08\n"
    val shell = Util.start(cmd)
    shell.expect(contains("[Pass] byte_entities_6_08"))
    shell.send("exit\n")
    shell.close()
  }
  
}
