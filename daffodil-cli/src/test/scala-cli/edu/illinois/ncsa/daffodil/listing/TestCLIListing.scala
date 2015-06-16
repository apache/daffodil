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

package edu.illinois.ncsa.daffodil.listing

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.CLI.Util._
import java.io.File
import edu.illinois.ncsa.daffodil.CLI.Util
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLIlisting {

  @Test def test_992_CLI_Executing_Listing_singleTestList() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile
    val shell = Util.start("")

    try {
      val cmd = String.format("%s test -l %s byte_entities_6_08", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains("byte_entities_6_08"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }
  /* 
  @Test def test_993_CLI_Executing_Listing_listAll() {
    val shell = ex.spawn("/bin/bash")
    shell.send("/bin/grep -c 'parserTestCase>' daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml\n")
    val num = shell.getCurrentStandardOutContents()
    shell.send(Util.binPath + " test -l daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml | tee /dev/tty | wc -l\n")
    shell.expect(num)
    shell.send("exit\n")
    shell.expectClose()
  }
  
  @Test def test_999_CLI_Executing_Listing_listRegex01() {
    val cmd = ""Util.binPath + " test -l --regex daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml "escape_entry4-\\d+"\n"""
    val shell = Util.start(cmd)
    shell.expect("escape_entry4-20")
    shell.expect("escape_entry4-21")
    shell.send("exit\n")
    shell.expectClose()
  }
  
  @Test def test_1000_CLI_Executing_Listing_listRegex02() {
    val cmd = Util.binPath + " test -l --regex daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml 'escape_entryb-\\d+'\n"
    val shell = Util.start(cmd)
    val output = shell.getCurrentStandardOutContents()
    if (output != ""){
      throw new Exception("Output does not match expected.")
    }
    shell.send("exit\n")
    shell.expectClose()
  }
*/

  @Test def test_1016_CLI_Executing_Listing_listVerbose() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section07/assertions/assert.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile
    val shell = Util.start("")

    try {
      shell.sendLine(String.format("%s test -l --regex %s assertPattern.*", Util.binPath, testTdmlFile))
      shell.expect(contains("assertPatternAndExp"))

      shell.sendLine(String.format("%s test -l -i --regex %s assertPattern.*", Util.binPath, testTdmlFile))
      shell.expect(contains("assertPatternAndExp              s2                e3         Section 7 - Assert Schema Error for Expression/Pattern - DFDL-7-047R"))

      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }
}
