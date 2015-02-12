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

package edu.illinois.ncsa.daffodil.performance

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
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLIPerformance {

  @Test def test_3393_CLI_Performance_2_Threads_2_Times() {

    var cmd = Util.binPath + " performance -N 2 -t 2 -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"

    val shell = Util.startIncludeErrors(cmd)

    shell.expect(contains("total parse time (sec):"))
    shell.expect(contains("avg rate (files/sec):"))
    shell.expect(contains("\n"))

    var errorFound = false
    var error = ""
    try {
      error = shell.expectIn(1, (contains("\n"))).getBefore()
      errorFound = true
    } catch {
      case ae: AssertionError => //No error was found, which is correct
    }
    if (errorFound) fail(error)

    val exitCodeCmd = if (Util.isWindows) "echo %ERRORLEVEL%\n" else "echo $?\n"
    shell.send(exitCodeCmd)
    val exitCode = shell.expect(contains("\n")).getBefore()
    if (!"0".equals(exitCode))
      fail("Tests failed. Exit code: " + exitCode)

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3394_CLI_Performance_3_Threads_20_Times() {

    var cmd = Util.binPath + " performance -N 20 -t 3 -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"

    val shell = Util.startIncludeErrors(cmd)
    shell.expect(contains("total parse time (sec):"))
    shell.expect(contains("avg rate (files/sec):"))
    shell.expect(contains("\n"))

    var errorFound = false
    var error = ""
    try {
      error = shell.expectIn(1, (contains("\n"))).getBefore()
      errorFound = true
    } catch {
      case ae: AssertionError => //No error was found, which is correct
    }
    if (errorFound) fail(error)

    val exitCodeCmd = if (Util.isWindows) "echo %ERRORLEVEL%\n" else "echo $?\n"
    shell.send(exitCodeCmd)
    val exitCode = shell.expect(contains("\n")).getBefore()
    if (!"0".equals(exitCode))
      fail("Tests failed. Exit code: " + exitCode)

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3395_CLI_Performance_5_Threads_50_Times() {

    var cmd = Util.binPath + " performance -N 50 -t 5 -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r Item2 daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input5.txt\n"

    val shell = Util.startIncludeErrors(cmd)

    shell.expect(contains("total parse time (sec):"))
    shell.expect(contains("avg rate (files/sec):"))
    shell.expect(contains("\n"))

    var errorFound = false
    var error = ""
    try {
      error = shell.expectIn(1, (contains("\n"))).getBefore()
      errorFound = true
    } catch {
      case ae: AssertionError => //No error was found, which is correct
    }
    if (errorFound) fail(error)

    val exitCodeCmd = if (Util.isWindows) "echo %ERRORLEVEL%\n" else "echo $?\n"
    shell.send(exitCodeCmd)
    val exitCode = shell.expect(contains("\n")).getBefore()
    if (!"0".equals(exitCode))
      fail("Tests failed. Exit code: " + exitCode)

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3396_CLI_Performance_2_Threads_2_Times_Negative() {

    var cmd = Util.binPath + " performance -N 2 -t 2 -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input5.txt\n"

    val shell = Util.startIncludeErrors(cmd)

    shell.expectIn(1, (contains("error")))

    shell.expect(contains("total parse time (sec):"))
    shell.expect(contains("avg rate (files/sec):"))
    shell.expect(contains("\n"))

    val exitCodeCmd = if (Util.isWindows) "echo %ERRORLEVEL%\n" else "echo $?\n"
    shell.send(exitCodeCmd)
    val exitCode = shell.expect(contains("\n")).getBefore()
    if ("0".equals(exitCode))
      fail("Tests were successful when they were expected to fail. Exit code: " + exitCode)

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

}
