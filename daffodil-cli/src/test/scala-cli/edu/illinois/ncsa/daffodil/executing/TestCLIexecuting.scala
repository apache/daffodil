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

package edu.illinois.ncsa.daffodil.executing

import junit.framework.Assert._
import org.junit.Test
import scala.language.postfixOps
import scala.xml._
import scala.sys.process._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.CLI.Util
import edu.illinois.ncsa.daffodil.CLI.Util._
import java.io.File
import java.io.IOException
import java.util.regex.Pattern
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.anyString
import net.sf.expectit.matcher.Matchers.regexp
import net.sf.expectit.matcher.Matchers.matches
import net.sf.expectit.ExpectIOException

class TestCLIexecuting {

  val output3 = Util.getExpectedString("output3.txt")
  val output13 = Util.getExpectedString("output13.txt", true)
  val output14 = Util.getExpectedString("output14.txt", true)
  val output15 = Util.getExpectedString("output15.txt", true)
  val output16 = Util.getExpectedString("output16.txt", true)

  @Test def test_995_CLI_Executing_Listing_negativeTest01() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test %s escape_entry1 escape_entry2-11 escape_entry1-5 escape_entry4_3", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains(output3))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1001_CLI_Executing_Listing_execRegex01() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test --regex %s \"escape_entry4_\\d\"", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains(output13))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1000_CLI_Executing_Listing_listRegex02() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("", timeout = 5)
    try {
      val cmd = String.format("%s test -l --regex %s \"escape_entryb-\\d+\"", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(matches(""))
    } catch {
      case ex: ExpectIOException => {
        fail("Output was found when none was expected.")
      }
    } finally {
      shell.sendLine("exit")
      shell.close()
    }
  }

  @Test def test_999_CLI_Executing_Listing_listRegex01() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test -l --regex %s \"escape_entry4_\\d+\"", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains(output14))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  //
  // Test removed to scala-debug as order of execution of the individual tests is not deterministic. 
  // You'd like them to go in the order they appear in the file, but that's not the case
  // necessarily: http://stackoverflow.com/questions/3693626/how-to-run-test-methods-in-specific-order-in-junit4
  // JIRA DFDL-1240
  //
  //  @Test def test_994_CLI_Executing_Listing_execAll() {
  //    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section31/escape_characters/Escapes.tdml")
  //    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile
  //
  //    val shell = Util.start("")
  //
  //    try {
  //      val cmd = String.format("%s test %s", Util.binPath, testTdmlFile)
  //      shell.sendLine(cmd)
  //      shell.expect(contains(output15))
  //      shell.sendLine("exit")
  //    } finally {
  //      shell.close()
  //    }
  //  }

  @Test def test_993_CLI_Executing_Listing_listAll() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      shell.sendLine(String.format("%s test -l %s", Util.binPath, testTdmlFile))
      shell.expect(contains(output16))
      shell.sendLine()

      val countTests = if (Util.isWindows) "find /v \"\" /c" else "wc -l"
      val numTests = Integer.parseInt((String.format("%s test -l %s", Util.binPath, testTdmlFile) #| countTests !!).trim())
      val numFile = Integer.parseInt((String.format(if (Util.isWindows) "cmd.exe /c more %s | find /c \"parserTestCase>\"" else "grep -c parserTestCase> %s", testTdmlFile)!!).trim())

      assertTrue("Number of tests run should match the number of tests in the file.", numTests == numFile)
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_992_CLI_Executing_Listing_singleTestList() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test -l %s byte_entities_6_08", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains("byte_entities_6_08"))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_990_CLI_Executing_Listing_singleTest() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test %s byte_entities_6_08", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains("[Pass] byte_entities_6_08"))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }
}
