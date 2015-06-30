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
import scala.sys.process._
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
import net.sf.expectit.ExpectIOException
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.regexp
import net.sf.expectit.matcher.Matchers.eof
import net.sf.expectit.matcher.Matchers.anyString
import scala.language.postfixOps

class TestCLIPerformance {

  @Test def test_3393_CLI_Performance_2_Threads_2_Times() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance -N 2 -t 2 -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total parse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      try {
        var error = shell.expectIn(1, anyString).group()
        fail(error)
      } catch {
        case ae: ExpectIOException => {
          //No error was found, which is correct
          val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
          shell.sendLine(exitCodeCmd)

          if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
          else shell.expect(contains("\n"))

          val sExitCode = shell.expect(contains("\n")).getBefore()
          val exitCode = Integer.parseInt(sExitCode.trim())
          if (exitCode != 0)
            fail("Tests failed. Exit code: " + exitCode)
        }
      }

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3394_CLI_Performance_3_Threads_20_Times() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance -N 20 -t 3 -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total parse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      try {
        var error = shell.expectIn(1, anyString).group()
        fail(error)
      } catch {
        case ae: ExpectIOException => {
          //No error was found, which is correct
          val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
          shell.sendLine(exitCodeCmd)

          if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
          else shell.expect(contains("\n"))

          val sExitCode = shell.expect(contains("\n")).getBefore()
          val exitCode = Integer.parseInt(sExitCode.trim())
          if (exitCode != 0)
            fail("Tests failed. Exit code: " + exitCode)
        }
      }

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }
/*
  @Test def test_3395_CLI_Performance_5_Threads_50_Times() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input5.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance -N 50 -t 5 -s %s -r Item2 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total parse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      try {
        var error = shell.expectIn(1, anyString).group()
        fail(error)
      } catch {
        case ae: ExpectIOException => {
          //No error was found, which is correct
          val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
          shell.sendLine(exitCodeCmd)

          if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
          else shell.expect(contains("\n"))

          val sExitCode = shell.expect(contains("\n")).getBefore()
          val exitCode = Integer.parseInt(sExitCode.trim())
          if (exitCode != 0)
            fail("Tests failed. Exit code: " + exitCode)
        }
      }

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }
*/
  @Test def test_3396_CLI_Performance_2_Threads_2_Times_Negative() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input5.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance -N 2 -t 2 -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total parse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))
      shell.expectIn(1, (contains("error")))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
      if (exitCode == 0)
        fail("Tests were successful when they were expected to fail. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3641_CLI_Performance_Unparse_2_Threads_2_Times() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input14.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance --unparse -N 2 -t 2 -s %s -r e3 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total unparse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      try {
        var error = shell.expectIn(1, anyString).group()
        fail(error)
      } catch {
        case ae: ExpectIOException => {
          //No error was found, which is correct
          val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
          shell.sendLine(exitCodeCmd)

          if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
          else shell.expect(contains("\n"))

          val sExitCode = shell.expect(contains("\n")).getBefore()
          val exitCode = Integer.parseInt(sExitCode.trim())
          if (exitCode != 0)
            fail("Tests failed. Exit code: " + exitCode)
        }
      }

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3643_CLI_Performance_Unparse_3_Threads_20_Times() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input14.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance --unparse -N 20 -t 3 -s %s -r e3 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total unparse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      try {
        var error = shell.expectIn(1, anyString).group()
        fail(error)
      } catch {
        case ae: ExpectIOException => {
          //No error was found, which is correct
          val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
          shell.sendLine(exitCodeCmd)

          if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
          else shell.expect(contains("\n"))

          val sExitCode = shell.expect(contains("\n")).getBefore()
          val exitCode = Integer.parseInt(sExitCode.trim())
          if (exitCode != 0)
            fail("Tests failed. Exit code: " + exitCode)
        }
      }

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3644_CLI_Performance_Unparse_5_Threads_50_Times() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input14.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance --unparse -N 50 -t 5 -s %s -r e3 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total unparse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      try {
        var error = shell.expectIn(1, anyString).group()
        fail(error)
      } catch {
        case ae: ExpectIOException => {
          //No error was found, which is correct
          val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
          shell.sendLine(exitCodeCmd)

          if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
          else shell.expect(contains("\n"))

          val sExitCode = shell.expect(contains("\n")).getBefore()
          val exitCode = Integer.parseInt(sExitCode.trim())
          if (exitCode != 0)
            fail("Tests failed. Exit code: " + exitCode)
        }
      }

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3642_CLI_Performance_Unparse_2_Threads_2_Times_Negative() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input16.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance --unparse -N 2 -t 2 -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total unparse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))
      shell.expectIn(1, (contains("error")))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
      if (exitCode == 0)
        fail("Tests were successful when they were expected to fail. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }
}
