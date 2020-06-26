/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.performance

import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLIPerformance {

  @Test def test_3393_CLI_Performance_2_Threads_2_Times(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance -N 2 -t 2 -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total parse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
      if (exitCode != 0)
        fail("Tests failed. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3394_CLI_Performance_3_Threads_20_Times(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance -N 20 -t 3 -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total parse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
      if (exitCode != 0)
        fail("Tests failed. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3395_CLI_Performance_5_Threads_50_Times(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input5.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance -N 50 -t 5 -s %s -r Item2 %s", Util.binPath, testSchemaFile, testInputFile)
      println(cmd)
      shell.sendLine(cmd)
      shell.expect(contains("total parse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
       if (exitCode != 0)
        fail("Tests failed. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3396_CLI_Performance_2_Threads_2_Times_Negative(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input5.txt")
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

  @Test def test_3641_CLI_Performance_Unparse_2_Threads_2_Times(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance --unparse -N 2 -t 2 -s %s -r e3 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total unparse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
      if (exitCode != 0)
        fail("Tests failed. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3643_CLI_Performance_Unparse_3_Threads_20_Times(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance --unparse -N 20 -t 3 -s %s -r e3 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total unparse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
      if (exitCode != 0)
        fail("Tests failed. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3644_CLI_Performance_Unparse_5_Threads_50_Times(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s performance --unparse -N 50 -t 5 -s %s -r e3 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("total unparse time (sec):"))
      shell.expect(contains("avg rate (files/sec):"))

      val exitCodeCmd = if (Util.isWindows) "echo %errorlevel%" else "echo $?"
      shell.sendLine(exitCodeCmd)

      if (Util.isWindows) shell.expect(contains(exitCodeCmd + "\n"))
      else shell.expect(contains("\n"))

      val sExitCode = shell.expect(contains("\n")).getBefore()
      val exitCode = Integer.parseInt(sExitCode.trim())
      if (exitCode != 0)
        fail("Tests failed. Exit code: " + exitCode)

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3642_CLI_Performance_Unparse_2_Threads_2_Times_Negative(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input16.txt")
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
