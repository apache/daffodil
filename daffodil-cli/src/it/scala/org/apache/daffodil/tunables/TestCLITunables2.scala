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

package org.apache.daffodil.tunables

import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof
import org.apache.daffodil.Main.ExitCode
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

class TestCLITunables2 {

  val stdout = 0
  val stderr = 1

  /**
   * Suppresses SDW messages.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings1(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      // note: 2>&1 is shell-speak for "connect stderr into stdout"
      val cmd = String.format("""echo a,b| %s parse -s %s -TsuppressSchemaDefinitionWarnings=all 2>&1""", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("""<ex:e1 xmlns:ex="http://example.com">
  <s1>a</s1>
  <s2>b
</s2>
</ex:e1>""".replace("\r\n", "\n")))

      Util.expectExitCode(ExitCode.Success, shell)
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  /**
   * Will display SDW warnings. Does not set the tunable that suppresses them.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings2(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      // note: 2>&1 is shell-speak for "connect stderr into stdout"
      val cmd = String.format("""echo a,b| %s parse -s %s 2>&1""", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("""Schema Definition Warning"""))
      shell.expect(contains("""dfdl:lengthKind"""))
      shell.expect(contains("""delimited"""))
      shell.expect(contains("""dfdl:length"""))
      shell.expect(contains("""<ex:e1 xmlns:ex="http://example.com">
  <s1>a</s1>
  <s2>b
</s2>
</ex:e1>""".replace("\r\n", "\n")))

      Util.expectExitCode(ExitCode.Success, shell)
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_ReloadingDoesNotRepeatWarnings(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val compiledProcFile = Util.newTempFile("savedProc", ".bin")
    compiledProcFile.deleteOnExit()
    val compiledProcFilePath = compiledProcFile.getAbsolutePath()
    //
    // Scala compiler was having trouble with local blocks to provide
    // separate scopes that have their own val shell = ....
    //
    // So I split this into two little local functions each of which knows
    // about the same compiledProcFilePath.
    //
    def saveIt(): Unit = {
      val shell = Util.start("")
      try {
        val cmd = String.format("""%s save-parser -s %s %s """, Util.binPath, testSchemaFile, compiledProcFilePath)
        shell.sendLine(cmd)
        //
        // Saving the processor should compile and issue SDWs which we should see
        // in the expected output
        //
        shell.expectIn(stderr, contains("""Schema Definition Warning"""))
        Util.expectExitCode(ExitCode.Success, shell)
        shell.sendLine("exit")
        shell.expectIn(stdout, eof)
      } finally {
        shell.close()
      }
    }
    def reloadIt(): Unit = {
        val shell = Util.start("")
        try {
          val cmd = String.format("""echo a,b| %s parse -P %s """, Util.binPath, compiledProcFilePath)
          shell.sendLine(cmd)
          shell.sendLine("exit")
          val output = shell.expectIn(stdout, eof).getBefore
          val errout = shell.expectIn(stderr, eof).getBefore
          //
          // Let's make sure we get a parse result
          //
          assertTrue(output.contains("""<ex:e1 xmlns:ex="http://example.com">"""))
          assertTrue(output.contains("""</ex:e1>"""))
          //
          // We should NOT see a SDW because that isn't displayed on a reload of a compiled processor
          //
          assertFalse(errout.contains("Warning"))
          assertFalse(output.contains("Warning")) // in case it is somehow routed to stdout instead.
        } finally {
          shell.close()
        }
      }

    saveIt()
    reloadIt()
  }
}
