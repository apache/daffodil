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

package org.apache.daffodil.parsing

import junit.framework.Assert._
import org.junit.Test
import java.io.File
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLIparsing {

  val output1 = Util.getExpectedString("output1.txt")
  val output1_nopretty = Util.getExpectedString("output1_nopretty.txt")
  val output2 = Util.getExpectedString("output2.txt")
  val output4 = Util.getExpectedString("output4.txt")
  val output6 = Util.getExpectedString("output6.txt")
  val output8 = Util.getExpectedString("output8.txt")
  val output9 = Util.getExpectedString("output9.txt")
  val output10 = Util.getExpectedString("output10.txt")
  val output12 = Util.getExpectedString("output12.txt")

  @Test def test_3677_CLI_Parsing_elementFormDefault_qualified() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/elementFormDefaultQualified.dfdl.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("echo strng| %s parse -s %s -r s1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:e1>strng</tns:e1>"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  @Test def test_3678_CLI_Parsing_elementFormDefault_unqualified() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/elementFormDefaultUnqualified.dfdl.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("echo strng| %s parse -s %s -r s1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("<e1>strng</e1>"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  @Test def test_2358_CLI_Parsing_SimpleParse_stdOut_extVars() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val configFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")
    val (testSchemaFile, testConfigFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(configFile)) else (schemaFile, configFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0| %s parse -s %s -r row -D\"{http://example.com}var1=99\" -c %s", Util.binPath, testSchemaFile, testConfigFile)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:row xmlns:tns=\"http://example.com\">"))
      shell.expect(contains("<cell>99</cell>"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  @Test def test_3507_CLI_Parsing_SimpleParse_SaveParser_extVars() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val configFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")
    val (testSchemaFile, testConfigFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(configFile)) else (schemaFile, configFile)

    val savedParser = "test_3507.xsd.bin"
    val parserFile = new File(savedParser)

    val shell = Util.startIncludeErrors("")

    try {
      var cmd = String.format("%s -v save-parser -s %s -r row -D\"{http://example.com}var1=99\" -c %s %s", Util.binPath, testSchemaFile, testConfigFile, savedParser)
      shell.sendLine(cmd)
      shell.expectIn(1, (contains("[info] Time (saving)")))
      assertTrue("save-parser failed", parserFile.exists())

      shell.sendLine();
      cmd = String.format("echo 0| %s parse --parser %s\n", Util.binPath, savedParser)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:row xmlns:tns=\"http://example.com\">"))
      shell.expect(contains("<cell>99</cell>"))

      cmd = String.format("echo 0| %s parse --parser %s -D\"{http://example.com}var1=55\"", Util.binPath, savedParser)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:row xmlns:tns=\"http://example.com\">"))
      shell.expect(contains("<cell>55</cell>"))

      shell.send("exit\n")
      shell.expect(eof)
    } finally {
      shell.close()
      if (parserFile.exists()) parserFile.delete()
    }
  }

  @Test def test_2360_CLI_Parsing_SimpleParse_stdOut_extVars2() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val configFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")
    val (testSchemaFile, testConfigFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(configFile)) else (schemaFile, configFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r row2 -c %s", Util.binPath, testSchemaFile, testConfigFile)
      shell.sendLine(cmd)
      shell.expect(contains(output12))

      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_3506_CLI_Parsing_SimpleParse_extVars2() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val savedParser = "test_3506.xsd.bin"
    val parserFile = new File(savedParser)

    val shell = Util.startIncludeErrors("")

    try {
      var cmd = String.format("%s -v save-parser -s %s -r row %s", Util.binPath, testSchemaFile, savedParser)
      shell.sendLine(cmd)
      shell.expectIn(1, (contains("[info] Time (saving)")))
      assertTrue("save-parser failed", parserFile.exists())

      cmd = String.format("echo 0| %s parse --parser %s", Util.binPath, savedParser)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:row xmlns:tns=\"http://example.com\">"))
      shell.expect(contains("<cell>-1</cell>"))

      cmd = String.format("echo 0| %s parse --parser %s -D\"{http://example.com}var1=55\"", Util.binPath, savedParser)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:row xmlns:tns=\"http://example.com\">"))
      shell.expect(contains("<cell>55</cell>"))

      shell.send("exit\n")
      shell.expect(eof)
    } finally {
      shell.close()
      if (parserFile.exists()) parserFile.delete()
    }
  }

  @Test def test_3227_CLI_Parsing_SimpleParse_DFDL1197_fix() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section12/delimiter_properties/testOptionalInfix.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 1/3| %s -vv parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      shell.expect(contains("<Sequence><Separator/><RepMinMax name='s1'>"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1593_CLI_Parsing_MultifileSchema_noGlobalElem() {
    val tmp_filename: String = (System.currentTimeMillis / 1000).toString()
    val file = new File(tmp_filename)
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/TopLevel.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/02nine_headers.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s -o %s %s", Util.binPath, testSchemaFile, tmp_filename, testInputFile)
      shell.sendLine(cmd)

      //TODO:update
      //val err = shell.getCurrentStandardErrContents()
      //assertEquals(err, "")
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
      //Remove the temporary output file
      assertTrue("Failed to remove temporary file: %s".format(file), file.delete)
    }
  }

  //  See comment in DFDL-952
  @Test def test_1585_CLI_Parsing_MultifileSchema_methodImportSameDir() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_14.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("test") + "| %s parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(output9))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1586_CLI_Parsing_MultifileSchema_methodIncludeSameDir() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_15.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("test") + "| %s parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      shell.expect(contains(output10))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1587_CLI_Parsing_MultifileSchema_methodImportSameDir2() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_16.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("test") + "| %s parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      shell.expect(contains(output10))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1317_IBMCompatibility_ABC_test_ibm_abc_cli() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/ABC_IBM.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo abcabcabc| %s parse -s %s -r ABC", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      shell.expect(contains(output8))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_977_CLI_Parsing_SimpleParse_stdOut() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r matrix", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_978_CLI_Parsing_SimpleParse_outFile() {
    val tmp_filename: String = (System.currentTimeMillis / 1000).toString()
    val file = new File(tmp_filename)
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r matrix -o %s", Util.binPath, testSchemaFile, tmp_filename)
      shell.sendLine(cmd)

      val catCmd = if (Util.isWindows) "type" else "cat"
      val openCmd = String.format("%s %s", catCmd, tmp_filename)

      shell.sendLine(openCmd)
      shell.expect(contains("<tns:cell>2</tns:cell>"))

      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
      assertTrue("Failed to remove temporary file: %s".format(file), file.delete)
    }
  }

  @Test def test_979_CLI_Parsing_SimpleParse_inFile() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_980_CLI_Parsing_SimpleParse_stOutDash() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s -r matrix -o - %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_981_CLI_Parsing_SimpleParse_stdInDash() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2,3| %s parse -s %s -r matrix -", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(output2))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_983_CLI_Parsing_SimpleParse_verboseMode() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("", true)

    try {

      shell.sendLine(String.format("echo 0,1| %s -v parse -s %s -r matrix -", Util.binPath, testSchemaFile))
      shell.expect(contains("[info]"))

      shell.sendLine(String.format("echo 0,1| %s -vv parse -s %s -r matrix -", Util.binPath, testSchemaFile))
      shell.expect(contains("[compile]"))

      shell.sendLine(String.format("echo 0,1| %s -vvv parse -s %s -r matrix -", Util.binPath, testSchemaFile))
      shell.expect(contains("[debug]"))

      shell.sendLine(String.format("echo 0,1| %s -vvvv parse -s %s -r matrix -", Util.binPath, testSchemaFile))
      shell.expect(contains("[oolagdebug]"))
      shell.send("exit\n")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_984_CLI_Parsing_negativeTest() {
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 0,1,2,3| %s parse", Util.binPath)
      shell.sendLine(cmd)
      shell.expect(contains("One of --schema or --parser must be defined"))
      shell.send("exit\n")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_985_CLI_Parsing_SimpleParse_defaultRoot() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2,3| %s parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(output2))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_988_CLI_Parsing_SimpleParse_specifiedRoot() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("")

    try {
      //val expected = """<tns:hcp2 xmlns:tns="http://www.example.org/example1/">12</tns:hcp2>"""
      val cmd = String.format("echo 12| %s parse -s %s -r hcp2", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:hcp2"))
      shell.expect(contains("12"))
      shell.expect(contains("</tns:hcp2>"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_996_CLI_Parsing_negativeTest04() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 12| %s parse -s %s -r unknown", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("No root element found for unknown in any available namespace"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_997_CLI_Parsing_multSchemas() {
    val schemaFile1 = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val schemaFile2 = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val (testSchemaFile1, testSchemaFile2) = if (Util.isWindows) (Util.cmdConvert(schemaFile1), Util.cmdConvert(schemaFile2)) else (schemaFile1, schemaFile2)

    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 12| %s parse -s %s -s %s  -r hcp2", Util.binPath, testSchemaFile1, testSchemaFile2)
      shell.sendLine(cmd)
      shell.expect(contains("Bad arguments for option 'schema'"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_3661_CLI_Parsing_badSchemaPath() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/doesnotexist.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 12| %s parse -s %s -r root", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Bad arguments for option 'schema'"))
      shell.expect(contains("Could not find file or resource"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1002_CLI_Parsing_negativeTest03() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 0,1,2| %s parse -P parserThatDoesNotExist", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      if (Util.isWindows) {
        shell.expect(contains("parserThatDoesNotExist (The system cannot find the file specified)"))
      } else {
        shell.expect(contains("parserThatDoesNotExist (No such file or directory)"))
      }
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1003_CLI_Parsing_SimpleParse_emptyNamespace() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s -r {}address %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains(output4))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1004_CLI_Parsing_SimpleParse_namespaceUsed() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s -r {target}matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains(output6))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_2615_CLI_Parsing_SimpleParse_namespaceUsedLongOpt() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s --root {target}matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains(output6))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1005_CLI_Parsing_SimpleParse_rootPath() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.startNoConvert("")

    try {
      //val expected = """<tns:hcp2 xmlns:tns="http://www.example.org/example1/">12</tns:hcp2>"""
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r hcp2 -p /", Util.binPath, testSchemaFile)

      shell.sendLine(cmd)
      shell.expect(contains("<tns:hcp2 xmlns:tns=\"http://www.example.org/example1/\">"))
      shell.expect(contains("12"))
      shell.expect(contains("</tns:hcp2>"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1015_CLI_Parsing_SimpleParse_defaultRootMultSchema() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains(output4))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationOn() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r matrix --validate on", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validation_missing_mode() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r matrix --validate", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Bad arguments"))
      shell.expect(contains("validate"))
      shell.expect(contains("exactly one argument"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationLimited() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r matrix --validate limited", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationOff() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("")

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r matrix --validate off", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationFooBar() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 0,1,2| %s parse --validate FooBar -s %s -r matrix", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("FooBar"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  /*
  //On hold until I implement a way to set the classpath before executing
  @Test def test_1313_CLI_Parsing_assertionFailure() {
    val cmd = "echo unacceptable| " + Util.binPath + " parse -s daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_B_08.dfdl.xsd -s daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_C_08.dfdl.xsd --root bElem2\n"
    val shell = Util.start(cmd)
    shell.expect(contains("Parse Error: Assertion failed. Assertion failed for dfdl:checkConstraints(.)"))

    shell.send("exit\n")
    shell.expect(eof)
    shell.close()
  }
*/

  @Test def test_1319_CLI_Parsing_invalidElementSDE() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/ABC_IBM_invalid.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo ababababbaacccccb| %s parse -s %s -r ABC", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("'fixed' is not a valid"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1346_CLI_Parsing_SimpleParse_defaultRootMultSchemaMultiple() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val cmd = String.format("%s parse -s %s %s", Util.binPath, testSchemaFile, testInputFile)

    for (x <- 1 to 10) {
      val shell = Util.start("")

      try {
        println("Run " + x + " of 10")
        shell.sendLine(cmd)
        shell.expect(contains(output4))
        shell.sendLine("exit")
        shell.expect(eof)
      } finally {
        shell.close()
      }
    }
  }

  @Test def test_1386_CLI_Parsing_negativeTest05() {
    val cmd = String.format("echo 12| %s", Util.binPath)
    val shell = Util.start("", true)

    try {
      shell.sendLine(cmd)
      shell.expect(contains("subcommand"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1971_CLI_Parsing_traceMode01() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_15.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo test| %s -t parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("parser: <Element name='rabbitHole'>"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_1973_CLI_Parsing_traceMode03() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("echo 0,1,2,3,,,,| %s -t parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(1, contains("Left over data. Consumed 56 bit(s) with at least"))
      shell.expectIn(1, contains("Left over data (Hex) starting at byte 8 is: ("))
      shell.expectIn(1, contains("Left over data (UTF-8) starting at byte 8 is: ("))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
      //assert(shell.getExitValue() == 1)
    }
  }

  @Test def test_1941_CLI_Parsing_SimpleParse_leftOverData() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo 1,2,3,4,,,| %s parse -s %s -r matrix", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Left over data. Consumed 56 bit(s) with at least"))
      shell.expect(contains("Left over data (Hex) starting at byte 8 is: ("))
      shell.expect(contains("Left over data (UTF-8) starting at byte 8 is: ("))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_BitParse_LSBPartialByte_leftOverData() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/bits_parsing.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("stri") + "| %s parse -s %s -r lsbPartialByte", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Left over data. Consumed 10 bit(s) with at least 16 bit(s) remaining."
        + "\nLeft over data starts with partial byte. Left over data (Binary) at byte 2 is: (0b011101xx)"
        + "\nLeft over data (Hex) starting at byte 3 is: (0x7269...)"
        + "\nLeft over data (UTF-8) starting at byte 3 is: (ri...)"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_BitParse_MSBPartialByte_leftOverData() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/bits_parsing.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("stri") + "| %s parse -s %s -r msbPartialByte", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Left over data. Consumed 10 bit(s) with at least 16 bit(s) remaining."
        + "\nLeft over data starts with partial byte. Left over data (Binary) at byte 2 is: (0bxx110100)"
        + "\nLeft over data (Hex) starting at byte 3 is: (0x7269...)"
        + "\nLeft over data (UTF-8) starting at byte 3 is: (ri...)"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_BitParse_MSBFullByte_leftOverData() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/bits_parsing.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("stri") + "| %s parse -s %s -r msbFullByte", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Left over data. Consumed 16 bit(s) with at least 16 bit(s) remaining."
        + "\nLeft over data (Hex) starting at byte 3 is: (0x7269...)"
        + "\nLeft over data (UTF-8) starting at byte 3 is: (ri...)"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_DFDL_714() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/global_element.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/test_DFDL-714.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:elem xmlns:tns=\"http://baseSchema.com\">"))
      shell.expect(contains("<content"))
      shell.expect(contains("Hello World"))
      shell.expect(contains("</tns:elem>"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_DFDL_1203_schema_from_jar() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/global_element.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/test_DFDL-714.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("", envp = Map("DAFFODIL_CLASSPATH" -> Util.daffodilPath("daffodil-cli/target/scala-2.10/*")))

    try {
      val cmd = String.format("%s parse -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:elem xmlns:tns=\"http://baseSchema.com\">"))
      shell.expect(contains("<content"))
      shell.expect(contains("Hello World"))
      shell.expect(contains("</tns:elem>"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_3606_CLI_Parsing_SimpleParse_largeInfoset() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val longInput = "0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64"
      val cmd = String.format("echo %s| %s parse -s %s -r matrix", longInput, Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      val result = shell.expect(contains("<tns:row")).getBefore()
      println(result)
      if (result.contains("""<tns:matrix xmlns:tns="http://www.example.org/example1/"><tns:matrix xmlns:tns="http://www.example.org/example1/">""")) {
        throw new Exception("Error - Root has been duplicated")
      }
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_built_in_formats() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_04.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input6.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format("%s parse -s %s -r e %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)

      shell.expectIn(1, contains("Schema Definition Warning"))
      shell.expectIn(1, contains("edu/illinois/ncsa/daffodil/xsd/built-in-formats.xsd"))
      shell.expectIn(1, contains("org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  // These DAFFODIL_JAVA_OPTS values change the Java defaults classes like
  // SAXParserFactory and SchemaFactory to be Java's internal classes instead
  // of those provided by dependencies (e.g. Xerces) included with Daffodil.
  // Some places require dependency version of these classes. This test ensures
  // that we override defaults when necesssary
  @Test def test_CLI_Parsing_JavaDefaults() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val java_opts = Map("DAFFODIL_JAVA_OPTS" ->
      ("-Djavax.xml.parsers.SAXParserFactory=com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl " +
        "-Djavax.xml.xml.validation.SchemaFactory=com/sun/org/apache/xerces/internal/jaxp/validation/XMLSchemaFactory"))

    val shell = Util.start("", envp = java_opts)

    try {
      val cmd = String.format("echo 0,1,2| %s parse -s %s -r matrix", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_XXX_CLI_Parsing_Stream_01() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_02.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("123") + "| %s parse --stream -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("<a>1</a>"))
      shell.expect(contains("<a>2</a>"))
      shell.expect(contains("<a>3</a>"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_XXX_CLI_Parsing_Stream_02() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_02.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format(Util.echoN("123ab") + "| %s parse --stream -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("<a>1</a>"))
      shell.expect(contains("<a>2</a>"))
      shell.expect(contains("<a>3</a>"))
      shell.expectIn(1, contains("Left over data after consuming 0 bits while streaming."))
      shell.expectIn(1, contains("Stopped after consuming 24 bit(s) with at least 16 bit(s) remaining."))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_XCatalog_Resolution_Failure() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/xcatalog_import_failure.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile

    val xcatalogFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/xcatalog_invalid.xml")
    val testXcatalogFile = if (Util.isWindows) Util.cmdConvert(xcatalogFile) else xcatalogFile

    val DAFFODIL_JAVA_OPTS = Map("DAFFODIL_JAVA_OPTS" -> ("-Dxml.catalog.files=" + testXcatalogFile + " -Xms256m -Xmx2048m -Djline.terminal=jline.UnsupportedTerminal -Dfile.encoding=UTF-8"))

    val shell = Util.startIncludeErrors("", envp = DAFFODIL_JAVA_OPTS)

    try {
      val cmd = String.format(Util.echoN("X") + "| %s parse -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)

      shell.expectIn(1, contains("Schema Definition Error"))
      if (Util.isWindows) {
        shell.expectIn(1, contains("\\this\\path\\does\\not\\exist"))
        shell.expectIn(1, contains("The system cannot find the path specified"))
      } else {
        shell.expectIn(1, contains("/this/path/does/not/exist"))
        shell.expectIn(1, contains("No such file or directory"))
      }

      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

}
