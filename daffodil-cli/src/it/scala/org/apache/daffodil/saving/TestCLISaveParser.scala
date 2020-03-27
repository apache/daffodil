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

package org.apache.daffodil.saving

import junit.framework.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.apache.daffodil.CLI.Util
import java.io.File
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLISaveParser {

  val output1 = Util.getExpectedString("output1.txt")
  val output4 = Util.getExpectedString("output4.txt")
  val output6 = Util.getExpectedString("output6.txt")
  val output12 = Util.getExpectedString("output12.txt")
  val savedParserFile = new File("savedParser.xsd.bin")

  @Before def before() {
    savedParserFile.delete
  }

  @After def after() {
    savedParserFile.delete
  }

  @Test def test_3017_CLI_Saving_SaveParser_simple() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val saveCmd = String.format("%s save-parser -s %s -r matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("echo 0,1,2| %s parse --parser %s", Util.binPath, savedParserFile.getName())
      shell.sendLine(cmd)
      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3018_CLI_Saving_SaveParser_stdout() {

    val shell = Util.startIncludeErrors("")
    val savedParser = "external_variables.dfdl.xsd.bin"
    val parserFile = new File(savedParser)
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val saveCmd = String.format("%s -v save-parser -s %s > %s\n",
      Util.binPath,
      (if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile),
      savedParser)

    try {
      shell.send(saveCmd)
      shell.expectIn(1, (contains("[info] Time")))
      assertTrue("save-parser failed", parserFile.exists())
      val parseCmd = String.format("echo 0,1,2| %s parse --parser %s\n", Util.binPath, savedParser)
      shell.send(parseCmd)
      shell.expect(contains("<tns:row xmlns:tns=\"http://example.com\">"))
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

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val configFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")
    val (testSchemaFile, testConfigFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(configFile)) else (schemaFile, configFile)

    val shell = Util.start("")

    try {
      val saveCmd = String.format("%s save-parser -s %s -r row2 -c %s %s", Util.binPath, testSchemaFile, testConfigFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("echo 0,1,2| %s parse --parser %s -c %s", Util.binPath, savedParserFile.getName(), testConfigFile)
      shell.sendLine(cmd)
      shell.expect(contains(output12))

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3020_CLI_Saving_SaveParser_namespaceUsed() {

    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val saveCmd = String.format("%s save-parser -s %s -r {target}matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("%s parse --parser %s %s", Util.binPath, savedParserFile.getName(), testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains(output6))

      shell.sendLine("exit\n")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3021_CLI_Saving_SaveParser_path() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val saveCmd = String.format("%s save-parser -s %s -r matrix -p / %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("echo 0,1,2| %s parse --parser %s", Util.binPath, savedParserFile.getName())
      shell.sendLine(cmd)
      shell.expect(contains(output1))
      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3022_CLI_Saving_SaveParser_MultSchema() {

    val schemaFile1 = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val schemaFile2 = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/charClassEntities.dfdl.xsd")
    val (testSchemaFile1, testSchemaFile2) = if (Util.isWindows) (Util.cmdConvert(schemaFile1), Util.cmdConvert(schemaFile2)) else (schemaFile1, schemaFile2)
    val shell = Util.start("", true)

    try {
      val cmd = String.format("%s save-parser -s %s -s %s %s", Util.binPath, testSchemaFile1, testSchemaFile2, savedParserFile.getName())
      shell.sendLine(cmd)

      shell.expect(contains("Bad arguments for option 'schema'"))

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_3023_CLI_Saving_SaveParser_verboseMode() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      shell.sendLine(String.format("%s -v save-parser -s %s -r matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName()))
      shell.expect(contains("[info]"))

      shell.sendLine(String.format("%s -vv save-parser -s %s -r matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName()))
      shell.expect(contains("[compile]"))

      shell.sendLine(String.format("%s -vvv save-parser -s %s -r matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName()))
      shell.expect(contains("[debug]"))

      shell.sendLine(String.format("%s -vvvv save-parser -s %s -r matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName()))
      shell.expect(contains("[oolagdebug]"))

      shell.send("exit\n")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  // See DFDL-1016
  /*@Test def test_3038_CLI_Saving_SaveParser_namespaceNoRoot() {

    val cmd = Util.binPath + " save-parser -s daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd -r {http://www.example.org/example1/} savedParser.xml\n"
    val shell = Util.start(cmd, true)

    // Error message needs to be updated with actual message
    shell.expect(contains("Error - Root is required if namespace is given"))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }*/

  @Test def test_3039_CLI_Saving_SaveParser_emptyNamespace() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.startNoConvert("")

    try {
      val saveCmd = String.format("%s save-parser -s %s -r {}matrix -p / %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("echo 0,1,2| %s parse --parser %s", Util.binPath, savedParserFile.getName())
      shell.sendLine(cmd)

      shell.expect(contains(output1))

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_DFDL_1205_CLI_FullValidation_SavedParser_Incompatible() {

    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)
    val shell = Util.start("", true)

    try {
      val saveCmd = String.format("%s save-parser -s %s -r {target}matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("%s parse --parser %s --validate on %s", Util.binPath, savedParserFile.getName(), testInputFile)
      shell.sendLine(cmd)

      shell.expect(contains("[error]"))
      shell.expect(contains("The validation mode must be 'limited' or 'off' when using a saved parser."))

      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  /**
   * Note that in Daffodil 2.6.0 behavior of external variables changed. They are not saved as part of binary
   * compiling. They are a runtime-thing only.
   */
  @Test def test_3508_CLI_Saving_SaveParser_extVars() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val savedParser = "test_3508.xsd.bin"
    val parserFile = new File(savedParser)

    val shell = Util.startIncludeErrors("")

    try {
      var cmd = String.format("%s -v save-parser -s %s -r row2  %s", Util.binPath, testSchemaFile, savedParser)
      shell.sendLine(cmd)
      shell.expectIn(1, (contains("[info] Time (saving)")))
      assertTrue("save-parser failed", parserFile.exists())

      shell.sendLine();
      cmd = String.format("""echo 0| %s parse --parser %s -D"{http://example.com}var1=25" "{http://example.com}var3=7" """, Util.binPath, savedParser)
      shell.sendLine(cmd)
      shell.expect(contains("<tns:row2 xmlns:tns=\"http://example.com\">"))
      shell.expect(contains("<cell>25</cell>"))
      shell.expect(contains("<cell>7</cell>"))

      shell.send("exit\n")
      shell.expect(eof)
    } finally {
      shell.close()
      if (parserFile.exists()) parserFile.delete()
    }
  }

  // See DFDL-1147
  /*@Test def test_3063_CLI_Saving_SaveParser_validate() {

    val cmd = Util.binPath + " save-parser --validate on -s daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)

    var cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)
    shell.expect(contains("[warning] Validation Error: validation_check: cvc-pattern-valid"))
    shell.expect(contains("[warning] Validation Error: validation_check failed"))

    cmd = Util.binPath + " save-parser --validate -s daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)

    cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)
    shell.expect(contains("[warning] Validation Error: validation_check: cvc-pattern-valid"))
    shell.expect(contains("[warning] Validation Error: validation_check failed"))

    cmd = Util.binPath + " save-parser --validate limited -s daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)

    cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)
    shell.expect(contains("[warning] Validation Error: validation_check failed"))

    cmd = Util.binPath + " save-parser --validate off -s daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd -r validation_check savedParser.xml\n"
    shell.send(cmd)

    cmd2 = "echo -ne 'test'| " + Util.binPath + " parse --parser savedParser.xml \n"
    shell.send(cmd2)

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }*/

  // See DFDL-1141
  /*@Test def test_3036_CLI_Saving_SaveParser_debug() {

    val cmd = Util.binPath + " -d save-parser -s daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix savedParser.xml\n"
    val shell = Util.start(cmd)

    val cmd2 = Util.binPath + " parse --parser savedParser.xml daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt\n"
    shell.send(cmd2)
    shell.expect(contains("(debug)"))
    shell.send("continue\n")
    shell.send("quit\n")

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }

  @Test def test_3037_CLI_Saving_SaveParser_trace() {

    val cmd = Util.binPath + " -t save-parser -s daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_15.dfdl.xsd savedParser.xml\n"
    val shell = Util.start(cmd)

    val cmd2 = "echo test| " + Util.binPath + " parse --parser savedParser.xml\n"
    shell.send(cmd2)
    shell.expect(contains("parser: <Element name='rabbitHole'><ComplexType>...</ComplexType></Element name='rabbitHole'>"))

    shell.send("exit\n")
    shell.expect(eof())
    shell.close()
  }*/

  /* // See DFDL-1342
  @Test def test_3572_CLI_Saving_SaveParser_unparse() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)
    val shell = Util.start("")

    try {
      val saveCmd = String.format("%s save-parser -s %s -r matrix %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("%s unparse --parser %s %s", Util.binPath, savedParserFile.getName(), testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("0,1,2"))
      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }
*/

  @Test def test_3573_CLI_Saving_SaveParser_unparse2() {

    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input12.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val saveCmd = String.format("%s save-parser -s %s -r e1 %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("%s unparse --parser %s %s", Util.binPath, savedParserFile.getName(), testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("Hello"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  @Test def test_3941_CLI_Saving_SaveParser_tunables() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input12.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("", true)

    try {
      val saveCmd = String.format("%s save-parser -s %s -r e1 -T parseUnparsePolicy=parseOnly %s", Util.binPath, testSchemaFile, savedParserFile.getName())
      shell.sendLine(saveCmd)

      val cmd = String.format("%s unparse --parser %s %s", Util.binPath, savedParserFile.getName(), testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("[error]"))
      shell.expect(contains("Runtime Schema Definition Error: This schema was compiled without unparse support."))
      shell.sendLine("exit")
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

}
