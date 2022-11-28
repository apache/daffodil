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

import java.nio.charset.StandardCharsets.UTF_8

import org.apache.commons.io.FileUtils

import org.junit.Assert._
import org.junit.Test

import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode

class TestCLIparsing {

  @Test def test_3677_CLI_Parsing_elementFormDefault_qualified(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/elementFormDefaultQualified.dfdl.xsd")

    runCLI(args"parse -s $schema -r s1") { cli =>
      cli.sendLine("strng", inputDone = true)
      cli.expect("<tns:e1>strng</tns:e1>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_3678_CLI_Parsing_elementFormDefault_unqualified(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/elementFormDefaultUnqualified.dfdl.xsd")

    runCLI(args"parse -s $schema -r s1") { cli =>
      cli.sendLine("strng", inputDone = true)
      cli.expect("<e1>strng</e1>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_2358_CLI_Parsing_SimpleParse_stdOut_extVars(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val config = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")

    runCLI(args"parse -s $schema -r row -D{http://example.com}var1=99 -c $config") { cli =>
      cli.sendLine("0", inputDone = true)
      cli.expect("<tns:row xmlns:tns=\"http://example.com\">")
      cli.expect("<cell>99</cell>")
    } (ExitCode.Success)
  }

  @Test def test_3507_CLI_Parsing_SimpleParse_SaveParser_extVars(): Unit = {
    withTempFile { parser =>
      val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
      val config = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")

      runCLI(args"-v save-parser -s $schema -r row -c $config $parser") { cli =>
        cli.expectErr("[info] Time (saving)")
      } (ExitCode.Success)

      runCLI(args"parse -P $parser -D{http://example.com}var1=99") { cli =>
        cli.sendLine("0", inputDone = true)
        cli.expect("<tns:row xmlns:tns=\"http://example.com\">")
        cli.expect("<cell>99</cell>")
      } (ExitCode.Success)

      runCLI(args"parse -P $parser -D{http://example.com}var1=55") { cli =>
        cli.sendLine("0", inputDone = true)
        cli.expect("<tns:row xmlns:tns=\"http://example.com\">")
        cli.expect("<cell>55</cell>")
      } (ExitCode.Success)
    }
  }

  @Test def test_2360_CLI_Parsing_SimpleParse_stdOut_extVars2(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val config = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")

    runCLI(args"parse -s $schema -r row2 -c $config") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expect("<cell>-9</cell>")
      cli.expect("<cell>-2</cell>")
      cli.expect("<cell>-8</cell>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_3506_CLI_Parsing_SimpleParse_extVars2(): Unit = {
    withTempFile { parser =>
      val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")

      runCLI(args"-v save-parser -s $schema -r row $parser") { cli =>
        cli.expectErr("[info] Time (saving)")
      } (ExitCode.Success)

      runCLI(args"parse --parser $parser") { cli =>
        cli.sendLine("0", inputDone = true)
        cli.expect("<tns:row xmlns:tns=\"http://example.com\">")
        cli.expect("<cell>-1</cell>")
      } (ExitCode.Success)

      runCLI(args"parse --parser $parser -D{http://example.com}var1=55") { cli =>
        cli.sendLine("0", inputDone = true)
        cli.expect("<tns:row xmlns:tns=\"http://example.com\">")
        cli.expect("<cell>55</cell>")
      } (ExitCode.Success)
    }
  }

  @Test def test_CLI_Parsing_SimpleParse_extVars_error(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")

    runCLI(args"parse -s $schema -r row2 -DdoesNotExist=1") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expectErr("definition not found")
      cli.expectErr("doesNotExist")
    } (ExitCode.BadExternalVariable)
  }

  @Test def test_3227_CLI_Parsing_SimpleParse_DFDL1197_fix(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section12/delimiter_properties/testOptionalInfix.dfdl.xsd")

    runCLI(args"-vv parse -s $schema") { cli =>
      cli.sendLine("1/3", inputDone = true)
      cli.expectErr("<Sequence><Separator/><RepMinMax name='s1'>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_1593_CLI_Parsing_MultifileSchema_noGlobalElem(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_21.dfdl.xsd")

    runCLI(args"parse -s $schema") { cli =>
      cli.send("does not matter", inputDone = true)
      cli.expectErr("No global elements")
      cli.expectErr("multi_base_21.dfdl.xsd")
    } (ExitCode.UnableToCreateProcessor)
  }

  //  See comment in DAFFODIL-952
  //
  //  Also note that this test is important in showing the expected existence
  //  and ordering of XML namespace prefix mappings. Daffodil ensures
  //  consistent and repeatable output of namespace prefix mappings, but normal
  //  TDML tests do not verify this part of expected infosets. This is one test
  //  verifies the expected output. If this test fails, it likely means we've
  //  broken our attempts to create consistent prefix mappings.
  @Test def test_1585_CLI_Parsing_MultifileSchema_methodImportSameDir(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_14.dfdl.xsd")

    runCLI(args"parse -s $schema") { cli =>
      cli.send("test", inputDone = true)
      cli.expect("""<base14:rabbitHole xmlns:a14="http://a14.com" xmlns:b14="http://b14.com" xmlns:base14="http://baseSchema.com">""")
      cli.expect("<a14:nestSequence>")
      cli.expect("<b14:nest>test</b14:nest>")
    } (ExitCode.Success)
  }

  @Test def test_1586_CLI_Parsing_MultifileSchema_methodIncludeSameDir(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_15.dfdl.xsd")

    runCLI(args"parse -s $schema") { cli =>
      cli.send("test", inputDone = true)
      cli.expect("<rabbitHole>")
      cli.expect("<nest>test</nest>")
    } (ExitCode.Success)
  }

  @Test def test_1587_CLI_Parsing_MultifileSchema_methodImportSameDir2(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_16.dfdl.xsd")

    runCLI(args"parse -s $schema") { cli =>
      cli.send("test", inputDone = true)
      cli.expect("<rabbitHole>")
      cli.expect("<nest>test</nest>")
    } (ExitCode.Success)
  }

  @Test def test_1317_IBMCompatibility_ABC_test_ibm_abc_cli(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/ABC_IBM.dfdl.xsd")

    runCLI(args"parse -s $schema -r ABC") { cli =>
      cli.sendLine("abcabcabc", inputDone = true)
      cli.expect("<Container>")
      cli.expect("<c>c</c>")
    } (ExitCode.Success)
  }

  @Test def test_977_CLI_Parsing_SimpleParse_stdOut(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expect("<tns:cell>2</tns:cell>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_978_CLI_Parsing_SimpleParse_outFile(): Unit = {
    withTempFile { output =>
      val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

      runCLI(args"parse -s $schema -r matrix -o $output") { cli =>
        cli.sendLine("0,1,2", inputDone = true)
      } (ExitCode.LeftOverData)

      val res = FileUtils.readFileToString(output.toFile, UTF_8)
      assertTrue(res.contains("<tns:cell>2</tns:cell>"))
    }
  }

  @Test def test_979_CLI_Parsing_SimpleParse_inFile(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")

    runCLI(args"parse -s $schema -r matrix $input") { cli =>
      cli.expect("<tns:cell>2</tns:cell>")
    } (ExitCode.Success)
  }

  @Test def test_980_CLI_Parsing_SimpleParse_stOutDash(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")

    runCLI(args"parse -s $schema -r matrix -o - $input") { cli =>
      cli.expect("<tns:cell>2</tns:cell>")
    } (ExitCode.Success)
  }

  @Test def test_981_CLI_Parsing_SimpleParse_stdInDash(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix -") { cli =>
      cli.sendLine("0,1,2,3", inputDone = true)
      cli.expect("<tns:cell>3</tns:cell>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_983_CLI_Parsing_SimpleParse_verboseMode(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"-v parse -s $schema -r matrix -") { cli =>
      cli.sendLine("0,1", inputDone = true)
      cli.expectErr("[info]")
    } (ExitCode.LeftOverData)

    runCLI(args"-vv parse -s $schema -r matrix -") { cli =>
      cli.sendLine("0,1", inputDone = true)
      cli.expectErr("[debug]")
    } (ExitCode.LeftOverData)
  }

  @Test def test_984_CLI_Parsing_negativeTest(): Unit = {
    runCLI(args"parse") { cli =>
      cli.sendLine("0,1,2,3", inputDone = true)
      cli.expectErr("There should be exactly one of the following options: schema, parser")
    } (ExitCode.Usage)
  }

  @Test def test_985_CLI_Parsing_SimpleParse_defaultRoot(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema") { cli =>
      cli.sendLine("0,1,2,3", inputDone = true)
      cli.expect("<tns:cell>3</tns:cell>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_988_CLI_Parsing_SimpleParse_specifiedRoot(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r hcp2") { cli =>
      cli.sendLine("12", inputDone = true)
      cli.expect("<tns:hcp2")
      cli.expect("12")
      cli.expect("</tns:hcp2>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_996_CLI_Parsing_negativeTest04(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r unknown") { cli =>
      cli.sendLine("12", inputDone = true)
      cli.expectErr("No root element found for unknown in any available namespace")
    } (ExitCode.UnableToCreateProcessor)
  }

  @Test def test_997_CLI_Parsing_multSchemas(): Unit = {
    val schema1 = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val schema2 = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")

    runCLI(args"parse -s $schema1 -s $schema2 -r hcp2") { cli =>
      cli.sendLine("12", inputDone = true)
      cli.expectErr("Bad arguments for option 'schema'")
    } (ExitCode.Usage)
  }

  @Test def test_3661_CLI_Parsing_badSchemaPath(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/doesnotexist.dfdl.xsd")

    runCLI(args"parse -s $schema -r root") { cli =>
      cli.sendLine("12", inputDone = true)
      cli.expectErr("Bad arguments for option 'schema'")
      cli.expectErr("Could not find file or resource")
    } (ExitCode.Usage)
  }

  @Test def test_1002_CLI_Parsing_negativeTest03(): Unit = {

    runCLI(args"parse -P parserThatDoesNotExist") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expectErr("parserThatDoesNotExist")
    } (ExitCode.FileNotFound)
  }

  @Test def test_1003_CLI_Parsing_SimpleParse_emptyNamespace(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt")

    runCLI(args"parse -s $schema -r {}address $input") { cli =>
      cli.expect("<address>")
    } (ExitCode.Success)
  }

  @Test def test_1004_CLI_Parsing_SimpleParse_namespaceUsed(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/charClassEntities.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt")

    runCLI(args"parse -s $schema -r {target}matrix $input") { cli =>
      cli.expect("""<tns:matrix xmlns:tns="target">""")
      cli.expect("<cell>14</cell>")
    } (ExitCode.Success)
  }

  @Test def test_2615_CLI_Parsing_SimpleParse_namespaceUsedLongOpt(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/charClassEntities.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt")

    runCLI(args"parse -s $schema --root {target}matrix $input") { cli =>
      cli.expect("""<tns:matrix xmlns:tns="target">""")
      cli.expect("<cell>14</cell>")
    } (ExitCode.Success)
  }

  @Test def test_1005_CLI_Parsing_SimpleParse_rootPath(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r hcp2 -p /") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("<tns:hcp2 xmlns:tns=\"http://www.example.org/example1/\">")
      cli.expect("12")
      cli.expect("</tns:hcp2>")
    } (ExitCode.Success)
  }

  @Test def test_1015_CLI_Parsing_SimpleParse_defaultRootMultSchema(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt")

    runCLI(args"parse -s $schema $input") { cli =>
      cli.expect("<address>")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationOn(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix --validate on") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expect("<tns:cell>2</tns:cell>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validation_missing_mode(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix --validate") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expectErr("Bad arguments")
      cli.expectErr("validate")
      cli.expectErr("exactly one argument")
    } (ExitCode.Usage)
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationLimited(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix --validate limited") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expect("<tns:cell>2</tns:cell>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationOff(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix --validate off") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expect("<tns:cell>2</tns:cell>")
    } (ExitCode.LeftOverData)
  }

  @Test def test_XXX_CLI_Parsing_SimpleSchema_basicTest_validationFooBar(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse --validate FooBar -s $schema -r matrix") { cli =>
      cli.sendLine("0,1,2", inputDone = true)
      cli.expectErr("FooBar")
    } (ExitCode.Usage)
  }

  @Test def test_1319_CLI_Parsing_invalidElementSDE(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/ABC_IBM_invalid.dfdl.xsd")

    runCLI(args"parse -s $schema -r ABC") { cli =>
      cli.sendLine("ababababbaacccccb", inputDone = true)
      cli.expectErr("'fixed' is not a valid")
    } (ExitCode.UnableToCreateProcessor)
  }

  @Test def test_1346_CLI_Parsing_SimpleParse_defaultRootMultSchemaMultiple(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt")
    for (x <- 1 to 10) {
      runCLI(args"parse -s $schema $input") { cli =>
        cli.expect("<address>")
      } (ExitCode.Success)
    }
  }

  @Test def test_1386_CLI_Parsing_negativeTest05(): Unit = {
    runCLI(args"") { cli =>
      cli.sendLine("12", inputDone = true)
      cli.expectErr("Subcommand required")
    } (ExitCode.Usage)
  }

  @Test def test_1971_CLI_Parsing_traceMode01(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_15.dfdl.xsd")

    runCLI(args"-t parse -s $schema") { cli =>
      cli.sendLine("test", inputDone = true)
      cli.expect("parser: <Element name='rabbitHole'>")
    } (ExitCode.Success)
  }

  @Test def test_1973_CLI_Parsing_traceMode03(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"-t parse -s $schema") { cli =>
      cli.sendLine("0,1,2,3,,,,", inputDone = true)
      cli.expectErr("Left over data. Consumed 56 bit(s) with at least")
      cli.expectErr("Left over data (Hex) starting at byte 8 is: (")
      cli.expectErr("Left over data (UTF-8) starting at byte 8 is: (")
    } (ExitCode.LeftOverData)
  }

  @Test def test_1941_CLI_Parsing_SimpleParse_leftOverData(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix") { cli =>
      cli.sendLine("1,2,3,4,,,", inputDone = true)
      cli.expectErr("Left over data. Consumed 56 bit(s) with at least")
      cli.expectErr("Left over data (Hex) starting at byte 8 is: (")
      cli.expectErr("Left over data (UTF-8) starting at byte 8 is: (")
    } (ExitCode.LeftOverData)
  }

  @Test def test_CLI_Parsing_BitParse_LSBPartialByte_leftOverData(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/bits_parsing.dfdl.xsd")

    runCLI(args"parse -s $schema -r lsbPartialByte") { cli =>
      cli.send("stri", inputDone = true)
      cli.expectErr("Left over data. Consumed 10 bit(s) with at least 16 bit(s) remaining."
        + "\nLeft over data starts with partial byte. Left over data (Binary) at byte 2 is: (0b011101xx)"
        + "\nLeft over data (Hex) starting at byte 3 is: (0x7269...)"
        + "\nLeft over data (UTF-8) starting at byte 3 is: (ri...)")
    } (ExitCode.LeftOverData)
  }

  @Test def test_CLI_Parsing_BitParse_MSBPartialByte_leftOverData(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/bits_parsing.dfdl.xsd")

    runCLI(args"parse -s $schema -r msbPartialByte") { cli =>
      cli.send("stri", inputDone = true)
      cli.expectErr("Left over data. Consumed 10 bit(s) with at least 16 bit(s) remaining."
        + "\nLeft over data starts with partial byte. Left over data (Binary) at byte 2 is: (0bxx110100)"
        + "\nLeft over data (Hex) starting at byte 3 is: (0x7269...)"
        + "\nLeft over data (UTF-8) starting at byte 3 is: (ri...)")
    } (ExitCode.LeftOverData)
  }

  @Test def test_CLI_Parsing_BitParse_MSBFullByte_leftOverData(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/bits_parsing.dfdl.xsd")

    runCLI(args"parse -s $schema -r msbFullByte") { cli =>
      cli.send("stri", inputDone = true)
      cli.expectErr("Left over data. Consumed 16 bit(s) with at least 16 bit(s) remaining."
        + "\nLeft over data (Hex) starting at byte 3 is: (0x7269...)"
        + "\nLeft over data (UTF-8) starting at byte 3 is: (ri...)")
    } (ExitCode.LeftOverData)
  }

  @Test def test_DFDL_714(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/global_element.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/test_DFDL-714.txt")

    runCLI(args"parse -s $schema $input") { cli =>
      cli.expect("<tns:elem xmlns:tns=\"http://baseSchema.com\">")
      cli.expect("<content")
      cli.expect("Hello World")
      cli.expect("</tns:elem>")
    } (ExitCode.Success)
  }

  @Test def test_DFDL_1203_schema_from_jar(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/global_element.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/test_DFDL-714.txt")

    runCLI(args"parse -s $schema $input") { cli =>
      cli.expect("<tns:elem xmlns:tns=\"http://baseSchema.com\">")
      cli.expect("<content")
      cli.expect("Hello World")
      cli.expect("</tns:elem>")
    } (ExitCode.Success)
  }

  @Test def test_3606_CLI_Parsing_SimpleParse_largeInfoset(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"parse -s $schema -r matrix") { cli =>
      val longInput = "0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64,0,1,24,5,64"
      cli.sendLine(longInput, inputDone = true)
      val result = cli.expect("<tns:row").getBefore()
      if (result.contains("""<tns:matrix xmlns:tns="http://www.example.org/example1/"><tns:matrix xmlns:tns="http://www.example.org/example1/">""")) {
        throw new Exception("Error - Root has been duplicated")
      }
    } (ExitCode.LeftOverData)
  }

  @Test def test_CLI_Parsing_built_in_formats(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_04.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input6.txt")

    runCLI(args"parse -s $schema -r e $input") { cli =>
      cli.expectErr("Schema Definition Warning")
      cli.expectErr("edu/illinois/ncsa/daffodil/xsd/built-in-formats.xsd")
      cli.expectErr("org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd")
    } (ExitCode.Success)
  }

  @Test def test_CLI_Parsing_JavaDefaults(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    withSysProp("javax.xml.parsers.SAXParserFactory" -> "com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl") {
      withSysProp("javax.xml.xml.validation.SchemaFactory" -> "com/sun/org/apache/xerces/internal/jaxp/validation/XMLSchemaFactory") {
        runCLI(args"parse -s $schema -r matrix") { cli =>
          cli.sendLine("0,1,2", inputDone = true)
          cli.expect("<tns:cell>2</tns:cell>")
        } (ExitCode.LeftOverData)
      }
    }
  }

  @Test def test_XXX_CLI_Parsing_Stream_01(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_02.dfdl.xsd")

    runCLI(args"parse --stream -s $schema") { cli =>
      cli.send("123", inputDone = true)
      cli.expect("<a>1</a>")
      cli.expect("<a>2</a>")
      cli.expect("<a>3</a>")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_Stream_02(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_02.dfdl.xsd")

    runCLI(args"parse --stream -s $schema") { cli =>
      cli.send("123ab", inputDone = true)
      cli.expect("<a>1</a>")
      cli.expect("<a>2</a>")
      cli.expect("<a>3</a>")
      cli.expectErr("Left over data after consuming 0 bits while streaming.")
      cli.expectErr("Stopped after consuming 24 bit(s) with at least 16 bit(s) remaining.")
    } (ExitCode.LeftOverData)
  }

  @Test def test_CLI_Parsing_XCatalog_Resolution_Failure(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/xcatalog_import_failure.dfdl.xsd")
    val xcatalog = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/xcatalog_invalid.xml")

    withSysProp("xml.catalog.files" -> xcatalog.toAbsolutePath.toString) {
      runCLI(args"parse -s $schema") { cli =>
        cli.send("X", inputDone = true)
        cli.expectErr("Schema Definition Error")
        cli.expectErr("non_existent_file.xml")
      } (ExitCode.UnableToCreateProcessor)
    }
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_w3cdom(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"parse -I w3cdom -s $schema -r e1") { cli =>
      cli.send("Hello", inputDone = true)
      cli.expect("""<tns:e1 xmlns:tns="http://example.com">Hello</tns:e1>""")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_jdom(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"parse -I jdom -s $schema -r e1") { cli =>
      cli.send("Hello", inputDone = true)
      cli.expect("""<tns:e1 xmlns:tns="http://example.com">Hello</tns:e1>""")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_scala_xml(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"parse -I scala-xml -s $schema -r e1") { cli =>
      cli.send("Hello", inputDone = true)
      cli.expect("""<tns:e1 xmlns:tns="http://example.com">Hello</tns:e1>""")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_json(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"parse -I json -s $schema -r e1") { cli =>
      cli.send("Hello", inputDone = true)
      cli.expect(""""e1": "Hello"""")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_null(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    // must be a forking test to match eof to make sure no data is output
    runCLI(args"parse -I null -s $schema -r e1", fork = true) { cli =>
      cli.send("Hello", inputDone = true)
      val res = cli.expect(net.sf.expectit.matcher.Matchers.eof)
      assertEquals("", res.getBefore())
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_sax(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"parse -I sax -s $schema -r e1") { cli =>
      cli.send("Hello", inputDone = true)
      cli.expect("""<tns:e1 xmlns:tns="http://example.com">Hello</tns:e1>""")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_exi(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"parse -I exi -s $schema -r e1") { cli =>
      cli.send("Hello", inputDone = true)
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Parsing_SimpleParse_exisa(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"parse -I exisa -s $schema -r e1") { cli =>
      cli.send("Hello", inputDone = true)
    } (ExitCode.Success)
  }

  @Test def test_CLI_Error_Return_Codes(): Unit = {
    val schema = path("this.does.not.exist")

    runCLI(args"parse -I scala-xml -s $schema -r e1") { cli =>
      cli.expectErr("this.does.not.exist")
    } (ExitCode.Usage)
  }

  @Test def test_2575_DFDLX_Trace_output(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/trace_input.dfdl.xsd")

    runCLI(args"-v parse -r output -s $schema") { cli =>
      cli.send("0", inputDone = true)
      cli.expectErr("dfdlx:trace")
    } (ExitCode.Success)
  }

}
