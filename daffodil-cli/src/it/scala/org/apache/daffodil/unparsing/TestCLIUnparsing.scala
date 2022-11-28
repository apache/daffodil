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

package org.apache.daffodil.unparsing

import java.nio.charset.StandardCharsets.UTF_8

import org.apache.commons.io.FileUtils

import org.junit.Assert._
import org.junit.Test

import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode

class TestCLIunparsing {

  @Test def test_3525_CLI_Unparsing_SimpleUnparse_inFile(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input12.txt")

    runCLI(args"unparse -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_3526_CLI_Unparsing_SimpleUnparse_inFile2(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input13.txt")

    runCLI(args"unparse -s $schema --root e3 $input") { cli =>
      cli.expect("[1,2]")
    } (ExitCode.Success)
  }

  @Test def test_3527_CLI_Unparsing_SimpleUnparse_stdin(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.txt")

    runCLI(args"unparse -s $schema --root e3") { cli =>
      cli.sendFile(input, inputDone = true)
      cli.expect("[1,2]")
    } (ExitCode.Success)
  }

  @Test def test_3528_CLI_Unparsing_SimpleUnparse_stdin2(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"unparse -s $schema --root e1") { cli =>
      cli.send("<tns:e1 xmlns:tns='http://example.com'>Hello</tns:e1>", inputDone = true)
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_3529_CLI_Unparsing_SimpleUnparse_stdin3(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"unparse -s $schema --root e1 -") { cli =>
      cli.send("<tns:e1 xmlns:tns='http://example.com'>Hello</tns:e1>", inputDone = true)
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_3584_CLI_Unparsing_SimpleUnparse_stdin4(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"unparse -s $schema --root file") { cli =>
      val input = "<tns:file xmlns:tns='http://www.example.org/example1/'><tns:header><tns:title>1</tns:title><tns:title>2</tns:title><tns:title>3</tns:title></tns:header><tns:record><tns:item>4</tns:item><tns:item>5</tns:item><tns:item>6</tns:item></tns:record></tns:file>"
      cli.send(input, inputDone = true)
      cli.expect("1,2,3")
      cli.expect("4,5,6")
    } (ExitCode.Success)
  }

  @Test def test_3574_CLI_Unparsing_SimpleUnparse_extVars(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val config = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input15.txt")

    runCLI(args"unparse -s $schema -r row -D{http://example.com}var1=99 -c $config $input") { cli =>
      cli.expect("0")
    } (ExitCode.Success)
  }

  @Test def test_3575_CLI_Unparsing_SimpleUnparse_extVars2(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd")
    val config = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input16.txt")

    runCLI(args"unparse -s $schema -r row -c $config $input") { cli =>
      cli.expect("0")
    } (ExitCode.Success)
  }

  @Test def test_3582_CLI_Unparsing_SimpleUnparse_outFile(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input13.txt")

    withTempFile { output =>
      runCLI(args"unparse -s $schema -r e3 -o $output $input") { cli =>
      } (ExitCode.Success)

      val res = FileUtils.readFileToString(output.toFile, UTF_8)
      assertTrue(res.contains("[1,2]"))
    }
  }

  @Test def test_3581_CLI_Unparsing_SimpleUnparse_stOutDash(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input13.txt")

    runCLI(args"unparse -s $schema -r e3 -o - $input") { cli =>
      cli.expect("[1,2]")
    } (ExitCode.Success)
  }

  @Test def test_3580_CLI_Unparsing_SimpleUnparse_verboseMode(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"-v unparse -s $schema --root e1") { cli =>
      cli.send("<tns:e1 xmlns:tns='http://example.com'>Hello</tns:e1>", inputDone = true)
      cli.expectErr("[info]")
    } (ExitCode.Success)

    runCLI(args"-vv unparse -s $schema --root e1") { cli =>
      cli.send("<tns:e1 xmlns:tns='http://example.com'>Hello</tns:e1>", inputDone = true)
      cli.expectErr("[debug]")
    } (ExitCode.Success)
  }

  @Test def test_3579_CLI_Unparsing_negativeTest(): Unit = {
    runCLI(args"unparse") { cli =>
      cli.send("<tns:e1 xmlns:tns='http://example.com'>Hello</tns:e1>", inputDone = true)
      cli.expectErr("There should be exactly one of the following options: schema, parser")
    } (ExitCode.Usage)
  }

  @Test def test_3578_CLI_Unparsing_SimpleUnparse_defaultRoot(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")

    runCLI(args"unparse -s $schema") { cli =>
      cli.send("<tns:e1 xmlns:tns='http://example.com'>Hello</tns:e1>", inputDone = true)
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_3583_CLI_Unparsing_SimpleUnparse_rootPath(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")

    runCLI(args"unparse -s $schema -r hcp2 -p /") { cli =>
      cli.send("<tns:hcp2 xmlns:tns='http://www.example.org/example1/'>12</tns:hcp2>", inputDone = true)
      cli.expect("12")
    } (ExitCode.Success)
  }

  // DAFFODIL-1346
  /*@Test*/ def test_3576_CLI_Unparsing_validate(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd")

    runCLI(args"unparse -s $schema -r validation_check --validate on") { cli =>
      cli.sendLine("""<ex:validation_check xmlns:ex="http://example.com">test</ex:validation_check>""", inputDone = true)
      cli.expect("[warn] Validation Error: validation_check: cvc-pattern-valid")
      cli.expect("[warn] Validation Error: validation_check failed")
    } (ExitCode.Failure)

    runCLI(args"unparse -s $schema -r validation_check --validate") { cli =>
      cli.sendLine("""<ex:validation_check xmlns:ex="http://example.com">test</ex:validation_check>""", inputDone = true)
      cli.expect("[warn] Validation Error: validation_check: cvc-pattern-valid")
      cli.expect("[warn] Validation Error: validation_check failed")
    } (ExitCode.Failure)

    runCLI(args"unparse -s $schema -r validation_check --validate limited") { cli =>
      cli.sendLine("""<ex:validation_check xmlns:ex="http://example.com">test</ex:validation_check>""", inputDone = true)
      cli.expect("[warn] Validation Error: validation_check failed")
    } (ExitCode.Failure)

    runCLI(args"unparse -s $schema -r validation_check --validate off") { cli =>
      cli.sendLine("""<ex:validation_check xmlns:ex="http://example.com">test</ex:validation_check>""", inputDone = true)
    } (ExitCode.Success)
  }

  // DAFFODIL-2709
  /*@Test*/ def test_3577_CLI_Unparsing_traceMode(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_15.dfdl.xsd")

    runCLI(args"-t unparse -s $schema") { cli =>
      cli.sendLine("<rabbitHole><nestSequence><nest>test</nest></nestSequence></rabbitHole>", inputDone = true)
      cli.expect("unparser: <Element name='nest'>")
      cli.expect("unparser: <Element name='rabbitHole'>")
      cli.expect("test")
    } (ExitCode.Success)
  }

  @Test def test_3662_CLI_Unparsing_badSchemaPath(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/doesnotexist.dfdl.xsd")

    runCLI(args"unparse -s $schema -r root") { cli =>
      cli.send("<tns:e1>Hello</tns:e1>", inputDone = true)
      cli.expectErr("Bad arguments for option 'schema'")
      cli.expectErr("Could not find file or resource")
    } (ExitCode.Usage)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_w3cdom(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse -I w3cdom -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_jdom(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse -I jdom -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_scala_xml(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse -I scala-xml -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_json(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.json")

    runCLI(args"unparse -I json -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_w3cdom_stream(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse --stream -I w3cdom -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_jdom_stream(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse --stream -I jdom -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_scala_xml_stream(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse --stream -I scala-xml -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_json_stream(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.json")

    runCLI(args"unparse --stream -I json -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_sax(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse -I sax -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_exi(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.exi")

    runCLI(args"unparse -I exi -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_exisa(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.exisa")

    runCLI(args"unparse -I exisa -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_xxxx_CLI_Unparsing_SimpleUnparse_null(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt")

    runCLI(args"unparse -I null -s $schema --root e1 $input") { cli =>
      cli.expect("Hello")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Unparsing_Stream_01(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_02.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input19.txt")

    runCLI(args"unparse --stream -s $schema $input") { cli =>
      cli.expect("123")
    } (ExitCode.Success)
  }

  @Test def test_XXX_CLI_Unparsing_Stream_sax(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_02.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input19.txt")

    runCLI(args"unparse -I sax --stream -s $schema $input") { cli =>
      cli.expect("123")
    } (ExitCode.Success)
  }

}
