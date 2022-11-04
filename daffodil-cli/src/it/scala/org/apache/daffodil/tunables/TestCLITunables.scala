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

import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode

class TestCLITunables {

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_01(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_01 -TunqualifiedPathStepPolicy=noNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("""<c xmlns="">2</c>""")
      cli.expect("""<s xmlns="">1</s>""")
      cli.expect("</test_01>")
    } (ExitCode.Success)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_02(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_02 -TunqualifiedPathStepPolicy=noNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expectErr("Schema Definition Error")
    } (ExitCode.UnableToCreateProcessor)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_03(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_03 -TunqualifiedPathStepPolicy=noNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expectErr("Schema Definition Error")
    } (ExitCode.UnableToCreateProcessor)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_04(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_04 -TunqualifiedPathStepPolicy=noNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("""<c xmlns="">2</c>""")
      cli.expect("""<s xmlns="">2</s>""")
      cli.expect("</test_04>")
    } (ExitCode.Success)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_01(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_01 -TunqualifiedPathStepPolicy=defaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("""<c xmlns="">2</c>""")
      cli.expect("""<s xmlns="">1</s>""")
      cli.expect("</test_01>")
    } (ExitCode.Success)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_02(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_02 -TunqualifiedPathStepPolicy=defaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("""<c xmlns="">2</c>""")
      cli.expect("""<s xmlns="">1</s>""")
      cli.expect("</test_02>")
    } (ExitCode.Success)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_03(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_03 -TunqualifiedPathStepPolicy=defaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expectErr("Schema Definition Error")
    } (ExitCode.UnableToCreateProcessor)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_04(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_04 -TunqualifiedPathStepPolicy=defaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expectErr("Schema Definition Error")
    } (ExitCode.UnableToCreateProcessor)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_01(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_01 -TunqualifiedPathStepPolicy=preferDefaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("""<c xmlns="">2</c>""")
      cli.expect("""<s xmlns="">1</s>""")
      cli.expect("</test_01>")
    } (ExitCode.Success)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_02(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_02 -TunqualifiedPathStepPolicy=preferDefaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("""<c xmlns="">2</c>""")
      cli.expect("""<s xmlns="">1</s>""")
      cli.expect("</test_02>")
    } (ExitCode.Success)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_03(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_03 -TunqualifiedPathStepPolicy=preferDefaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expectErr("Schema Definition Error")
    } (ExitCode.UnableToCreateProcessor)
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_04(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")

    runCLI(args"parse -s $schema -r test_04 -TunqualifiedPathStepPolicy=preferDefaultNamespace") { cli =>
      cli.send("12", inputDone = true)
      cli.expect("""<c xmlns="">2</c>""")
      cli.expect("""<s xmlns="">2</s>""")
      cli.expect("</test_04>")
    } (ExitCode.Success)
  }

  /**
   * Suppresses SDW messages.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings1(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")

    runCLI(args"parse -s $schema -TsuppressSchemaDefinitionWarnings=all") { cli =>
      cli.send("a,b", inputDone = true)
      cli.expect("<s1>a</s1>")
      cli.expect("<s2>b</s2>")
    } (ExitCode.Success)
  }

  /**
   * Will display SDW warnings. Does not set the tunable that suppresses them.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings2(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")

    runCLI(args"parse -s $schema") { cli =>
      cli.send("a,b", inputDone = true)
      cli.expect("<s1>a</s1>")
      cli.expect("<s2>b</s2>")
      cli.expectErr("Schema Definition Warning")
      cli.expectErr("dfdl:lengthKind")
      cli.expectErr("delimited")
      cli.expectErr("dfdl:length")
    } (ExitCode.Success)
  }

  /**
   * Saving the processor should compile and issue SDWs which we should see in
   * the expected output. When reloading, we should NOT see a SDW because that
   * isn't displayed on a reload of a compiled processor
   */
  @Test def test_CLI_Parsing_ReloadingDoesNotRepeatWarnings(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema $parser") { cli =>
        cli.expectErr("Schema Definition Warning")
      } (ExitCode.Success)

      runCLI(args"parse -P $parser") { cli =>
        cli.send("a,b", inputDone = true)
        cli.expect("<s1>a</s1>")
        cli.expect("<s2>b</s2>")
      } (ExitCode.Success)
    }
  }

}
