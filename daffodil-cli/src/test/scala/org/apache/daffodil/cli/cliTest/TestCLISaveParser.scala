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

package org.apache.daffodil.cli.cliTest

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util._

import org.junit.Test

class TestCLISaveParser {

  @Test def test_CLI_Saving_SaveParser_simple(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r matrix $parser") { _ => }(ExitCode.Success)

      runCLI(args"parse --parser $parser") { cli =>
        cli.sendLine("0,1,2", inputDone = true)
        cli.expect("<tns:cell>2</tns:cell>")
      }(ExitCode.LeftOverData)
    }
  }

  @Test def test_CLI_Saving_SaveParser_stdout(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd"
    )

    runCLI(args"-v save-parser -s $schema") { cli =>
      cli.expectErr("[info] Time")
      cli.expect("DAFFODIL") // check for magic number
    }(ExitCode.Success)
  }

  @Test def test_CLI_Saving_SaveParser_withConfig(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd"
    )
    val config = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/daffodil_config_cli_test.xml"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r row2 -c $config $parser") { _ => }(
        ExitCode.Success
      )

      runCLI(args"parse --parser $parser -c $config") { cli =>
        cli.sendLine("0,1,2", inputDone = true)
        cli.expect("<cell>-9</cell>")
        cli.expect("<cell>-2</cell>")
        cli.expect("<cell>-8</cell>")
      }(ExitCode.LeftOverData)
    }
  }

  @Test def test_CLI_Saving_SaveParser_namespaceUsed(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input8.txt")

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r {target}matrix $parser") { _ => }(
        ExitCode.Success
      )

      runCLI(args"parse --parser $parser $input") { cli =>
        cli.expect("<cell>14</cell>")
      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Saving_SaveParser_path(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r matrix -p / $parser") { _ => }(ExitCode.Success)

      runCLI(args"parse --parser $parser") { cli =>
        cli.sendLine("0,1,2", inputDone = true)
        cli.expect("<tns:cell>2</tns:cell>")
      }(ExitCode.LeftOverData)
    }
  }

  @Test def test_CLI_Saving_SaveParser_MultSchema(): Unit = {
    val schema1 = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section07/defineFormat/defineFormat.dfdl.xsd"
    )
    val schema2 = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema1 -s $schema2 $parser") { cli =>
        cli.expectErr("Bad arguments for option 'schema'")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Saving_SaveParser_verboseMode(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"-v save-parser -s $schema -r matrix $parser") { cli =>
        cli.expectErr("[info]")
      }(ExitCode.Success)

      runCLI(args"-vv save-parser -s $schema -r matrix $parser") { cli =>
        cli.expectErr("[debug]")
      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Saving_SaveParser_namespaceNoRoot(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r {http://www.example.org/example1/} $parser") {
        cli =>
          cli.expectErr("Bad arguments for option 'root'")
          cli.expectErr("{http://www.example.org/example1/}")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Saving_SaveParser_emptyNamespace(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r {}matrix -p / $parser") { _ => }(ExitCode.Success)

      runCLI(args"parse --parser $parser") { cli =>
        cli.sendLine("0,1,2", inputDone = true)
        cli.expect("<tns:cell>2</tns:cell>")
      }(ExitCode.LeftOverData)
    }
  }

  @Test def test_CLI_DFDL_1205_FullValidation_SavedParser_Incompatible(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input8.txt")

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r {target}matrix $parser") { _ => }(
        ExitCode.Success
      )

      runCLI(args"parse --parser $parser --validate on $input") { cli =>
        cli.expectErr("[error]")
        cli.expectErr(
          "The validation mode must be 'limited' or 'off' when using a saved parser."
        )
      }(ExitCode.Usage)
    }
  }

  /**
   * Note that in Daffodil 2.6.0 behavior of external variables changed. They are not saved as part of binary
   * compiling. They are a runtime-thing only.
   */
  @Test def test_CLI_Saving_SaveParser_extVars(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section07/external_variables/external_variables.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r row2 $parser") { _ => }(ExitCode.Success)

      runCLI(
        args"parse --parser $parser -D{http://example.com}var1=25 {http://example.com}var3=7"
      ) { cli =>
        cli.sendLine("0", inputDone = true)
        cli.expect("<tns:row2 xmlns:tns=\"http://example.com\">")
        cli.expect("<cell>25</cell>")
        cli.expect("<cell>7</cell>")
      }(ExitCode.LeftOverData)
    }
  }

  @Test def test_CLI_Saving_SaveParser_validate(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser --validate on -s $schema -r validation_check $parser") { cli =>
        cli.expectErr("Unknown option 'validate'")
      }(ExitCode.Usage)

      runCLI(args"save-parser -s $schema -r validation_check $parser") { _ => }(
        ExitCode.Success
      )

      runCLI(args"parse --validate limited -P $parser") { cli =>
        cli.send("test", inputDone = true)
        cli.expectErr("[error] Validation Error")
        cli.expectErr("ex:validation_check failed")
        cli.expectErr("[0-8]+")
      }(ExitCode.ParseError)

      runCLI(args"parse --validate on -P $parser") { cli =>
        cli.send("test", inputDone = true)
        cli.expectErr("validation mode must be 'limited' or 'off' when using a saved parser.")
      }(ExitCode.Usage)
    }
  }

  @Test
  def test_CLI_Saving_SaveParser_debug(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -d -s $schema -r matrix $parser") { cli =>
        cli.expectErr("Unknown option 'd'")
      }(ExitCode.Usage)
    }
  }

  @Test
  def test_CLI_Saving_SaveParser_trace(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -t -s $schema -r matrix $parser") { cli =>
        cli.expectErr("Unknown option 't'")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Saving_SaveParser_unparse(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt.xml"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r matrix $parser") { _ => }(ExitCode.Success)

      runCLI(args"unparse --parser $parser $input") { cli =>
        cli.expect("0,1,2")
      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Saving_SaveParser_unparse2(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input12.txt"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r e1 $parser") { _ => }(ExitCode.Success)

      runCLI(args"unparse --parser $parser $input") { cli =>
        cli.expect("Hello")
      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Saving_SaveParser_tunables(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input12.txt"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema -r e1 -T parseUnparsePolicy=parseOnly $parser") { _ =>
      }(ExitCode.Success)

      runCLI(args"unparse --parser $parser $input") { cli =>
        cli.expectErr("[error]")
        cli.expectErr(
          "Runtime Schema Definition Error: This schema was compiled without unparse support."
        )
      }(ExitCode.UnparseError)
    }
  }

  /**
   * Attempted to save-parser an invalid schema so we can check the diagnostic error for leaked information
   */
  @Test def test_CLI_Saving_SaveParser_error(): Unit = {
    val schema = path(
      "daffodil-runtime1/src/test/resources/test/api/mySchema6.dfdl.xsd"
    )

    withTempFile { parser =>
      runCLI(args"save-parser -s $schema $parser") { cli =>
        cli.expectErr("[error]")
        cli.expectErr(s"Schema context: Location line 32 column 74 in ${schema.normalize()}")
      }(ExitCode.UnableToCreateProcessor)
    }
  }

}
