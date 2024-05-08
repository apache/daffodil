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

import java.nio.file.Files.exists

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util._

import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Checks that we can run the "daffodil generate" subcommand with
 * various options and get expected outputs.
 */
class TestCLIGenerate {

  @Test def test_CLI_Generate_C(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate c -s $schema $tempDir") { _ => }(ExitCode.Success)
      assertTrue(exists(tempDir.resolve("c/libruntime/generated_code.c")))
    }
  }

  @Test def test_CLI_Generate_noLang_error(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate -s $schema $tempDir") { cli =>
        cli.expectErr("Unknown option 's'")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Generate_unknownLang_error(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate vhld -s $schema $tempDir") { cli =>
        cli.expectErr("Unknown option 's'")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Generate_noSchema_error(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate c $tempDir") { cli =>
        cli.expectErr("Required option 'schema' not found")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Generate_twoSchema_error(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate c -s $schema -s $schema $tempDir") { cli =>
        cli.expectErr("you should provide exactly one argument")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Generate_verbose(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"-v generate c -s $schema $tempDir") { cli =>
        cli.expectErr("[info] Time (compiling)")
        cli.expectErr("[info] Time (generating)")
      }(ExitCode.Success)
      assertTrue(exists(tempDir.resolve("c/libruntime/generated_code.c")))
    }
  }

  @Test def test_CLI_Generate_root(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate c -s $schema -r {http://example.com}ex_nums $tempDir") { _ => }(
        ExitCode.Success
      )
      assertTrue(exists(tempDir.resolve("c/libruntime/generated_code.c")))
    }
  }

  @Test def test_CLI_Generate_root_error(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate c -s $schema -r {ex}ex_nums $tempDir") { cli =>
        cli.expectErr("Schema Definition Error")
        cli.expectErr("No global element found for {ex}ex_nums")
      }(ExitCode.GenerateCodeError)
    }
  }

  @Test def test_CLI_Generate_namespaceNoRoot_error(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate c -s $schema -r {http://example.com} $tempDir") { cli =>
        cli.expectErr("Invalid syntax for extended QName")
      }(ExitCode.Usage)
    }
  }

  @Test def test_CLI_Generate_tunable(): Unit = {
    val schema = path(
      "daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/ex_nums.dfdl.xsd"
    )

    withTempDir { tempDir =>
      runCLI(args"generate c -s $schema -T parseUnparsePolicy=parseOnly $tempDir") { _ => }(
        ExitCode.Success
      )
      assertTrue(exists(tempDir.resolve("c/libruntime/generated_code.c")))
    }
  }
}
