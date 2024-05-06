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

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util._
import org.apache.daffodil.lib.Implicits._

import net.sf.expectit.matcher.Matchers.regexp
import org.junit.Test

/**
 * Tests specific to the CLI debugger
 *
 * Note that all tests set "fork = true" because SBT creates a class loader with two versions of
 * jline, one a dependency of SBT and one a dependency of Daffodil. With these two versions on
 * the classpath, sometimes a class is found in one version of jline and sometimes in another,
 * which causes compatibility issues if the jline versions are too different (newer versions of
 * jline added an incompatibility) and leads to exceptions being thrown. By forking, we no
 * longer use SBT's classloader and avoid the issue entirely.
 *
 * Note that we only have this issue with CLI debugger tests because the CLI debugger is the
 * only thing that uses jline. If the debugger is never triggered by a test, ten the SBT class
 * loader never tries to find, load, or use any of these incompatible jline classes.
 *
 * Note that because we fork, these tests are put in daffodil-test-integration instead of
 * daffodil-cli, since this project requires the daffodil CLI to be staged and disables parallel
 * execution to reduce memory requirements.
 *
 * Additionally, because we now fork, the forked process uses normal stdin/stdout and so the
 * CLIDebuggerRunner uses jline to find the best Terminal to use, instead of using a
 * DumbTerminal like when we don't fork. But the fancy terminal that jline finds fails on
 * windows. To fix this, we also define a number of jline properties to force it to use a dumb
 * terminal for these integration tests.
 */
class TestCLIDebugger {

  val javaOpts = Seq(
    "-Dorg.jline.terminal.type=dumb",
    "-Dorg.jline.terminal.provider=dumb",
    "-Dorg.jline.terminal.dumb=true",
    "-Dorg.jline.terminal.dumb.color=false",
    "-Dfile.encoding=UTF-8"
  )

  val envs = Map(
    "DAFFODIL_JAVA_OPTS" -> javaOpts.mkString(" ")
  )

  /**
    * This shows that the way SBT creates its class loader breaks debugger tests if we do not
    * fork (note that this is the only test in this file where "fork = false" and the envs are
    * not provided). If this test ever fails, it probably means SBT fixed the jline issue and we
    * can remove "fork = true" and "envs = envs" from these tests and move them back to the
    * daffodil-test project.
    *
    * Update: newer versions of SBT have updated jline, which fixes incompatibility issues so
    * this test no longer fails (because the CLI command succeeds). However, SBT hasn't fixed
    * the underlying class loader issue, so a newer version of jline could potentially break
    * things again. The test is only commented out in case we ever want to reenable it. Also
    * keeping the other debugger tests as integration tests that fork--they don't take that much
    * longer to run when forking and saves us time if a jline update breaks things again.
    */
  // @Test
  def test_CLI_Debugger_sbt_jline_broken(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    intercept[Exception] {
      runCLI(args"-d parse -s $schema -r matrix $input", fork = false) { cli =>
        cli.expect("(debug)")
        cli.sendLine("continue")
      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Debugger_invalidExpressions(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("eval (/invalid)")
      cli.expect("error: expression evaluation failed: Schema Definition Error:")
      cli.expect("(debug)")

      cli.sendLine("eval (func())")
      cli.expect(
        "error: expression evaluation failed: Schema Definition Error: Unsupported function:"
      )
      cli.expect("(debug)")

      cli.sendLine("eval (/invalid!)")
      cli.expect("error: expression evaluation failed: Schema Definition Error:")
      cli.expect("(debug)")

      cli.sendLine("eval (!)")
      cli.expect("error: expression evaluation failed: Schema Definition Error:")
      cli.expect("(debug)")

      cli.sendLine("eval (././.\\/)")
      cli.expect("error: expression evaluation failed: Schema Definition Error:")
      cli.expect("(debug)")

      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_invalidCommandError(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("garbage")
      cli.expect("error: undefined command: garbage")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_dataAndWrapLength(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("debug")

      cli.sendLine("info data")
      cli.expect("0~,~1~,~2~,~3~,~4~,~5~,~6~")

      cli.sendLine("set dataLength -938")
      cli.sendLine("info data")
      cli.expect("0~,~1~,~2~,~3~,~4~,~5~,~6~")

      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_simpleDebugger(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_displaysTesting(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display eval (.)")
      cli.sendLine("step")
      cli.expect("matrix")

      cli.sendLine("info displays")
      cli.expect("1: eval (.)")

      cli.sendLine("disable display 1")
      cli.sendLine("info displays")
      cli.expect("1*: eval (.)")
      cli.sendLine("step")
      cli.sendLine("enable display 1")

      cli.sendLine("step")
      cli.expect("</tns:cell>")

      cli.sendLine("delete display 1")
      cli.sendLine("step")

      cli.sendLine("enable display 1")
      cli.expect("error: 1 is not a valid display id")

      cli.sendLine("continue")
      cli.expect("matrix")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_removeHidden(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input6.txt")

    runCLI(args"-d parse -s $schema -r e $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("set removeHidden false")
      cli.sendLine("display info infoset")
      cli.sendLine("step")
      cli.sendLine("step")
      // intentionally look for a newline to make sure normally hidden elements
      // are output with a trailing newline when the debugger displays them
      cli.expect("<sneaky></sneaky>\n")
      cli.sendLine("break g")
      cli.sendLine("continue")
      cli.expect("<sneaky>5</sneaky>\n")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_removeHidden2(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input6.txt")

    runCLI(args"-d parse -s $schema -r e $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("set removeHidden false")
      cli.sendLine("display info infoset")
      cli.sendLine("break g")
      cli.sendLine("continue")
      cli.expect("<sneaky>5</sneaky>")
      cli.sendLine("continue")
      val result = cli.expect("</ex:e>").getBefore
      assert(!result.contains("sneaky"))

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_breakpointTesting4(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input3.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("break cell")
      cli.sendLine("break cell")

      cli.sendLine("condition 1 dfdl:occursIndex() mod 2 eq 1")
      cli.sendLine("condition 2 dfdl:occursIndex() mod 2 eq 0")

      cli.sendLine("info breakpoints")
      cli.expect("2: cell   { dfdl:occursIndex() mod 2 eq 0 }")

      cli.sendLine("display info occursIndex")

      cli.sendLine("continue")
      cli.expect("occursIndex: 1")

      cli.sendLine("continue")
      cli.expect("occursIndex: 2")

      cli.sendLine("continue")
      cli.expect("occursIndex: 3")

      cli.sendLine("disable breakpoint 2")

      cli.sendLine("continue")
      cli.expect("occursIndex: 5")

      cli.sendLine("continue")
      cli.expect("occursIndex: 7")

      cli.sendLine("enable breakpoint 2")

      cli.sendLine("continue")
      cli.expect("occursIndex: 8")

      cli.sendLine("disable breakpoint 1")
      cli.sendLine("disable breakpoint 2")

      cli.sendLine("continue")
      cli.expect("<tns:cell>3</tns:cell>")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_breakOnValueOfElement(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input3.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("set breakOnlyOnCreation false")
      cli.expect("(debug)")

      cli.sendLine("display info infoset")
      cli.expect("(debug)")

      cli.sendLine("break cell")
      cli.expect("1: cell")
      cli.sendLine("condition 1 xsd:string(.) eq '3'")
      cli.expect("1: cell   { xsd:string(.) eq '3' }")

      cli.sendLine("info breakpoints")
      cli.expect("breakpoints:")
      cli.expect("1: cell   { xsd:string(.) eq '3' }")

      cli.sendLine("continue")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("</tns:row>")
      cli.expect("</tns:matrix>")
      cli.sendLine("continue")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("</tns:row>")
      cli.expect("</tns:matrix>")
      cli.sendLine("continue")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("</tns:row>")
      cli.expect("</tns:matrix>")

      cli.sendLine("continue")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("</tns:row>")
      cli.expect("</tns:matrix>")
      cli.sendLine("continue")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("</tns:row>")
      cli.expect("</tns:matrix>")

      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_pointsOfUncertaintyInfo(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input5.txt")

    runCLI(args"-d parse -s $schema -r Item2 $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info pointsOfUncertainty")

      cli.sendLine("step")
      cli.expect("pointsOfUncertainty:")
      cli.expect("(none)")

      cli.sendLine("step")
      cli.expect("pointsOfUncertainty:")
      cli.expect("bitPos: 0, context: choice[1]")

      cli.sendLine("step")
      cli.expect("pointsOfUncertainty:")
      cli.expect("(none)")

      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_breakpointTesting(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info infoset")
      cli.sendLine("break cell")

      cli.sendLine("continue")
      cli.expect("</tns:cell>")

      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("<tns:cell>0</tns:cell>")

      cli.sendLine("continue")
      cli.expect("</tns:cell>")

      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("<tns:cell>1</tns:cell>")

      cli.sendLine("delete breakpoint 1")
      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_breakpointTesting2(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info infoset")
      cli.sendLine("break cell")
      cli.sendLine("condition 1 dfdl:occursIndex() eq 3")

      cli.sendLine("info breakpoints")
      cli.expect("1: cell   { dfdl:occursIndex() eq 3 }")

      cli.sendLine("continue")
      cli.expect("</tns:cell>")

      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("<tns:cell>2</tns:cell>")

      cli.sendLine("continue")
      cli.expect("<tns:cell>6</tns:cell>")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_SDE_message(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info infoset")
      cli.sendLine("break cell")
      cli.sendLine(
        "condition 1 fn:count(../cell) eq 3"
      ) // ../cell is wrong. Needs to be ../tns:cell

      cli.sendLine("continue")
      cli.expect("Schema Definition Error")
      cli.expect("{}cell")
      cli.expect("tns:cell")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_breakpointTesting3(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info occursIndex")
      cli.expect("(debug)")
      cli.sendLine("break cell")
      cli.expect("(debug)")
      cli.sendLine("info breakpoints")
      cli.expect("1: cell")

      cli.sendLine("continue")
      cli.expect("occursIndex: 1")

      cli.sendLine("continue")
      cli.expect("occursIndex: 2")

      cli.sendLine("disable breakpoint 1")
      cli.sendLine("info breakpoints")
      cli.expect("1*: cell")

      cli.sendLine("info data")
      cli.expect("0~,~1~,~2~,~3~,~4~,~5~,~6~")

      cli.sendLine("continue")
      cli.expect("<tns:cell>6</tns:cell>")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_settingInfosetLines(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input3.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info infoset")
      cli.sendLine("set infosetLines 1")

      cli.sendLine("break cell")
      cli.sendLine("continue")
      cli.expect("...")
      cli.expect("</tns:matrix>")

      cli.sendLine("set infosetLines 4")
      cli.sendLine("continue")
      cli.expect("...")
      cli.expect("<tns:cell>3</tns:cell>")
      cli.expect("</tns:matrix>")

      cli.sendLine("set infosetLines 10")
      cli.sendLine("continue")
      cli.expect("<tns:matrix")

      cli.sendLine("set infosetLines -900")
      cli.sendLine("continue")
      cli.expect("<tns:matrix")
      cli.expect("</tns:matrix>")

      cli.sendLine("disable breakpoint 1")
      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_infoBitPosition(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info bitPosition")
      cli.sendLine("display info data")
      cli.sendLine("break cell")

      cli.sendLine("continue")
      cli.expect("bitPosition: 0")

      cli.sendLine("continue")
      cli.expect("bitPosition: 16")

      cli.sendLine("continue")
      cli.expect("bitPosition: 32")

      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_childIndex(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input4.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("break cell")
      cli.sendLine("display info childIndex")
      cli.sendLine("display info infoset")

      cli.sendLine("continue")
      cli.expect("childIndex: 1")

      cli.sendLine("continue")
      cli.expect("childIndex: 2")

      cli.sendLine("continue")
      cli.expect("childIndex: 4")

      cli.sendLine("disable breakpoint 1")
      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_infoPath(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("break cell")
      cli.sendLine("display info path")

      cli.sendLine("continue")
      cli.expect("matrixType::sequence[1]::row::LocalComplexTypeDef::sequence[1]::cell")

      cli.sendLine("delete breakpoint 1")
      cli.expect("debug")
      cli.sendLine("continue")

      cli.expect("""<tns:matrix xmlns:tns="http://www.example.org/example1/">""")
      cli.expect("<tns:cell>2</tns:cell>")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_dataAndWrapLength2(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("break cell")
      cli.sendLine("continue")
      cli.sendLine("info data")
      cli.expect("0~,~1~,~2~,~3~,~4~,~5~,~6~")

      cli.sendLine("set dataLength -938")
      cli.sendLine("info data")
      cli.expect("0~,~1~,~2~,~3~,~4~,~5~,~6~")

      cli.sendLine("disable breakpoint 1")
      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_groupIndex01(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema_03.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input9.txt")

    runCLI(args"-d parse -r list -s $schema $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info groupIndex")
      cli.sendLine("break price")
      cli.expect("1: price")
      cli.sendLine("break comment")
      cli.expect("2: comment")

      cli.sendLine("continue")
      cli.expect("groupIndex: 2")
      cli.sendLine("continue")
      cli.expect("groupIndex: 4")
      cli.sendLine("continue")
      cli.expect("groupIndex: 2")
      cli.sendLine("continue")
      cli.expect("groupIndex: 4")
      cli.sendLine("continue")
      cli.expect("<ex:price>89.99</ex:price>")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_validation1(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema_03.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input9.txt")

    runCLI(args"-d parse -r list -s $schema $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info dne1")
      cli.expect("error: undefined info command: dne1")
      cli.sendLine("display info bitLimit dne2")
      cli.expect("error: bitLimit command requires zero arguments")
      cli.sendLine("display break")
      cli.expect("error: undefined command: break")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_infodata(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display info data")
      cli.sendLine("step")
      cli.expect("│") //  (0 to 0)
      cli.expect("0~,~1~,~2~,~3~,~4~,~5~,~6~")

      cli.sendLine("break cell")
      cli.sendLine("condition 1 dfdl:occursIndex() eq 5")
      cli.sendLine("continue")

      cli.expect("""                                  │                                    │""")
      cli.expect(
        """    87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~"""
      )
      cli.expect(
        """    00000000: 302c 312c 322c 332c 342c 352c 36         0~,~1~,~2~,~3~,~4~,~5~,~6~      """
      )
      cli.sendLine("continue")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_undefined_command(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("display data")
      cli.expect("error: undefined command: data")

      cli.sendLine("set breakonfailure true")
      cli.expect("error: undefined command: breakonfailure")

      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_delimiterStack(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("break row")
      cli.expect("(debug)")

      cli.sendLine("continue")
      cli.expect("(debug)")

      cli.sendLine("info delimiterStack")
      cli.expect("local:  %NL; (separator)")
      cli.expect("(debug)")

      cli.sendLine("break cell")
      cli.expect("(debug)")

      cli.sendLine("continue")
      cli.expect("(debug)")

      cli.sendLine("info delimiterStack")
      cli.expect("remote: %NL; (separator)")
      cli.expect("local:  , (separator)")

      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_utf16_encoding(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/utf16schema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/hextest.txt"
    )

    runCLI(args"-d parse -s $schema -r e2 $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("info data")
      cli.expect("\u240A")

      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_info_infoset(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("info infoset")
      cli.expect("No Infoset")

      cli.sendLine("step")
      cli.sendLine("info infoset")
      cli.expect("matrix")

      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_InfoHidden_1(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section14/sequence_groups/SequencesWithHiddenRefs.dfdl.xsd"
    )

    withTempFile { input =>
      Files.write(input, "2~3".getBytes(UTF_8))

      runCLI(args"-d parse -s $schema -r e5 $input", fork = true, envs = envs) { cli =>
        cli.expect("(debug)")

        cli.sendLine("break f")
        cli.sendLine("display info hidden")

        cli.sendLine("continue")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect("<f xmlns=\"\">2</f>")

      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Debugger_InfoHidden_2(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section14/sequence_groups/SequencesWithHiddenRefs.dfdl.xsd"
    )

    withTempFile { input =>
      Files.write(input, "2~3".getBytes(UTF_8))

      runCLI(args"-d parse -s $schema -r e4 $input", fork = true, envs = envs) { cli =>
        cli.expect("(debug)")

        cli.sendLine("break f")
        cli.sendLine("display info hidden")

        cli.sendLine("continue")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect("<f xmlns=\"\">3</f>")

      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Debugger_InfoHidden_3(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section15/choice_groups/ChoicesInHiddenContexts.dfdl.xsd"
    )

    withTempFile { input =>
      Files.write(input, "2,3".getBytes(UTF_8))

      runCLI(args"-d parse -s $schema -r e8 $input", fork = true, envs = envs) { cli =>
        cli.expect("(debug)")

        cli.sendLine("break a")
        cli.sendLine("break h")
        cli.sendLine("break g")
        cli.sendLine("break e")
        cli.sendLine("break f")
        cli.sendLine("display info hidden")

        cli.sendLine("continue")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect("<a>2</a>")
        cli.expect("<g></g>")
      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Debugger_InfoHidden_4(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section15/choice_groups/ChoicesInHiddenContexts.dfdl.xsd"
    )

    withTempFile { input =>
      Files.write(input, "[6~]9".getBytes(UTF_8))

      runCLI(args"-d parse -s $schema -r e9 $input", fork = true, envs = envs) { cli =>
        cli.expect("(debug)")

        cli.sendLine("break e")
        cli.sendLine("break f")
        cli.sendLine("break g")
        cli.sendLine("break h")
        cli.sendLine("break i")
        cli.sendLine("display info path hidden")

        cli.sendLine("continue")
        cli.expect(":f")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect(":i")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect(":h")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect(":e")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect(":f")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect(":f")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect(":g")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect(":i")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect(":h")
        cli.expect("hidden: false")

        cli.sendLine("continue")
        cli.expect(":e")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect(":f")
        cli.expect("hidden: true")

        cli.sendLine("continue")
        cli.expect("<h></h>")
      }(ExitCode.Success)
    }
  }

  @Test def test_CLI_Debugger_simpleDebugger_unparse(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input12.txt"
    )

    runCLI(args"-d unparse -s $schema -r e1 $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("break e1")
      cli.expect("1: e1")
      cli.sendLine("continue")
      cli.expect("Hello  breakpoint 1: e1")
      cli.sendLine("info data")
      cli.expect("4865 6c6c 6f                             Hello")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_prefixLength(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/prefixed_length.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/prefix.txt")

    runCLI(args"-d parse -s $schema $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("display info infoset")
      cli.expect("(debug)")
      cli.sendLine("display eval .")
      cli.sendLine("step")
      cli.expect("<field></field>")
      cli.sendLine("step")
      cli.expect("<field (prefixLength)></field (prefixLength)>")
      cli.sendLine("step")
      cli.expect("<field (prefixLength)>4</field (prefixLength)>")
      cli.sendLine("step")
      cli.expect("<field>abcd</field>")
      cli.sendLine("complete")
      cli.expect("<field>abcd</field>")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_info_variables(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("info variables byteOrder")
      cli.expect("byteOrder: bigEndian (default)")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_info_data_text(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("display info data text")
      cli.expect("(debug)")
      cli.sendLine("step")
      cli.expect("0~,~1~,~2~")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_info_data_binary(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("display info data binary")
      cli.expect("(debug)")
      cli.sendLine("step")
      cli.expect("302c 312c 32")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_info_diff_01(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section07/variables/variables_01.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r c $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("display info diff")
      cli.expect("(debug)")
      cli.sendLine("step")
      cli.expect("(no differences)")
      cli.sendLine("step")
      cli.expect("(no differences)")
      cli.sendLine("step")
      cli.expect("variable: tns:v_with_default: 42 (default) -> 42 (read)")
      cli.sendLine("step")
      cli.expect("variable: tns:v_no_default: (undefined) -> 42 (set)")
      cli.sendLine("step")
      cli.expect("childIndex: 1 -> 2")
      cli.sendLine("step")
      cli.expect("variable: tns:v_no_default: 42 (set) -> 42 (read)")
      cli.sendLine("step")
      cli.expect("<d>42</d>")
      cli.expect("<e>42</e>")
    }(ExitCode.LeftOverData)
  }

  @Test def test_CLI_Debugger_info_diff_02(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"-d parse -s $schema -r matrix $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("display info diff")
      cli.expect("(debug)")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("bitPosition: 0 -> 8")
      cli.expect("foundDelimiter: (no value) -> ,")
      cli.expect("foundField: (no value) -> 0")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("bitPosition: 8 -> 16")
      cli.expect("childIndex: 1 -> 2")
      cli.expect("foundDelimiter: , -> (no value)")
      cli.expect("foundField: 0 -> (no value)")
      cli.expect("groupIndex: 1 -> 2")
      cli.expect("occursIndex: 1 -> 2")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("bitPosition: 16 -> 24")
      cli.expect("foundDelimiter: (no value) -> ,")
      cli.expect("foundField: (no value) -> 1")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_info_diff_03(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input6.txt")

    runCLI(args"-d parse -s $schema -r e $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("display info diff")
      cli.expect("(debug)")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("hidden: false -> true")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.sendLine("step")
      cli.expect("hidden: true -> false")
      cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_info_diff_04(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt.xml"
    )

    runCLI(args"-d unparse -s $schema -r matrix -o $devNull $input", fork = true, envs = envs) {
      cli =>
        cli.expect("(debug)")
        cli.sendLine("display info diff")
        cli.expect("(debug)")
        cli.sendLine("set diffExcludes childIndex")
        cli.expect("(debug)")
        cli.sendLine("step")
        cli.expect("bitPosition: 0 -> 8")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.expect(regexp("\\+ Suppressable.* for cell"))
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.expect(regexp("RegionSplit.* for cell"))
        cli.sendLine("info suspensions")
        cli.expect(regexp("Suppressable.* for cell"))
        cli.expect(regexp("RegionSplit.* for cell"))
        cli.sendLine("quit")
    }(ExitCode.Failure)
  }

  @Test def test_CLI_Debugger_info_diff_05(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema_03.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input9.txt.xml"
    )

    runCLI(args"-d unparse -r list -s $schema -o $devNull $input", fork = true, envs = envs) {
      cli =>
        cli.expect("(debug)")
        cli.sendLine("set diffExcludes doesNotExist1 bitLimit doesNotExist2")
        cli.expect("unknown or undiffable info commands: doesNotExist1, doesNotExist2")

        cli.sendLine("display info diff")
        cli.sendLine("break Item")
        cli.sendLine("continue")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.expect(regexp("\\+ SuppressableSeparator.* ex:Item"))
        cli.sendLine("continue")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.sendLine("step")
        cli.expect(regexp("\\+ RegionSplit.* ex:Item"))
        cli.sendLine("step")
        cli.sendLine("step")
        cli.expect(regexp("\\- RegionSplit.* ex:Item"))
        cli.sendLine("info suspensions")
        cli.expect(regexp("SuppressableSeparator.* ex:Item"))
        cli.sendLine("step")
        cli.sendLine("step")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_parse_unparser_not_available(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input3.txt")

    runCLI(args"-d parse -s $schema $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("info parser")
      cli.expect(
        "parser: <Element name='matrix'><DelimiterStackParser>...</DelimiterStackParser></Element>"
      )

      cli.sendLine("info unparser")
      cli.expect("unparser: not available")

      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Debugger_unparse_parser_not_available(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt.xml"
    )

    runCLI(args"-d unparse -s $schema $input", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")

      cli.sendLine("info unparser")
      cli.expect("unparser: <ConvertTextNumberUnparser/>")

      cli.sendLine("info parser")
      cli.expect("parser: not available")

      cli.sendLine("continue")

    }(ExitCode.Success)
  }

  @Test def test_CLI_Tdml_Debug_singleTest(): Unit = {
    val tdml = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml"
    )

    runCLI(args"-d test $tdml byte_entities_6_08", fork = true, envs = envs) { cli =>
      cli.expect("(debug)")
      cli.sendLine("continue")
      cli.expect("[Pass] byte_entities_6_08")
    }(ExitCode.Success)
  }

}
