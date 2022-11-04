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

package org.apache.daffodil.tdml

import org.junit.Test

import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode

class TestCLItdml {

  @Test def test_995_CLI_Tdml_Listing_negativeTest01(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")

    runCLI(args"test $tdml escape_entry1 escape_entry2-11 escape_entry1-5 escape_entry4_3") { cli =>
      cli.expect("Total: 4, Pass: 2, Fail: 0, Not Found: 2")
    } (ExitCode.Success)
  }

  @Test def test_1001_CLI_Tdml_Listing_execRegex01(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")

    runCLI(args"test --regex $tdml escape_entry4_\d") { cli =>
      cli.expect("Total: 9, Pass: 9, Fail: 0, Not Found: 0")
    } (ExitCode.Success)
  }

  @Test def test_1000_CLI_Tdml_Listing_listRegex02(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")

    runCLI(args"test -l --regex $tdml escape_entryb-\d+") { cli =>
    } (ExitCode.Success)
  }

  @Test def test_999_CLI_Tdml_Listing_listRegex01(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")

    runCLI(args"test -l --regex $tdml escape_entry4_\d+") { cli =>
      cli.expect("escape_entry4_1")
      cli.expect("escape_entry4_10")
      cli.expect("escape_entry4_20")
      cli.expect("escape_entry4_9")
    } (ExitCode.Success)
  }

  @Test def test_994_CLI_Tdml_Listing_execAll(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")

    runCLI(args"test $tdml") { cli =>
      cli.expect("Total: 88, Pass: 88, Fail: 0, Not Found: 0")
    } (ExitCode.Success)
  }

  @Test def test_993_CLI_Tdml_Listing_listAll(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml")

    runCLI(args"test -l $tdml") { cli =>
      cli.expect("byte_entities_6_01")
      cli.expect("dataDumpEncoding")
      cli.expect("emptyStringEntityTermInExpression_01")
      cli.expect("text_entities_6_04")
      cli.expect("whitespace_10")
    } (ExitCode.Success)
  }

  @Test def test_992_CLI_Tdml_Listing_singleTestList(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml")

    runCLI(args"test -l $tdml byte_entities_6_08") { cli =>
      cli.expect("byte_entities_6_08")
    } (ExitCode.Success)
  }

  @Test def test_990_CLI_Tdml_Listing_singleTest(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml")

    runCLI(args"test $tdml byte_entities_6_08") { cli =>
      cli.expect("[Pass] byte_entities_6_08")
    } (ExitCode.Success)
  }

  @Test def test_CLI_catch_TestNotCompatible(): Unit = {
    val tdml = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/testNonCompatibleImplementation.tdml")

    runCLI(args"test -iii $tdml testNotCompatibleImplementation1") { cli =>
      cli.expect("[Skipped] testNotCompatibleImplementation1 (not compatible with implementation: daffodil)")
    } (ExitCode.Success)
  }

  @Test def test_CLI_catch_TestBadArguments(): Unit = {
    val tdml = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/testNonCompatibleImplementation.tdml")

    runCLI(args"test -I notDaffodilC $tdml") { cli =>
      cli.expectErr("[error] Bad arguments for option 'implementation'")
    } (ExitCode.Usage)
  }

  @Test def test_CLI_Tdml_implementation(): Unit = {
    val tdml = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/testNonCompatibleImplementation.tdml")

    runCLI(args"test -I daffodilC $tdml testDaffodilCImplementation1") { cli =>
      cli.expect("[Pass] testDaffodilCImplementation1")
    } (ExitCode.Success)
  }

  @Test def test_1016_CLI_Tdml_Listing_listVerbose(): Unit = {
    val tdml = path("daffodil-test/src/test/resources/org/apache/daffodil/section07/assertions/assert.tdml")

    runCLI(args"test -l -i --regex $tdml assertPattern.*") { cli =>
      cli.expect("assertPatternAndExp              s2                e3         Section 7 - Assert Schema Error for Expression/Pattern - DFDL-7-047R")
    } (ExitCode.Success)
  }
}
