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

class TestCLIPerformance {

  @Test def test_CLI_Performance_2_Threads_2_Times(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"performance -N 2 -t 2 -s $schema -r matrix $input") { cli =>
      cli.expect("total parse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_2_Threads_2_Times_sax(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"performance -I sax -N 2 -t 2 -s $schema -r matrix $input") { cli =>
      cli.expect("total parse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_2_Threads_2_Times_exi(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"performance -I exi -N 2 -t 2 -s $schema -r matrix $input") { cli =>
      cli.expect("total parse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_2_Threads_2_Times_exisa(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"performance -I exisa -N 2 -t 2 -s $schema -r matrix $input") { cli =>
      cli.expect("total parse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_3_Threads_20_Times(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt")

    runCLI(args"performance -N 20 -t 3 -s $schema -r matrix $input") { cli =>
      cli.expect("total parse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_5_Threads_50_Times(): Unit = {
    val schema = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input5.txt")

    runCLI(args"performance -N 50 -t 5 -s $schema -r Item2 $input") { cli =>
      cli.expect("total parse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_2_Threads_2_Times_Negative(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input5.txt")

    runCLI(args"performance -N 2 -t 2 -s $schema $input") { cli =>
      cli.expect("total parse time (sec):")
      cli.expect("avg rate (files/sec):")
      cli.expectErr("error")
    }(ExitCode.PerformanceTestError)
  }

  @Test def test_CLI_Performance_Unparse_2_Threads_2_Times(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.txt"
    )

    runCLI(args"performance --unparse -N 2 -t 2 -s $schema -r e3 $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_Unparse_2_Threads_2_Times_sax(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.txt"
    )

    runCLI(args"performance --unparse -I sax -N 2 -t 2 -s $schema -r e3 $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_Unparse_2_Threads_2_Times_exi(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.exi"
    )

    runCLI(args"performance --unparse -I exi -N 2 -t 2 -s $schema -r e3 $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_Unparse_2_Threads_2_Times_exisa(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.exisa"
    )

    runCLI(args"performance --unparse -I exisa -N 2 -t 2 -s $schema -r e3 $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_Unparse_2_Threads_2_Times_null(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.txt"
    )

    runCLI(args"performance --unparse -I null -N 2 -t 2 -s $schema -r e3 $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_Unparse_3_Threads_20_Times(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.txt"
    )

    runCLI(args"performance --unparse -N 20 -t 3 -s $schema -r e3 $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_Unparse_5_Threads_50_Times(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.txt"
    )

    runCLI(args"performance --unparse -N 50 -t 5 -s $schema -r e3 $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Performance_Unparse_2_Threads_2_Times_Negative(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    val input = path(
      "daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input16.txt"
    )

    runCLI(args"performance --unparse -N 2 -t 2 -s $schema $input") { cli =>
      cli.expect("total unparse time (sec):")
      cli.expect("avg rate (files/sec):")
      cli.expectErr("error")
    }(ExitCode.PerformanceTestError)
  }
}
